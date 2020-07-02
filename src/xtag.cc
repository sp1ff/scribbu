/**
 * \file xtag.cc
 *
 * Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#include "config.h"
#include "command-utilities.hh"

#include <queue>
#include <string>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>

#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/tagset.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"(scribbu xtag -- manage the tag cloud frame

scribbu xtag [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, create or update the "tag cloud" frame in all tags in the files
named on the command line. If an argument is a directory, create or update the
tag cloud in all tags in all files in the directory tree rooted at that
argument.

The tag cloud is an experimental ID3v2 frame (with frame identifier "XTAG")
introduced by `scribbu'. It is meant to represent a collection of arbitrary tags
attached to the track, with or without values. On the command-line, this
sub-command expresses it as urlencoded query parameters, e.g.:

    scribbu xtag --tags=sub-genres=foo,bar&90s...

will update the tag cloud for any XTAG frames found.

This operation can be scoped in a few ways:

    scribbu xtag -t=1 --tags=sub-genres=foo,bar&90s...

will only operate on the XTAG frame in the second tag in each file (the
option can be given more than once to specify multiple indicies).

    scribbu xtag -c --tags=sub-genres=foo,bar&90s...

will create an ID3v2.3 tag and add an XTAG frame if no ID3v2 tag exists, but
only if the file already has an ID3v1 tag (any information therein will be
copied to the new ID3v2 tag). The `-C' flag will always create an ID3v2.3 tag,
whether there's an ID3v1 tag present or not. This should be used with care when
operating on directories, or you may find assorted, non-music files have had
ID3v2 tags prepended to them.

    scribbu xtag --tags=sub-genres=foo,bar&90s --merge

will "merge" the tags instead of overwriting.

For detailed help, say `scribbu xtag --help'. To see the manual, say
`info "scribbu (xtag) "'.
)");

////////////////////////////////////////////////////////////////////////////////
//                       functor for processing tagsets                       //
////////////////////////////////////////////////////////////////////////////////

/**
 * \brief tagset_processor for creating/updating XTAG frames
 *
 * \sa process_dirent_args
 * \sa tagset_processor
 *
 *
 * Since a lot of the logic of processing one or more "file-or-directory"
 * sub-command arguments, reading their tags & making adjustments is
 * boilerplate, I've factored it out into the process_dirent_args template free
 * function & the tagset_processor functor. The logic specific to the `xtag'
 * sub-command resides here.
 *
 *
 */

class set_xtag: public tagset_processor
{
public:
  /// Ctor for processing all ID3v2 tags
  set_xtag(const std::string &tag_cloud,
           const std::string &owner,
           bool               create_xtag,
           bool               merge_xtag,
           v2_creation_policy v2cp,
           bool               create_backups,
           bool               dry_run,
           bool               adj_unsync):
    tagset_processor(v2_simple_tag_scope_policy::all,
                     v1_tag_scope_policy::no,
                     v2cp,
                     v1_creation_policy::never,
                     dry_run, false, adj_unsync,
                     create_backups),
    tag_cloud_(tag_cloud),
    owner_(owner),
    create_(create_xtag),
    merge_(merge_xtag)
  { }
  /// Ctor for processing only some ID3v2 tags
  template <typename FII> // Forward Input Iterator => size_t (tag index)
  set_xtag(FII                p0,
           FII                p1,
           const std::string &tag_cloud,
           const std::string &owner,
           bool               create_xtag,
           bool               merge_xtag,
           v2_creation_policy v2cp,
           bool               create_backups,
           bool               dry_run,
           bool               adj_unsync):
    tagset_processor(p0, p1,
                     v1_tag_scope_policy::no,
                     v2cp,
                     v1_creation_policy::never,
                     dry_run, false, adj_unsync,
                     create_backups),
    tag_cloud_(tag_cloud),
    owner_(owner),
    create_(create_xtag),
    merge_(merge_xtag)
  { }

  //////////////////////////////////////////////////////////////////////////////
  //                 tagset_processor interface for sub-classes               //
  //////////////////////////////////////////////////////////////////////////////

public:
  /// Create a new ID3v1 tag when there are ID3v2 tags present
  virtual
  std::unique_ptr<scribbu::id3v1_tag>
  create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &v2);
  /// Create a new ID3v1 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v1_tag> create_v1();
  /// Create a new ID3v2 tag when there's an ID3v1 tag present
  virtual std::unique_ptr<scribbu::id3v2_tag>
  create_v2(const scribbu::id3v1_tag &v1);
  /// Create a new ID3v2 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v2_tag> create_v2();
  /// Process the ID3v1 tag
  virtual bool process_v1(scribbu::id3v1_tag &v1);
  /// Process an ID3v2.2 tag
  virtual bool process_v2(scribbu::id3v2_2_tag &v2);
  /// Process an ID3v2.3 tag
  virtual bool process_v2(scribbu::id3v2_3_tag &v2);
  /// Process an ID3v2.4 tag
  virtual bool process_v2(scribbu::id3v2_4_tag &v2);

private:

  template <typename tag_type>
  struct tag_traits
  { };

  /**
   * \brief template member function for creating or updating the XTAG frame, in
   * terms if id3v2_tag sub-classes
   *
   *
   * \param tag [in,out] reference to an ID3v2 tag upon which to operate; an
   * XTAG frame will be added to the tag or merged with an extant frame
   * according to the program options
   *
   * \return true if the tag was modified in any way, false else (NB this value
   * ignores the "dry-run" flag; i.e. if `--dry-run' was given, and the tag
   * *would* have been modified, this method will still return true
   *
   *
   * At the time of this writing, class id3v2_tag doesn't offer a virtual
   * function for handling tag cloud frames so I need to work in terms of the
   * concrete sub-class (i.e. class id3v2_2_tag, id3v2_3_tag, or
   * id3v2_4_tag). Since the logic is the same in each case, I've written this
   * template, and factored out type-specific information into a traits class
   * (tag_traits).
   *
   * I suppose providing xtag-related virtuals on class id3v2_tag would be a
   * more elegant solution, but the XTAG frame is, well, experimental & I'm not
   * quite ready to do that, yet.
   *
   * \todo Consider adding xtag-related virtuals to class id3v2_tag &
   * cleaning-up the xtag sub-command implementation considerably.
   *
   *
   */

  template <typename tag_type>
  bool
  process_tag(tag_type &tag)
  {
    using namespace std;

    typedef tag_traits<tag_type> traits_type;
    typedef typename traits_type::frame_type frame_type;
    typedef typename traits_type::xtag_type  xtag_type;

    size_t num_xtag = 0;
    for (frame_type &F: tag) {

      if (!traits_type::is_xtag(F)) {
        continue;
      }

      xtag_type &G = dynamic_cast<xtag_type&>(F);
      std::string email = G.owner();
      if (!owner_.empty() && owner_ != email) {
        continue;
      }

      if (merge_) {
        if (dry_run()) {
          cout << "Merging " << tag_cloud_ << " into XTAG (" << email <<
            ")" << endl;
        } else {
          G.merge(tag_cloud_);
        }
      } else {
        if (dry_run()) {
          cout << "Setting " << tag_cloud_ << " into XTAG (" << email <<
            ")" << endl;
        } else {
          G.update(tag_cloud_);
        }
      }
        num_xtag++;
    } // End iteration over `tag''s frames.

    bool upd = num_xtag != 0;
    if (!num_xtag && create_) {
      if (dry_run()) {
        cout << "creating a new XTAG frame for " << owner_ << ": " <<
          tag_cloud_ << endl;
      } else {
        tag.push_back(traits_type::make_xtag(owner_, tag_cloud_));
      }
      upd = true;
    }

    return upd;
  }

private:
  std::string tag_cloud_;
  std::string owner_;
  bool create_;
  bool merge_;
};

template <>
struct set_xtag::tag_traits<scribbu::id3v2_2_tag>
{
  typedef scribbu::id3v2_2_tag   tag_type;
  typedef scribbu::id3v2_2_frame frame_type;
  typedef scribbu::XTG           xtag_type;

  static bool is_xtag(const frame_type &F) {
    static const scribbu::frame_id3 XTAG("XTG");
    return F.id() == XTAG;
  }

  static xtag_type make_xtag(const std::string &email,
                             const std::string &tags) {
    return xtag_type(email, tags);
  }
};

template <>
struct set_xtag::tag_traits<scribbu::id3v2_3_tag>
{
  typedef scribbu::id3v2_3_tag   tag_type;
  typedef scribbu::id3v2_3_frame frame_type;
  typedef scribbu::XTAG          xtag_type;

  static bool is_xtag(const frame_type &F) {
    static const scribbu::frame_id4 XTAG("XTAG");
    return F.id() == XTAG;
  }

  static xtag_type make_xtag(const std::string &email,
                             const std::string &tags) {
    using namespace scribbu;
    return xtag_type(email, tags,
                     id3v2_3_plus_frame::tag_alter_preservation::preserve,
                     id3v2_3_plus_frame::file_alter_preservation::preserve,
                     id3v2_3_plus_frame::read_only::clear, boost::none,
                     boost::none, boost::none);
  }
};

template <>
struct set_xtag::tag_traits<scribbu::id3v2_4_tag>
{
  typedef scribbu::id3v2_4_tag   tag_type;
  typedef scribbu::id3v2_4_frame frame_type;
  typedef scribbu::XTAG_2_4      xtag_type;

  static bool is_xtag(const frame_type &F) {
    static const scribbu::frame_id4 XTAG("XTAG");
    return F.id() == XTAG;
  }

  static xtag_type make_xtag(const std::string &email,
                             const std::string &tags) {
    using namespace scribbu;
    return xtag_type(email, tags,
                     id3v2_3_plus_frame::tag_alter_preservation::preserve,
                     id3v2_3_plus_frame::file_alter_preservation::preserve,
                     id3v2_3_plus_frame::read_only::clear, boost::none,
                     boost::none, false, false, boost::none);
  }
};

/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_xtag::create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &/*v2*/)
{
  throw bad_operation(unknown_op::create_v1_from_v2);
}

/// Create a new ID3v1 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_xtag::create_v1()
{
  throw bad_operation(unknown_op::create_v1);
}

/// Create a new ID3v2 tag when there's an ID3v1 tag present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_xtag::create_v2(const scribbu::id3v1_tag &v1)
{
  auto p = copy_id3_v1(v1);
  process_tag(*p);
  return p;
}

/// Create a new ID3v2 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_xtag::create_v2()
{
  auto p = std::make_unique<scribbu::id3v2_3_tag>(DEFAULT_PADDING);
  process_tag(*p);
  return p;
}

/// Process the ID3v1 tag
/*virtual*/ bool
set_xtag::process_v1(scribbu::id3v1_tag &v1)
{
  throw bad_operation(unknown_op::process_v1);
}

/// Process an ID3v2.2 tag
/*virtual*/ bool
set_xtag::process_v2(scribbu::id3v2_2_tag &v2)
{
  return process_tag(v2);
}

/// Process an ID3v2.3 tag
/*virtual*/ bool
set_xtag::process_v2(scribbu::id3v2_3_tag &v2)
{
  return process_tag(v2);
}

/// Process an ID3v2.4 tag
/*virtual*/ bool
set_xtag::process_v2(scribbu::id3v2_4_tag &v2)
{
  return process_tag(v2);
}

namespace {

  /**
   * \brief `xtag' sub-command handler
   *
   * \sa handler_type
   * \sa register_command
   *
   *
   * `scribbu xtag' is a sub-command for maintaining my custom XTAG frame in
   * ID3v2 tags.
   *
   *
   */

  int
  handle_xtag(int argc, char **argv)
  {
    using namespace std;

    int status = EXIT_SUCCESS;

    /////////////////////////////////////////////////////////////////////////////
    //                                                                         //
    //                       C O M M A N D   O P T I O N S                     //
    //                                                                         //
    // Let's divide the options in two ways:                                   //
    //                                                                         //
    // - public versus developer-only options                                  //
    // - options permissible only on the command line versus options           //
    //   permissible on the command line, configuration file, and the          //
    //   environment                                                           //
    //                                                                         //
    //                            public   private                             //
    //                          +--------+---------+                           //
    //                cli-only  | clopts | xclopts |                           //
    //                          +--------+---------+                           //
    //                cli & env |  opts  |  xopts  |                           //
    //                          +--------+---------+                           //
    //                                                                         //
    /////////////////////////////////////////////////////////////////////////////

    po::options_description clopts("command-line only options");
    clopts.add_options()
      ("help,h", po::bool_switch(), "Display help & exit")
      ("info", po::bool_switch(), "Display help in Info format & exit");

    po::options_description xclopts("command-line only developer options");
    xclopts.add_options()
      ("man", po::bool_switch(), "Display the man page & exit");

    po::options_description opts("general options");
    opts.add_options()
      ("adjust-unsync,u", po::bool_switch(), "Update the unsynchronisation "
       "flag as needed on write (default is to never use it).")
      ("always-create-v2,A", po::bool_switch(), "Always create an ID3v2 tag"
       "with an XTAG frame for any file that has no ID3v2 tag")
      ("create,a", po::bool_switch(), "Create a new XTAG frame if not present")
      ("create-v2,c", po::bool_switch(), "Create an ID3v2.3 tag containing "
       "an XTAG frame for any file that an ID3v1 tag, but no ID3v2 tag")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
      ("dry-run,n", po::bool_switch(), "Don't do anything; just print what "
       "*would* be done")
      ("merge,m", po::bool_switch(), "Merge the given tags, don't overwrite")
      ("owner,o", po::value<string>(), "Operate only on XTAG frames with "
       "this owner, or specify the owner in case an XTAG frame is being created")
      ("index,t", po::value<vector<size_t>>(), "Zero-based index of the tag "
       "on which to operate; may be given more than once to select "
       "multiple tags")
      ("tags,T", po::value<string>(), "Tags to be set or merged "
       "expressed in HTTP query params style using URL-encoding, e.g. "
       "foo&bar=has%20%2c&splat=a,b,c");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or "
       "more files or directories to be examined; if a directory is given, it "
       "will be searched recursively");

    po::options_description docopts;
    docopts.add(clopts).add(opts);

    po::options_description all;
    all.add(clopts).add(xclopts).add(opts).add(xopts);

    po::positional_options_description popts;
    popts.add("arguments", -1);

    try {

      vector<string> tokens;
      convert_tokens(argc, argv, back_inserter(tokens));

      po::variables_map vm;
      po::parsed_options parsed = po::command_line_parser(tokens).
        options(all).
        positional(popts).
        run();

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-xtag",
                        "(scribbu) Invoking scribbu xtag");

      po::store(parsed, vm);

      const map<string, string> ENV_OPTS {
        make_pair("SCRIBBU_ADJUST_UNSYNC", "adjust-unsync"),
        make_pair("SCRIBBU_ALWAYS_CREATE_V2", "always-create-v2"),
        make_pair("SCRIBBU_CREATE", "create"),
        make_pair("SCRIBBU_CREATE_V2", "create-v2"),
        make_pair("SCRIBBU_CREATE_BACKUPS", "create-backups"),
        make_pair("SCRIBBU_DRY_RUN", "dry-run"),
        make_pair("SCRIBBU_OWNER", "owner"),
      };
      parsed = po::parse_environment(opts, [&ENV_OPTS](const string &var) {
        auto p = ENV_OPTS.find(var);
        return ENV_OPTS.end() == p ? "" : p->second.c_str();
      });
      po::store(parsed, vm);

      po::notify(vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...

      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      std::vector<fs::path> args;
      if (vm.count("arguments")) {
        for (auto s: vm["arguments"].as<std::vector<string>>()) {
          args.push_back(fs::path(s));
        }
      }

      bool adj_unsync       = vm["adjust-unsync"   ].as<bool>();
      bool always_create_v2 = vm["always-create-v2"].as<bool>();
      bool create           = vm["create"          ].as<bool>();
      bool create_v2        = vm["create-v2"       ].as<bool>();
      bool create_backups   = vm["create-backups"  ].as<bool>();
      bool dry_run          = vm["dry-run"         ].as<bool>();
      bool merge            = vm["merge"           ].as<bool>();

      string owner;
      if (vm.count("owner")) {
        owner = vm["owner"].as<string>();
      }

      vector<size_t> tags;
      if (vm.count("tag")) {
        tags = vm["tag"].as<vector<size_t>>();
      }

      if (!vm.count("tags")) {
        throw po::required_option("--tags");
      }
      string tag_cloud = vm["tags"].as<string>();

      typedef typename tagset_processor::v2_creation_policy v2_creation_policy;
      v2_creation_policy v2cp;
      if (always_create_v2 && create_v2) {
        throw po::error("at most of of --create-v2 & --always-create-v2 may be "
                        "given");
      } else if (!always_create_v2 && create_v2) {
        v2cp = v2_creation_policy::when_v1_present;
      } else if (always_create_v2 && !create_v2) {
        v2cp = v2_creation_policy::always;
      } else {
        v2cp = v2_creation_policy::never;
      }

      unique_ptr<set_xtag> pF;
      if (tags.empty()) {
        pF.reset(new set_xtag(tag_cloud, owner, create, merge, v2cp,
                              create_backups, dry_run, adj_unsync));
      } else {
        pF.reset(new set_xtag(tags.begin(), tags.end(), tag_cloud,
                              owner, create, merge, v2cp, create_backups,
                              dry_run, adj_unsync));
      }

      process_dirent_args(args.begin(), args.end(), *pF);

    } catch (const po::error &ex) {

      cerr << ex.what() << endl;
      print_usage(cerr, docopts, USAGE);
      status = EXIT_INCORRECT_USAGE;

    } catch (const std::exception &ex) {

      cerr << ex.what() << endl;
      status = EXIT_FAILURE;

    }

    return status;

  }

  register_command r("xtag", handle_xtag);

}
