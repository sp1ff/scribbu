/**
 * \file text.cc
 *
 * Copyright (C) 2019-2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/scribbu.hh>
#include <scribbu/charsets.hh>
#include <scribbu/framesv2.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/tagset.hh>
#include <scribbu/id3v2-utils.hh>

#include <deque>
#include <exception>

#include <filesystem>
#include <fstream>
#include <boost/lexical_cast.hpp>

namespace fs = std::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"(scribbu text -- manage text frames

Usage: scrubbu text [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

Create, update or delete one or more text frames in ID3v2 tags (and ID3v1 tags,
if present). By default, update the specified frames in all tags in all files
named on the command line. If an argument is a directory, operate on all tags in
all files in the directory tree rooted at that argument.

Examples:

Set the artist & title in a given file:

    scribbu text --artist='Pogues, The' --title="Lorca's Novena" foo.mp3

Delete the "Encoded By" frame in a directory full of files:

    scribbu text --delete=TENC foo/

The operation can be scoped in a few ways; for instance, to set the
artist in a group of files, but only in the first tag:

    scribbu text --tag=0 --artist='Pogues, The' *.mp3

The --tag option can be given more than once to specify multiple indicies.

By default, only existing tags will be processed. When setting a frame or
frames, specify the -c flag to create a new ID3v2 tag to house them, but only if
there's already an ID3v1 tag present (any information therein will be copied to
the new ID3v2 tag). The -C flag will unconditionally create a new ID3v2
tag. This should be used with care when operating on directories, or you may
find assorted, non-music files have had ID3v2 tags prepended to them.

For detailed help, say `scribbu text --help'. To see the manual, say
`info "scribbu (text) "'.
)");

////////////////////////////////////////////////////////////////////////////////
//                       functor for processing tagsets                       //
////////////////////////////////////////////////////////////////////////////////

/**
 * \brief tagset_processor for creating, updating or deleting text frames
 *
 * \sa process_dirent_args
 * \sa tagset_processor
 *
 *
 * Since a lot of the logic of processing one or more "file-or-directory"
 * sub-command arguments, reading their tags & making adjustments is
 * boilerplate, I've factored it out into the process_dirent_args template free
 * function & the tagset_processor functor. The logic specific to the `text'
 * sub-command resides here.
 *
 *
 */

class set_text: public tagset_processor
{
public:
  /// Use this ctor when no --tag options have been specified; it takes
  /// a range of strings to create or update in the form of pairs of
  /// id3v2_text_frames members and associated text, a range of frames
  /// to delete in the form of id3v2_text_frames members, along with the
  /// other usual parameters.
  template <typename FIIS, // forward input iterator * => pair<id3v2_text_frames, string>
            typename FIID> // forward input iterator * => id3v2_text_frames
  set_text(FIIS ps0, FIIS ps1, FIID pd0, FIID pd1, scribbu::encoding srcenc,
           v2_creation_policy v2cp, bool verbose, bool adj_unsync,
           bool create_backups):
    tagset_processor(v2_simple_tag_scope_policy::all,
                     v1_tag_scope_policy::no,
                     v2cp,
                     v1_creation_policy::never,
                     false,
                     verbose,
                     adj_unsync,
                     create_backups),
    dels_(pd0, pd1),
    adds_(ps0, ps1),
    enc_(srcenc)
  { }
  /// Use this ctor when the --tag option has been given; it takes a range of
  /// tag indicies, a range of strings to create or update in the form of pairs
  /// of id3v2_text_frames members and associated text, a range of frames to
  /// delete in the form of id3v2_text_frames members, along with the other
  /// usual parameters.
  template <typename FIIS, // forward input iterator * => pair<frame, string>
            typename FIID, // forward input iterator * => frame
            typename FIIT> // forward input iterator * => size_t
    set_text(FIIS ps0, FIIS ps1, FIID pd0, FIID pd1, FIIT pt0, FIIT pt1,
             scribbu::encoding srcenc, v2_creation_policy v2cp,
             bool verbose, bool adj_unsync, bool create_backups):
      tagset_processor(pt0, pt1,
                       v1_tag_scope_policy::yes,
                       v2cp,
                       v1_creation_policy::never,
                       false,
                       verbose,
                       adj_unsync,
                       create_backups),
      dels_(pd0, pd1),
      adds_(ps0, ps1),
      enc_(srcenc)
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
  /// This class works through virtuals, so all operations can be concentrated
  /// here (in a method that just takes a ptr to id3v2_tag)
  bool process_tag(scribbu::id3v2_tag *p);

private:
  typedef std::vector<scribbu::id3v2_text_frames> frames_type;
  typedef std::map<scribbu::id3v2_text_frames, std::string> strings_type;

  /// Collection of frame IDs to be deleted
  frames_type dels_;
  /// Collection of strings to be added/updated
  strings_type adds_;
  /// Character encoding used for all strings in `adds_'
  scribbu::encoding enc_;

};

/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_text::create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &/*v2*/)
{
  throw bad_operation(unknown_op::create_v1_from_v2);
}

/// Create a new ID3v1 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_text::create_v1()
{
  throw bad_operation(unknown_op::create_v1);
}

/// Create a new ID3v2 tag when there's an ID3v1 tag present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_text::create_v2(const scribbu::id3v1_tag &v1)
{
  auto p = copy_id3_v1(v1);
  process_tag(p.get());
  return p;
}

/// Create a new ID3v2 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_text::create_v2()
{
  auto p = std::make_unique<scribbu::id3v2_3_tag>(DEFAULT_PADDING);
  process_tag(p.get());
  return p;
}

/// Process the ID3v1 tag
/*virtual*/ bool
set_text::process_v1(scribbu::id3v1_tag &v1)
{
  using namespace std;
  using namespace scribbu;

  size_t num_deltas = 0;
  for_each(dels_.begin(), dels_.end(), [&](scribbu::id3v2_text_frames id) {
    switch (id) {
    case id3v2_text_frames::tit2:
      v1.set_title(string());
      break;
    case id3v2_text_frames::tpe1:
      v1.set_artist(string());
      break;
    case id3v2_text_frames::talb:
      v1.set_album(string());
      break;
    default:
      // do nothing
      break;
    }
  });

  for_each(adds_.begin(), adds_.end(), [&](const strings_type::value_type &x) {
    switch (x.first) {
    case id3v2_text_frames::tit2:
      v1.set_title(x.second, enc_);
      break;
    case id3v2_text_frames::tpe1:
      v1.set_artist(x.second, enc_);
      break;
    case id3v2_text_frames::talb:
      v1.set_album(x.second, enc_);
      break;
    default:
      // do nothing
      break;
    }
  });

  return num_deltas != 0;    
}

/// Process an ID3v2.2 tag
/*virtual*/ bool
set_text::process_v2(scribbu::id3v2_2_tag &v2)
{
  return process_tag(&v2);
}

/// Process an ID3v2.3 tag
/*virtual*/ bool
set_text::process_v2(scribbu::id3v2_3_tag &v2)
{
  return process_tag(&v2);
}

/// Process an ID3v2.4 tag
/*virtual*/ bool
set_text::process_v2(scribbu::id3v2_4_tag &v2)
{
  return process_tag(&v2);
}

/// This class works through virtuals, so all operations can be concentrated
/// here (in a method that just takes a ptr to id3v2_tag)
bool
set_text::process_tag(scribbu::id3v2_tag *ptag)
{
  using namespace std;
  for_each(dels_.begin(), dels_.end(), [ptag, this](scribbu::id3v2_text_frames id) {
    if (verbose()) {
      cout << "Deleting " << id << "...";
    }
    ptag->delete_frame(id);
    if (verbose()) {
      cout << "done." << endl;
    }
  });
  for_each(adds_.begin(), adds_.end(), [ptag, this](const strings_type::value_type &x) {
    if (verbose()) {
      cout << "Setting " << x.first << " to " << x.second << "...";
    }
    ptag->text(x.first, x.second, enc_);
    if (verbose()) {
      cout << "done." << endl;
    }
  });
  return true;
}

namespace {

  int
  handle_text(int argc, char **argv)
  {
    using namespace std;

    using boost::lexical_cast;

    using scribbu::encoding;
    using scribbu::encoding_from_system_locale;
    using scribbu::id3v2_text_frames;

    int status = EXIT_SUCCESS;

    ///////////////////////////////////////////////////////////////////////////
    //                                                                       //
    //                       C O M M A N D   O P T I O N S                   //
    //                                                                       //
    // Let's divide the options in two ways:                                 //
    //                                                                       //
    // - public versus developer-only options                                //
    // - options permissible only on the command line versus options         //
    //   permissible on the command line, configuration file, and the        //
    //   environment                                                         //
    //                                                                       //
    //                            public   private                           //
    //                          +--------+---------+                         //
    //                cli-only  | clopts | xclopts |                         //
    //                          +--------+---------+                         //
    //                cli, cfg, |  opts  |  xopts  |                         //
    //                & env     +--------+---------+                         //
    //                                                                       //
    ///////////////////////////////////////////////////////////////////////////

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
      ("album,a", po::value<string>(), "Set the TALB, or Album/Movie/Show Title"
       " frame")
      ("always-create-v2,C", po::bool_switch(), "Always create an ID3v2 tag"
       "with the given text frames for any file that has no ID3v2 tag")
      ("artist,A", po::value<string>(), "Set the TPE1, or Lead artist(s)/Lead "
       "performer(s)/Soloist(s)/Performing group frame")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
      ("create-v2,c", po::bool_switch(), "Create an ID3v2.3 tag containing "
       "the given text frames for any file that an ID3v1 tag, but no ID3v2 tag")
      ("delete,d", po::value<vector<string>>(), "Specify a frame to remove, if"
       "present; this option may be given more than once to delete multiple "
       "frames. Frames may be named by either their option name (e.g. `artist')"
       " or by their ID3v2.3 frame ID (e.g. TPE1).")
      ("encoding,E", po::value<encoding>(), "Specify the character encoding "
       "used in the input strings using the iconv name (`ISO-8859-1', e.g.) "
       "If not given, the system locale will be assumed")
      ("encoded-by,e", po::value<string>(), "Set the TENC, or Encoded By frame")
      ("genre,g", po::value<string>(), "Set the TCON, or Content time frame")
      ("tag,t", po::value<vector<size_t>>(), "Zero-based index of the tag "
       "on which to operate; may be given more than once to select "
       "multiple tags")
      ("title,T", po::value<string>(), "Set the TIT2, or Title/Songname/Content"
       " description frame")
      ("track,k", po::value<string>(), "Set the TRCK, or Track number/Position "
       "in set frame")
      ("year,y", po::value<string>(), "Set the TYER, or Year frame")
      ("verbose,v", po::bool_switch(), "Turn on verbose output");;

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
       "files or directories to be examined; if a directory is given, it will "
       "be searched recursively");

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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-text",
                        "(scribbu) Invoking scribbu text");

      po::store(parsed, vm);

      const map<string, string> ENV_OPTS {
        make_pair("SCRIBBU_ADJUST_UNSYNC", "adjust-unsync"),
        make_pair("SCRIBBU_ALWAYS_CREATE_V2", "always-create-v2"),
        make_pair("SCRIBBU_CREATE_BACKUPS", "create-backups"),
        make_pair("SCRIBBU_CREATE_V2", "create-v2"),
        make_pair("SCRIBBU_ENCODING", "encoding"),
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
      bool create_v2        = vm["create-v2"       ].as<bool>();
      bool make_bu          = vm["create-backups"  ].as<bool>();
      bool verbose          = vm["verbose"         ].as<bool>();

      encoding srcenc = vm.count("encoding") ?
        vm["encoding"].as<encoding>() : encoding_from_system_locale();

      map<id3v2_text_frames, string> adds;
      if (vm.count("album")) {
        adds[id3v2_text_frames::talb] = vm["album"].as<string>();
      }
      if (vm.count("artist")) {
        adds[id3v2_text_frames::tpe1] = vm["artist"].as<string>();
      }
      if (vm.count("encoded-by")) {
        adds[id3v2_text_frames::tenc] = vm["encoded-by"].as<string>();
      }
      if (vm.count("genre")) {
        adds[id3v2_text_frames::tcon] = vm["genre"].as<string>();
      }
      if (vm.count("title")) {
        adds[id3v2_text_frames::tit2] = vm["title"].as<string>();
      }
      if (vm.count("track")) {
        adds[id3v2_text_frames::trck] = vm["track"].as<string>();
      }
      if (vm.count("year")) {
        adds[id3v2_text_frames::tyer] = vm["year"].as<string>();
      }

      vector<id3v2_text_frames> dels;
      if (vm.count("delete")) {
        for (auto s: vm["delete"].as<vector<string>>()) {
          dels.push_back(lexical_cast<id3v2_text_frames>(s));
        }
      }

      vector<size_t> tags;
      if (vm.count("tag")) {
        tags = vm["tag"].as<vector<size_t>>();
      }

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

      unique_ptr<set_text> pF;
      if (tags.empty()) {
        pF.reset(new set_text(adds.begin(), adds.end(),
                              dels.begin(), dels.end(),
                              srcenc, v2cp, verbose,
                              adj_unsync, make_bu));
      } else {
        pF.reset(new set_text(adds.begin(), adds.end(),
                              dels.begin(), dels.end(),
                              tags.begin(), tags.end(),
                              srcenc, v2cp, verbose,
                              adj_unsync, make_bu));
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

  register_command r("text", handle_text);

}
