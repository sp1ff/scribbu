/**
 * \file xtag.cc
 *
 * Copyright (C) 2019 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/tagset.hh>


namespace fs = boost::filesystem;
namespace po = boost::program_options;


const std::string USAGE(R"(scribbu xtag -- manage the tag cloud frame

scribbu xtag [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, create or update the "tag cloud" frame in all tags in the files
named on the command line. If an argument is a directory, create or update the
tag cloud in all tags in all files in the directory tree rooted at that
argument.

The tag cloud is expressed in urlencoded query format:

    scribbu xtag --tags=sub-genres=foo,bar&90s

will update the tag cloud for any XTAG frames found.

This operation can be scoped in a few ways:

    scribbu xtag -t=1 ...

will only operate on the XTAG frame in the second tag in each file (the
option can be given more than once).

    scribbu xtag --tags=sub-genres=foo,bar&90s --merge

will "merge" the tags instead of overwriting.

For detailed help, say `scribbu xtag --help'. To see the manual, say
`info "scribbu (xtag) "'.
)");


////////////////////////////////////////////////////////////////////////////

namespace {

  template <typename tag_type>
  struct tag_traits
  { };
  
  template <>
  struct tag_traits<scribbu::id3v2_2_tag>
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
  struct tag_traits<scribbu::id3v2_3_tag>
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
  struct tag_traits<scribbu::id3v2_4_tag>
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

  template <typename tag_type>
  bool
  process_tag(tag_type          &tag,
              const std::string &tag_cloud, 
              const std::string &owner, 
              bool               create, 
              bool               merge, 
              bool               dry_run)
  {
    using namespace std;

    typedef tag_traits<tag_type> traits_type;

    typedef typename traits_type::frame_type frame_type;
    typedef typename traits_type::xtag_type  xtag_type;

    size_t num_xtag = 0;
    for (frame_type &F: tag) {
      
      if (traits_type::is_xtag(F)) {

        xtag_type &G = dynamic_cast<xtag_type&>(F);
        std::string email = G.owner();
        if (owner.empty() || owner == email) {
          if (merge) {
            if (dry_run) {
              cout << "Merging " << tag_cloud << " into XTAG (" << email <<
                ")" << endl;
            } else {
              G.merge(tag_cloud);
            }
          } else {
            if (dry_run) {
              cout << "Setting " << tag_cloud << " into XTAG (" << email <<
                ")" << endl;
            } else {
              G.update(tag_cloud);
            }
          }
          num_xtag++;
        }

      } // End if on F being XTAG frame.

    } // End iteration over `tag''s frames.
    
    bool upd = num_xtag != 0;

    if (!num_xtag && create) {
      
      if (dry_run) {
        cout << "creating a new XTAG frame for " << owner << ": " <<
          tag_cloud << endl;
      } else {
        tag.push_back(traits_type::make_xtag(owner, tag_cloud));
      }
      
      upd = true;

    }
    
    return upd;
  
  } // End free function template `process_tag'.

  void
  process_file(const fs::path           &pth,
               const std::string        &tag_cloud,
               const std::deque<size_t> &dtags,
               const std::string        &owner,
               bool                      create,
               bool                      merge,
               bool                      create_backups,
               bool                      dry_run,
               bool                      adj_unsync)
  {
    using namespace std;
    using namespace scribbu;

    queue<size_t> tags(dtags);

    // TODO(sp1ff): Why convert here? Why not higher up the call stack?
    vector<unique_ptr<id3v2_tag>> T;
    
    {
      fs::ifstream ifs(pth, fs::ifstream::binary);
      read_all_id3v2(ifs, back_inserter(T));
    }

    bool any_upd = false;

    for (size_t i = 0, n = T.size(); i < n; ++i) {
      
      if (tags.front() == i) {

        bool upd = false;
        id3v2_tag *ptag = T[i].get();
        unsigned char version = ptag->version();
        if (2 == version) {
          id3v2_2_tag &tag = dynamic_cast<id3v2_2_tag&>(*ptag);
          upd = process_tag(tag, tag_cloud, owner, create, merge, dry_run);
        } else if (3 == version) {
          id3v2_3_tag &tag = dynamic_cast<id3v2_3_tag&>(*ptag);
          upd = process_tag(tag, tag_cloud, owner, create, merge, dry_run);
        } else if (4 == version) {
          id3v2_4_tag &tag = dynamic_cast<id3v2_4_tag&>(*ptag);
          upd = process_tag(tag, tag_cloud, owner, create, merge, dry_run);
        } else {

          // TODO(sp1ff): throw something more specific?
          throw std::logic_error("unknown ID3v2 version!");

        }
        
        tags.pop();
        any_upd = any_upd || upd;
      }

    } // End loop over all ID3v2 tags in `pth'.
    
    if (any_upd) {
      
      if (dry_run) {
      
        cout << "the dry-run flag was given-- nothing written";
      
        if (create_backups) {
        
          cout << " (with backup).";

        }
      
        cout << endl;

      } else {
      
        apply_unsync au = adj_unsync ? apply_unsync::as_needed :
          apply_unsync::never;
        
        if (create_backups) {
          
          replace_tagset_copy(pth, T.begin(), T.end(), au);

        } else {
        
          maybe_emplace_tagset(pth, T.begin(), T.end(), au,
                               emplace_strategy::only_with_full_padding,
                               padding_strategy::adjust_padding_evenly);

        } // End if on `create_backups'.
        
      } // End if on `dry_run'.
      
    } else if (dry_run) {

      cout << "the tagset wasn't updated, so nothing would be written" << endl;

    }

  } // End free function `process_file'/

  /// Handle a single filesystem entity
  void
  process_dirent(const fs::path            &pth,
                 const std::string         &tag_cloud,
                 const std::deque<size_t>  &tags,
                 const std::string         &owner,
                 bool                       create,
                 bool                       merge,
                 bool                       create_backups,
                 bool                       dry_run,
                 bool                       adj_unsync)
  {
    if (fs::is_directory(pth)) {
      std::for_each(fs::directory_iterator(pth), fs::directory_iterator(),
                    [=] (const fs::path &p) {
                      if (!fs::is_directory(p)) {
                        process_file(p, tag_cloud, tags, owner, create,
                                     merge, create_backups, dry_run,
                                     adj_unsync);
                      }
                    });
    } else {
      process_file(pth, tag_cloud, tags, owner, create, merge, 
                   create_backups, dry_run, adj_unsync);
    }
  }

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
      ("create,a", po::bool_switch(), "Create a new XTAG frame if not present")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
      ("dry-run,n", po::bool_switch(), "Don't do anything; just print what "
       "*would* be done")
      ("merge,m", po::bool_switch(), "Merge the given tags, don't overwrite")
      ("owner,o", po::value<string>(), "Operate only on XTAG frames with "
       "this owner, or specify the owner in case an XTAG frame is being created")
      ("tag,t", po::value<vector<size_t>>(), "Zero-based index of the tag "
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

    po::options_description nocli;
    nocli.add(opts).add(xopts);

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

      parsed = po::parse_environment(nocli, "SCRIBBU");
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

      bool adj_unsync     = vm["adjust-unsync" ].as<bool>();
      bool create         = vm["create"        ].as<bool>();
      bool create_backups = vm["create-backups"].as<bool>();
      bool dry_run        = vm["dry-run"       ].as<bool>();
      bool merge          = vm["merge"         ].as<bool>();

      string owner;
      if (vm.count("owner")) {
        owner = vm["owner"].as<string>();
      }
      
      deque<size_t> tags;
      if (vm.count("tag")) {
        vector<size_t> V = vm["tag"].as<vector<size_t>>();
        tags.insert(tags.begin(), V.begin(), V.end());
      } else {
        tags.push_back(0);
      }

      if (!vm.count("tags")) {
        throw po::required_option("--tags");
      }
      string tag_cloud = vm["tags"].as<string>();

      for_each(args.begin(), args.end(), 
               [=] (const fs::path &pth) {
                 process_dirent(pth, tag_cloud, tags, owner, create, merge,
                                create_backups, dry_run, adj_unsync);
               }
             );
      
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

