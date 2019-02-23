/**
 * \file popm.cc
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
#include <boost/regex.hpp>

#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/tagset.hh>


namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"(scribbu popm -- manage playcount and/or popularimeter frames

scribbu popm [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, increment the play count in all PCNT & POPM frames in all tags
in the files named on the command line. If an argument is a file, increment
the play count in all files in the directory tree rooted at that argument
containing ID3v2 tags.

This operation can be scoped in a number of ways:

    scribbu popm -t=1 ...

will only increment the play count in the second tag in every file.

    scribbu popm -p ...

will only modify PCNT frames (not POPM).

    scribbu popm -m

will only modify POPM frames (not PCNT). Many options can be combined:

    scribbu popm -t 0 -o sp1ff@pbox.com -c 111

will set the play count to 111 if the first ID3v2 tag in the POPM
frame with owner sp1ff@pobox.com.

For detailed help, say `scribbu popm --help'. To see the manual, say
`info "scribbu (popm) "'.
)");


////////////////////////////////////////////////////////////////////////////////
//                             handler                                        //
////////////////////////////////////////////////////////////////////////////////

namespace {

  /// interpret \a text as a rating
  unsigned char 
  rating_for_text(const std::string &text)
  {
    using boost::regex;
    using boost::smatch;

    using namespace std;
    
    // let's try to interpret `text' as an unsigned integer...
    try {
      unsigned long ul = stoul(text);
      // less than or equal to 255...
      if (255 < ul) {
        throw std::out_of_range("rating too large");
      }
      // done.
      return (unsigned char) ul;
    } catch (const std::exception&) {
      /* swallow */
    }

    // OK-- that didn't work. Let's match `text' against a regex
    // selecting a single character repeated between one & five times.
    smatch what;
    regex re1(".{1,5}");
    if (! regex_match(text, what, re1)) {
      // Well, that didn't work-- give up: maybe the user fat-fingered
      // the option?
      throw po::invalid_option_value("bad rating");
    }
    
    // So it's a single character repeated a number of times-- is it one we accept?
    regex re2("[a-zA-Z@#%*+]{1,5}");
    if (! regex_match(text, what, re1)) {
      // Stranger, but still we give up:
      throw po::invalid_option_value("bad rating");
    }
    
    // 1-51 (26)      => *
    // 52-102 (77)    => **
    // 103->153 (128) => ***
    // 154->204 (179) => ****
    // 205->255 (230) => *****
    if (1 == text.length()) {
      return 26;
    } else if (2 == text.length()) {
      return 77;
    } else if (3 == text.length()) {
      return 128;
    } else if (4 == text.length()) {
      return 179;
    } else {
      return 230;
    }
    
  }

  //////////////////////////////////////////////////////////////////////////
  // The processing logic is largely identical for id3v2_{2,3,4}_tag, so it
  // makes sense to factor it out into a function template.  The logic
  // requires a set of related types, so I've grouped them into a traits
  // type. I'm sure there's some kind of template metaprogramming 
  // wizardry that could do this more elegantly, but I'm tired, dirty
  // & frustrated :)
  //////////////////////////////////////////////////////////////////////////

  template <typename tag_type>
  struct tag_traits
  { };
  
  template <>
  struct tag_traits<scribbu::id3v2_2_tag>
  {
    typedef scribbu::id3v2_2_tag   tag_type;
    typedef scribbu::id3v2_2_frame frame_type;
    typedef scribbu::CNT           pcnt_type;
    typedef scribbu::POP           popm_type;
    
    static bool is_pcnt(const frame_type &F) {
      static const scribbu::frame_id3 PCNT("CNT");
      return F.id() == PCNT;
    }

    static bool is_popm(const frame_type &F) {
      static const scribbu::frame_id3 POPM("POP");
      return F.id() == POPM;
    }
    
    static pcnt_type make_pcnt(std::size_t n) {
      return scribbu::CNT(n);
    }

    static popm_type make_popm(const std::string &email,
                               unsigned char    rating,
                               std::size_t      n) {
      return scribbu::POP(email, rating, n);
    }
  };

  template <>
  struct tag_traits<scribbu::id3v2_3_tag>
  {
    typedef scribbu::id3v2_3_tag   tag_type;
    typedef scribbu::id3v2_3_frame frame_type;
    typedef scribbu::PCNT          pcnt_type;
    typedef scribbu::POPM          popm_type;
    
    static bool is_pcnt(const frame_type &F) {
      static const scribbu::frame_id4 PCNT("PCNT");
      return F.id() == PCNT;
    }

    static bool is_popm(const frame_type &F) {
      static const scribbu::frame_id4 POPM("POPM");
      return F.id() == POPM;
    }

    static pcnt_type make_pcnt(std::size_t n) {
      using namespace scribbu;
      return PCNT(n, id3v2_3_plus_frame::tag_alter_preservation::preserve, 
                  id3v2_3_plus_frame::file_alter_preservation::preserve, 
                  id3v2_3_plus_frame::read_only::clear, boost::none,
                  boost::none, boost::none);
    }

    static popm_type make_popm(const std::string &email,
                               unsigned char    rating,
                               std::size_t      n) {
      using namespace scribbu;
      return POPM(email, rating, n, 
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
    typedef scribbu::PCNT_2_4      pcnt_type;
    typedef scribbu::POPM_2_4      popm_type;
    
    static bool is_pcnt(const frame_type &F) {
      static const scribbu::frame_id4 PCNT("PCNT");
      return F.id() == PCNT;
    }

    static bool is_popm(const frame_type &F) {
      static const scribbu::frame_id4 POPM("POPM");
      return F.id() == POPM;
    }

    static pcnt_type make_pcnt(std::size_t n) {
      using namespace scribbu;
      return PCNT_2_4(n, id3v2_3_plus_frame::tag_alter_preservation::preserve, 
                      id3v2_3_plus_frame::file_alter_preservation::preserve, 
                      id3v2_3_plus_frame::read_only::clear, boost::none,
                      boost::none, false, false, boost::none);
    }

    static popm_type make_popm(const std::string &email,
                               unsigned char    rating,
                               std::size_t      n) {
      using namespace scribbu;
      return POPM_2_4(email, rating, n, 
                      id3v2_3_plus_frame::tag_alter_preservation::preserve, 
                      id3v2_3_plus_frame::file_alter_preservation::preserve, 
                      id3v2_3_plus_frame::read_only::clear, boost::none,
                      boost::none, false, false, boost::none);
    }
  };

  template <typename tag_type>
  bool
  process_tag(tag_type                             &tag, 
              bool                                  playcount_only, 
              bool                                  popularimeter_only,
              const std::string                    &owner,
              const boost::optional<size_t>        &increment, 
              const boost::optional<size_t>        &play_count, 
              const boost::optional<unsigned char> &rating,
              bool                                  create,
              bool                                  dry_run)
  {
    using namespace std;

    typedef tag_traits<tag_type> traits_type;

    typedef typename traits_type::frame_type frame_type;
    typedef typename traits_type::pcnt_type  pcnt_type;
    typedef typename traits_type::popm_type  popm_type;
    
    size_t num_pcnt = 0, num_popm = 0;
    for (frame_type &F: tag) {
      
      if (!popularimeter_only && traits_type::is_pcnt(F)){

        // We have a playcount frame & we mean to process it
        pcnt_type &G = dynamic_cast<pcnt_type&>(F);
        if (play_count) {
          size_t cnt = *play_count;
          G.count(cnt);
          if (dry_run) {
            cout << "setting PCNT to " << cnt << endl;
          }
        } else {
          size_t inc = 1;
          if (increment) {
            inc = *increment;
          }
          if (dry_run) {
            cout << "setting PCNT to " << G.count() + inc << endl;
          }
          G.count(G.count() + inc);
        }
        ++num_pcnt;

      } else if (!playcount_only && traits_type::is_popm(F)) {

        // We have a popularimeter frame...
        popm_type &G = dynamic_cast<popm_type&>(F);
        // do we scope it by owner?
        std::string email = G.template email<std::string>();
        if (owner.empty() || owner == email) {
          if (play_count) {
            size_t cnt = *play_count;
            G.count(cnt);
            if (dry_run) {
              cout << "setting POPM (" << email << ") count to " << cnt << endl;
            }
          } else {
            size_t inc = 1;
            if (increment) {
              inc = *increment;
            }
            if (dry_run) {
              cout << "setting POPM (" << email << ") count to " << 
                G.count() + inc << endl;
            }
            G.count(G.count() + inc);
          }
          if (rating) {
            unsigned char r = *rating;
            G.rating(r);
            if (dry_run) {
              cout << "setting POPM (" << email << ") rating to " << (int)r <<
                endl;
            }
          }
          ++num_popm;

        } // End if on `owner'.

      } // End if on playcount or popularimeter frame.
        
    } // End iteration over `tag''s frames.
    
    bool upd = (num_popm != 0) || (num_pcnt != 0);

    if (create) {
      
      if (0 == num_pcnt && !popularimeter_only) {
        
        if (dry_run) {
          cout << "creating a new PCNT frame with a count of " <<
            (play_count ? *play_count : 0) << endl;
        }

        tag.push_back(traits_type::make_pcnt(play_count ? *play_count : 0));
        upd = true;

      }
      else if (0 == num_popm && !playcount_only) {
        
        if (dry_run) {
          cout << "creating a new POPM frame for " << owner << 
            " with a count of " <<
            (play_count ? *play_count : 0) << " and a rating of " <<
            *rating << endl;
        }

        tag.push_back(traits_type::make_popm(owner, *rating, 
                                             play_count ? *play_count : 0));
        upd = true;

      }
      
    } // End if on `create'.
    
    return upd;

  } // End free function template `process_tag'.

  /**
   * \brief Process a single file
   *
   *
   * \param pth [in] path to the file to be processed
   *
   * \param tags [in] vector of indicies naming tags to be processed
   *
   * \param playcount_only [in] if true, only process PCNT frames; only one of
   * \a playcount_only & \a popularimeter only may be true
   *
   * \param popularimeter_only [in] if true, only process POPM frames; only one
   * of \a playcount_only & \a popularimeter only may be true
   *
   * \param owner [in] if we're processing POPM frames, only processes POPM
   * frames owned by \a owner
   *
   * \param increment [in] number by which to increment the playcount (in either
   * PCNT or POPM frames); only one of \a increment & \a play_count may be not
   * none
   *
   * \param play_count [in] number to which to set the playcount (in either PCNT
   * or POPM frames); only one of \a increment & \a play_count may be not none
   *
   * \param rating [in] rating to use if we're setting POPM frames
   *
   * \param create [in] if true, create the PCNT and/or POPM frames if they
   * don't exist
   *
   * \param create_backups [in] if true, create backup files before writing
   *
   * \param dry_run [in] if true, don't actually do anything; just print what
   * *would* be done
   *
   *
   */
  
  void
  process_file(const fs::path                       &pth, 
               const std::deque<size_t>             &dtags,
               bool                                  playcount_only, 
               bool                                  popularimeter_only, 
               const std::string                    &owner, 
               const boost::optional<size_t>        &increment, 
               const boost::optional<size_t>        &play_count, 
               const boost::optional<unsigned char> &rating, 
               bool                                  create, 
               bool                                  create_backups, 
               bool                                  dry_run,
               bool                                  adj_unsync)
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
          upd = process_tag(tag, playcount_only, popularimeter_only,
                            owner, increment, play_count, rating,
                            create, dry_run);
        } else if (3 == version) {
          id3v2_3_tag &tag = dynamic_cast<id3v2_3_tag&>(*ptag);
          upd = process_tag(tag, playcount_only, popularimeter_only,
                            owner, increment, play_count, rating,
                            create, dry_run);
        } else if (4 == version) {
          id3v2_4_tag &tag = dynamic_cast<id3v2_4_tag&>(*ptag);
          upd = process_tag(tag, playcount_only, popularimeter_only,
                            owner, increment, play_count, rating,
                            create, dry_run);
        } else {

          // TODO(sp1ff): throw something more specific?
          throw std::logic_error("unknown ID3v2 version!");

        }
        
        any_upd = any_upd || upd;

        tags.pop();
      }

    } // End loop over all ID3v2 tags in `pth'.
    
    if (any_upd) {
    
      if (dry_run) {
        
        cout << "the dry-run flag was given-- nothing written";

        if (create_backups) {
          
          cout << " (with backup)";

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

  } // End free function template `process_file'.

  /// Handle a single filesystem entity
  void
  process_dirent(const fs::path                       &pth,
                 const std::deque<size_t>             &tags,
                 bool   	                           playcount_only,
                 bool   	                           popularimeter_only,
                 const std::string                    &owner,
                 const boost::optional<size_t>        &increment,
                 const boost::optional<size_t>        &play_count,
                 const boost::optional<unsigned char> &rating,
                 bool   	                           create,
                 bool   	                           create_backups,
                 bool   	                           dry_run,
                 bool                                  adj_unsync)
  {
    if (fs::is_directory(pth)) {
      std::for_each(fs::directory_iterator(pth), fs::directory_iterator(),
                    [=] (const fs::path &p) {
                      if (!fs::is_directory(p)) {
                        process_file(p, tags, playcount_only, 
                                     popularimeter_only, owner, increment, 
                                     play_count, rating, create, 
                                     create_backups, dry_run, adj_unsync);
                      }
                    });
    } else {
      process_file(pth, tags, playcount_only, popularimeter_only, 
                   owner, increment, play_count, rating, 
                   create, create_backups, dry_run, adj_unsync);
    }
  }
  
  int
  handle_popm(int argc, char **argv)
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
    // None at this time...

    po::options_description xclopts("command-line only developer options");
    // None at this time...

    po::options_description opts("general options");
    opts.add_options()
      ("adjust-unsync,u", po::bool_switch(), "Update the unsynchronisation "
       "flag as needed on write (default is to never use it).")
      ("count,c", po::value<size_t>(), "Set the play count "
       "to this value")
      ("create,a", po::bool_switch(), "Create a new frame; if only -c is "
       "given, create a new PCNT frame; if -c, -o and -r are given, create "
       "a POPM frame")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
      ("dry-run,n", po::bool_switch(), "Don't do anything; just print what "
       "*would* be done")
      ("help,h", po::bool_switch(), "Display help & exit")
      ("increment,i", po::value<size_t>(), "Increment by "
       "which to increment the play count")
      ("owner,o", po::value<string>(), "Operate only on POPM frames with "
       "this owner, or specify the owner in case a POPM frame is being created")
      ("playcount-only,p", po::bool_switch(), "Operate only on PCNT frames")
      ("popularimeter-only,m", po::bool_switch(), "Operate only on POPM frames")
      ("rating,r", po::value<string>(), "Set the rating in popularimeter "
       "tag (s); either 0-255, or X{1,5} for X in [a-zA-Z@#%*+]")
      ("tag,t", po::value<vector<size_t>>(), "Zero-based index of the tag "
       "on which to operate; may be given more than once to select "
       "multiple tags");
     
    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>(), "one or more "
       "files or directories to be examined; if a directory is given, it "
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
      po::store(parsed, vm);

      help_level help = help_level::none;
      if (vm.count("help")) {
        help = help_level_for_parsed_opts(parsed);
      }
      
      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      po::notify(vm);

      if (help_level::regular == help) {

        print_usage(cout, docopts, USAGE);

      } else if (help_level::verbose == help) {

        show_man_page("scribbu-popm");
        
      } else {

        // That's it-- the list of files and/or directories to be processed
        // should be waiting for us in 'arguments'...
      
        // Work around to https://svn.boost.org/trac/boost/ticket/8535
        std::vector<fs::path> args;
        if (vm.count("arguments")) {
          for (auto s: vm["arguments"].as<std::vector<string>>()) {
            args.push_back(fs::path(s));
          }
        }

        bool adj_unsync         = vm["adjust-unsync"     ].as<bool  >();
        bool playcount_only     = vm["playcount-only"    ].as<bool  >();
        bool popularimeter_only = vm["popularimeter-only"].as<bool  >();
        bool create             = vm["create"            ].as<bool  >();
        bool create_backups     = vm["create-backups"    ].as<bool  >();
        bool dry_run            = vm["dry-run"           ].as<bool  >();
        
        boost::optional<size_t> increment = boost::none;
        if (vm.count("increment")) {
          increment = vm["increment"].as<size_t>();
        }
         
        boost::optional<size_t> play_count = boost::none;
        if (vm.count("count")) {
          play_count = vm["count"].as<size_t>();
        }

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
        
        boost::optional<unsigned char> rating;
        if (vm.count("rating")) {
          string s = vm["rating"].as<string>();
          rating = rating_for_text(s);
        }
        
        // Validate our options:
        
        // 1. At most one of `playcount_only' & `popularimeter_only' may be true
        if (playcount_only && popularimeter_only) {
          throw po::invalid_option_value("at most one of `playcount-only' & "
                                         "`popularimeter-only' may be given");
        }
        
        // 2. `playcount' & `increment' may not both be given
        if (increment && play_count) {
          throw po::invalid_option_value("at most one of `playcount' & "
                                         "`increment' may be given");
        }

        for_each(args.begin(), args.end(), 
                 [=] (const fs::path &pth) {
                   process_dirent(pth, tags, playcount_only, popularimeter_only, 
                                  owner, increment, play_count, rating, create, 
                                  create_backups, dry_run, adj_unsync);
                 }
               );

      }

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

  register_command r("popm", handle_popm);

}
