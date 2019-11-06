/**
 * \file text.cc
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

#include <scribbu/scribbu.hh>
#include <scribbu/charsets.hh>
#include <scribbu/framesv2.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/tagset.hh>
#include <scribbu/id3v2-utils.hh>

#include <deque>
#include <exception>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>


namespace fs = boost::filesystem;
namespace po = boost::program_options;


const std::string USAGE(R"(scribbu text -- manage text frames

scrubbu text [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

Create, update or delete one or more text frames in ID3v2 tags.

Set the artist & title in a given file:

    scribbu text --artist='Pogues, The' --title="Lorca's Novena" foo.mp3

Set the artist in a group of files, but only in the first tag:

    scribbu text --tag=0 --artist='Pogues, The' *.mp3

Delete the "Encoded By" frame in a directory full of files:

    scribbu text --delete=TENC foo/

For detailed help, say `scribbu text --help'. To see the manual, say
`info "scribbu (text) "'.
)");


namespace {

  /// convenience typedef for a list of indicies
  typedef std::deque<std::size_t> idx_deque;
  /// convenience typedef for a list of text frames
  typedef std::vector<scribbu::id3v2_text_frames> frames_vec;
  /// convenience typedef for a mapping from text frame identifier to the
  /// text to be added or updated
  typedef std::map<scribbu::id3v2_text_frames, std::string> text_map;

  /**
   * \brief Process one tag
   *
   *
   */

  void
  process_tag(scribbu::id3v2_tag *ptag,
              const frames_vec   &dels,
              const text_map     &strings,
              scribbu::encoding   srcenc)
  {
    for (auto id: dels) {
      ptag->delete_frame(id);
    }

    for (const auto& val: strings) {
      ptag->text(val.first, val.second, srcenc);
    }

  }

  /**
   * \brief Process one file
   *
   *
   */

  void
  process_file(const fs::path   &pth,
               idx_deque        &tags,
               const frames_vec &dels,
               const text_map   &strings,
               scribbu::encoding srcenc,
               bool              make_bu,
               bool              adj_unsync)
  {
    using namespace std;
    using namespace scribbu;

    vector<unique_ptr<id3v2_tag>> T;

    {
      fs::ifstream ifs(pth, fs::ifstream::binary);
      read_all_id3v2(ifs, back_inserter(T));
    }

    for (size_t i = 0, n = T.size(); i < n; ++i) {

      if (tags.front() == i) {
        process_tag(T[i].get(), dels, strings, srcenc);
        tags.pop_front();
      }

    } // End loop over all ID3v2 tags in `pth'.

    apply_unsync au = adj_unsync ? apply_unsync::as_needed :
      apply_unsync::never;

    if (make_bu) {
      replace_tagset_copy(pth, T.begin(), T.end(), au);
    } else {
      maybe_emplace_tagset(pth, T.begin(), T.end(), au,
                           emplace_strategy::only_with_full_padding,
                           padding_strategy::adjust_padding_evenly);
    }
  }

  /**
   * \brief Process one directory entity
   *
   *
   */

  void
  process_dirent(const fs::path   &pth,
                 idx_deque        &tags,
                 const frames_vec &dels,
                 const text_map   &strings,
                 scribbu::encoding srcenc,
                 bool              make_bu,
                 bool              adj_unsync)
  {
    if (fs::is_directory(pth)) {
      std::for_each(fs::directory_iterator(pth), fs::directory_iterator(),
                    [=] (const fs::path &p) {
                      if (!fs::is_directory(p)) {
                        auto tmp = tags;
                        process_file(p, tmp, dels, strings, srcenc, make_bu,
                                     adj_unsync);
                      }
                    });
    } else {
      process_file(pth, tags, dels, strings, srcenc, make_bu, adj_unsync);
    }
  }

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
      ("artist,A", po::value<string>(), "Set the TPE1, or Lead artist(s)/Lead "
       "performer(s)/Soloist(s)/Performing group frame")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
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
      ("year,y", po::value<string>(), "Set the TYER, or Year frame");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
       "files or directories to be examined; if a directory is given, it will "
       "be searched recursively");

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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-text",
                        "(scribbu) Invoking scribbu text");

      po::store(parsed, vm);

      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      ////////////////////////////////////////////////////////////////////
      //                       process arguments                        //
      ////////////////////////////////////////////////////////////////////

      frames_vec dels;
      if (vm.count("delete")) {
        for (auto s: vm["delete"].as<vector<string>>()) {
          dels.push_back(lexical_cast<id3v2_text_frames>(s));
        }
      }

      text_map strings;

      if (vm.count("album")) {
        strings[id3v2_text_frames::talb] = vm["album"].as<string>();
      }
      if (vm.count("artist")) {
        strings[id3v2_text_frames::tpe1] = vm["artist"].as<string>();
      }
      if (vm.count("encoded-by")) {
        strings[id3v2_text_frames::tenc] = vm["encoded-by"].as<string>();
      }
      if (vm.count("genre")) {
        strings[id3v2_text_frames::tcon] = vm["genre"].as<string>();
      }
      if (vm.count("title")) {
        strings[id3v2_text_frames::tit2] = vm["title"].as<string>();
      }
      if (vm.count("track")) {
        strings[id3v2_text_frames::trck] = vm["track"].as<string>();
      }
      if (vm.count("year")) {
        strings[id3v2_text_frames::tyer] = vm["year"].as<string>();
      }

      idx_deque tags;
      if (vm.count("tag")) {
        vector<size_t> tmp = vm["tag"].as<vector<size_t>>();
        tags.insert(tags.begin(), tmp.begin(), tmp.end());
      } else {
        tags.push_back(0);
      }

      encoding srcenc = vm.count("encoding") ?
        vm["encoding"].as<encoding>() : encoding_from_system_locale();

      bool adj_unsync = vm["adjust-unsync" ].as<bool>();
      bool make_bu    = vm["create-backups"].as<bool>();

      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      // Convert all arguments to path-s here to catch invalid arguments
      // before we start processing.
      std::vector<fs::path> args;
      for (auto s: vm["arguments"].as<std::vector<string>>()) {
        args.push_back(fs::path(s));
      }

      ////////////////////////////////////////////////////////////////////
      //                       modify text frames                       //
      ////////////////////////////////////////////////////////////////////

      for (auto p: args) {
        process_dirent(p, tags, dels, strings, srcenc, make_bu, adj_unsync);
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

  register_command r("text", handle_text);

}
