/**
 * \file split.cc
 *
 * Copyright (C) 2015-2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

namespace fs = boost::filesystem;
namespace po = boost::program_options;


namespace {

  const std::string USAGE(R"(scribbu split -- split a file into ID3v2, track data, and ID3v1
tag

scribbu split [OPTION...] INPUT

Split a file into it's ID3v2 tag, track data, and\or ID3v1 tag. The input files
will be named "id3v2NS", N=0,1,2,..., "trackS" & "id3v1S", where S is the
--suffix option, if given.

For detailed help, say `scribbu split --help'. To see the manual, say
`info "scribbu (split) "'.
)");

  void xfer_bytes(std::istream   &in,
                  const fs::path &pth,
                  std::size_t     cb)
  {
    const std::ios::iostate EXC_MASK = std::ios::eofbit | std::ios::failbit |
      std::ios::badbit;

    fs::ofstream ofs(pth, std::istream::binary);
    ofs.exceptions(EXC_MASK);

    std::unique_ptr<char[]> p(new char[cb]);
    in.read(p.get(), cb);
    ofs.write(p.get(), cb);
  }

  /**
   * \brief Split a file into ID3v2 tag, track data, and/or ID3v1 tag
   *
   *
   * - read the first ten bytes
   *
   * - if the file has an ID3v2 tag, decode the size & write the first
   *    ten bytes along with the rest of the tag to "id3v2" + \a suffix
   *
   * - else, write the first ten bytes to "track" + \a suffix
   *
   * - examine the file contents beginning at 355 bytes & 128 bytes
   *   from the end
   *
   * - if the file has an ID3v1 tag, write the intervening data to
   *   "track" + \a suffix and write the remainder to "id3v1" +
   *   \a suffix
   *
   * - else write the data from the current location to the end of the
   *   file to "track" + \a suffix
   *
   *
   */

  void split_file(const fs::path    &in,
                  const std::string &suffix)
  {
    const std::ios::iostate EXC_MASK = std::ios::eofbit | std::ios::failbit | std::ios::badbit;

    fs::ifstream ifs(in, fs::ifstream::binary);
    ifs.exceptions(EXC_MASK);

    scribbu::id3v2_info id3v2 = scribbu::looking_at_id3v2(ifs);

    std::size_t count = 0;
    while (id3v2.present_) {
      std::stringstream stm;
      stm << "id3v2" << count << suffix;
      ++count;
      fs::path pth(stm.str());
      xfer_bytes(ifs, pth, 10 + id3v2.size_);
      // Another?
      id3v2 = scribbu::looking_at_id3v2(ifs);
    }

    // Either way, we're at the start of the track data...
    scribbu::id3v1_info I = scribbu::ends_in_id3v1(ifs);
    std::size_t cb = I.start_ - ifs.tellg();
    fs::path pth("track" + suffix);
    xfer_bytes(ifs, pth, cb);

    if (I.start_ != I.end_) {
      fs::path pth("id3v1" + suffix);
      xfer_bytes(ifs, pth, I.end_ - I.start_);
    }

  } // End free function split_file.

}


////////////////////////////////////////////////////////////////////////////////
//                             handler                                        //
////////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_split(int argc, char **argv)
  {
    using namespace std;

    using boost::optional;

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
    //                cli, cfg, |  opts  |  xopts  |                           //
    //                & env     +--------+---------+                           //
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
      ("suffix", po::value<std::string>()->default_value(std::string()),
       "Optional suffix to be appended to all generated files");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("argument", po::value<std::string>()->required(), "input file");

    po::options_description docopts;
    docopts.add(clopts).add(opts);

    po::options_description nocli;
    nocli.add(opts).add(xopts);

    po::options_description all;
    all.add(clopts).add(xclopts).add(opts).add(xopts);

    po::positional_options_description popts;
    popts.add("argument", 1);

    try {

      vector<string> tokens;
      convert_tokens(argc, argv, back_inserter(tokens));

      po::variables_map vm;
      po::parsed_options parsed = po::command_line_parser(tokens).
        options(all).
        positional(popts).
        run();

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-split",
                        "(scribbu) Invoking scribbu split");

      po::store(parsed, vm);

      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      po::notify(vm);
      split_file(fs::path(vm["argument"].as<std::string>()),
                 vm["suffix"].as<std::string>());

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

  register_command r("split", handle_split);

}
