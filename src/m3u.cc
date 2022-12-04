/**
 * \file m3u.cc
 *
 * Copyright (C) 2021-2022 Michael Herstine <sp1ff@pobox.com>
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

#include <iostream>

#include <boost/optional.hpp>

#include <scribbu/id3v2-utils.hh>
#include <scribbu/mp3.hh>
#include <scribbu/scribbu.hh>

namespace fs = std::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"usage(scribbu m3u -- M3U playlists from one or more files

scribbu m3u [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, write extended M3U entries to stdout for all the files listed on the
command line. If the argument is a directory, do so for all .mp3 files
discovered recursively (the order of which is unspecified).

An extended M3U entry takes the form:

    # EXINF:<duration-in-seconds>,<display title>
    <path-to-file>

The display title will be "Artist - Title" if those two items can be derived
from ID3 tags; else the file basename will be used. The path will be relative or
absolute, according to the argument (i.e. specifying an absolute path to a
directory will produce absolute paths, a relative path to a directory relative
paths in the output).

Output to stdout:

When writing to stdout, the file paths & display titles will be written in the
system locale's text encoding scheme. The text forming the artist & title tags
will also be assumed to be in the system locale's encoding. To override this,
specify the -s flag (for source encoding).

Output to file:

If the -o option is given, the output will be written to the named file. By
convention, files ending in .m3u8 are UTF-8 encoded and files ending in .m3u
are written in an unspecified encoding. Given that M3U is a de facto standard,
scribbu does not enforce this (or any other naming convention).

In this case, a new file will be created with the #EXTM3U header line, unless
the -a (append) flag is given, in which case the output will be appended to an
existing file.

By default, paths will be written in the system locale's encoding (the same
encoding in which they will be read). To force utf-8 output, specify the -8
flag.

The text forming the artist & title tags will also be assumed to be in
the system locale. To override this, specify the -s flag (for source
encoding). On output, they will be written in the system locale's encoding,
again unless the -8 flag is given.

So, for instance, if your system locale's encoding is ISO-8859-1, and your
tags are written in, say, Windows Code Page 1251, but you would like an M3U
playlist in UTF-8 format, say:

  scribbu m3u -s CP1251 -o test.m3u8 -8 some-directory/

Summary of options:

  -s SRC, --source-encoding=SRC Specify the text encoding in which textual
                                ID3 tags like artist & title are written
  -o OUT, --output=OUT          Specify that output be written to file OUT
  -a, --append                  Append, don't overwrite OUT
  -8, --use-utf-8               Write the output in utf-8, not the system
                                locale's encoding
  -v, --verbose                 Produce more verbose output

For detailed help, say `scribbu m3u --help'. To see the manual, say
`info "scribbu (m3u)"'.
)usage");

namespace {

  //////////////////////////////////////////////////////////////////////////////
  //                                 class m3u                                //
  //////////////////////////////////////////////////////////////////////////////

  /// Core implementation
  class m3u
  {
  public:
    m3u(std::ostream& os, boost::optional<scribbu::encoding> src_enc, bool verbose,
        scribbu::encoding dst_enc):
      os_(os), src_enc_(src_enc), dst_enc_(dst_enc), verbose_(verbose)
    { }

  public:
    void process_file(const fs::path &pth);
    bool verbose() const
    { return verbose_ ; }


  protected:
    template <typename forward_input_iterator>
    boost::optional<std::string>
    guess_display_title(forward_input_iterator    ptagv2_0,
                        forward_input_iterator    ptagv2_1,
                        const scribbu::id3v1_tag *ptagv1);

  private:
    std::ostream &os_;
    boost::optional<scribbu::encoding> src_enc_;
    scribbu::encoding dst_enc_;
    bool verbose_;
  };

  template <typename forward_input_iterator>
  boost::optional<std::string>
  m3u::guess_display_title(forward_input_iterator    ptagv2_0,
                           forward_input_iterator    ptagv2_1,
                           const scribbu::id3v1_tag *ptagv1)
  {
    using namespace std;
    using namespace scribbu;

    forward_input_iterator p =
      find_if(ptagv2_0, ptagv2_1,
              [](const unique_ptr<id3v2_tag>& ptag) {
                return ptag->has_artist() && ptag->has_title();
              });

    if (p != ptagv2_1) {

      string a = (*p)->artist(dst_enc_, on_no_encoding::fail, src_enc_);;
      string t = (*p)->title(dst_enc_, on_no_encoding::fail, src_enc_);

      if (!a.empty() && !t.empty()) {
        return a + " - " + t;
      }
    }

    if (ptagv1) {
      string a = ptagv1->artist<string>(src_enc_, dst_enc_);
      string t = ptagv1->title<string>(src_enc_, dst_enc_);

        if (!a.empty() && !t.empty()) {
          return a + " - " + t;
        }
    }

    return boost::none;

  }

  void
  m3u::process_file(const fs::path &pth)
  {
    using namespace std;
    using namespace scribbu;

    ifstream ifs = open_ifstream(pth.native(), ios_base::binary);
    vector<unique_ptr<id3v2_tag>> id3v2;
    read_all_id3v2(ifs, back_inserter(id3v2));
    double secs = get_mp3_duration(ifs);
    unique_ptr<id3v1_tag> pid3v1 = process_id3v1(ifs);

    string display;
    boost::optional<string> gdt;
    if (gdt = guess_display_title(id3v2.begin(), id3v2.end(), pid3v1.get())) {
      display = *gdt;
    } else {
      display = pth.stem().native();
    }

    os_ << "#EXTINF:" << (int) (secs + 0.5) << "," << display << "\n" <<
      pth.native() << endl;
  }

  //////////////////////////////////////////////////////////////////////////////
  //                          class m3u_to_stdout                             //
  //////////////////////////////////////////////////////////////////////////////

  /// Write M3U entries to stdout
  class m3u_to_stdout: public m3u
  {
  public:
    m3u_to_stdout(boost::optional<scribbu::encoding> src_enc, bool verbose);

  };

  m3u_to_stdout::m3u_to_stdout(boost::optional<scribbu::encoding> src_enc, bool verbose):
    m3u(std::cout, src_enc, verbose, scribbu::encoding_from_system_locale())
  { }

  //////////////////////////////////////////////////////////////////////////////
  //                             class m3u_to_file                            //
  //////////////////////////////////////////////////////////////////////////////

  /// Write M3u entries to file
  class m3u_to_file: public m3u
  {
  public:
    m3u_to_file(boost::optional<scribbu::encoding> src_enc,
                bool verbose,
                const fs::path   &out,
                bool              append,
                bool              use_utf8);

  private:
    std::ofstream ofs_;

  };

  m3u_to_file::m3u_to_file(boost::optional<scribbu::encoding> src_enc,
                           bool verbose,
                           const fs::path   &out,
                           bool              append,
                           bool              use_utf8):
    m3u(ofs_, src_enc, verbose,
        use_utf8 ? scribbu::encoding::UTF_8 : scribbu::encoding_from_system_locale())
  {
    using namespace std;

    ios_base::openmode mode = ios_base::out;
    mode |= append ? ios_base::app : ios_base::trunc;

    ofs_.open(out, mode);
    if (!ofs_) {
      throw scribbu::file_open_error(out.native(), errno);
    }

    ofs_ << "#EXTM3U" << endl;

  }

  /**
   * \brief `m3u' sub-command handler
   *
   * \sa handler_type
   * \sa register_command
   *
   *
   * `scribbu m3u' is a sub-command that will produce EXTM3U entries for its
   * arguments, either to stdout or to file. Presently it only supports the
   * #EXTM3U & #EXTINF directives.
   *
   *
   */

  int
  handle_m3u(int argc, char **argv)
  {
    using namespace std;
    using namespace scribbu;

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
      ("help,h", po::bool_switch(), "Display help & exit; `--help' will display"
       "the man page for this sub-command & `-h' will display this sub-"
       "command's usage message")
      ("info", po::bool_switch(), "Display this sub-command's node in the "
       "scribbu Info manual");

    po::options_description xclopts("command-line only developer options");
    xclopts.add_options()
      ("man", po::bool_switch(), "Display the man page for this sub-command");

    po::options_description opts("general options");
    opts.add_options()
      ("source-encoding,s", po::value<encoding>()->default_value(encoding::CP1252),
       "specify the text encoding in which textual ID3 tags like artist & title "
       "are written")
      ("output,o", po::value<fs::path>(), "specify that output be written to "
       "file instead of stdout")
      ("append,a", po::bool_switch(), "append to instead of overwriting the "
       "output file (only applies if -o is given)")
      ("use-utf8,8", po::bool_switch(), "write the output in utf-8, not the "
       "system locale's encoding (only applies if -o is given)")
      ("verbose,v", po::bool_switch(), "produce more verbose output");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>(), "one or more "
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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-m3u",
                        "(scribbu) Invoking scribbu m3u");

      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      //////////////////////////////////////////////////////////////////////////
      //                         parsing arguments                            //
      //////////////////////////////////////////////////////////////////////////

      boost::optional<encoding> src_enc = boost::none;
      if (vm.count("encoding")) {
        src_enc = vm["encoding"].as<encoding>();
      }
      fs::path out = vm.count("output") ?
        vm["output"].as<fs::path>() : fs::path();
      bool verbose = vm["verbose"].as<bool>();
      bool append = vm["append"].as<bool>();
      if (append && out.empty()) {
        throw po::error("--append is only relevant when --output is given");
      }
      bool use_utf8 = vm["use-utf8"].as<bool>();
      if (use_utf8 && out.empty()) {
        throw po::error("--use-utf8 is only relevant when --output is given");
      }

      // Workaround to https://svn.boost.org/trac/boost/ticket/8535
      std::vector<fs::path> args;
      for (auto s: vm["arguments"].as<std::vector<string>>()) {
        args.push_back(fs::path(s));
      }

      if (out.empty()) {
        m3u_to_stdout F(src_enc, verbose);
        process_dirent_args(args.begin(), args.end(), F);
      } else {
        m3u_to_file F(src_enc, verbose, out, append, use_utf8);
        process_dirent_args(args.begin(), args.end(), F);
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

  register_command r("m3u", handle_m3u);

}
