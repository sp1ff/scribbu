/**
 * \file rename.cc
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/scribbu.hh>

#include <filesystem>
#include <fstream>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>

namespace fs = std::filesystem;
namespace po = boost::program_options;


namespace {

  const std::string USAGE(R"(scribbu rename -- rename .mp3 files

scribbu rename [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

Rename one or more files, and/or the files in one or more directories,
according to their ID3 tags. For detailed help, say `scribbu rename --help'.
To see the manual, say `info "scribbu (rename) "'.
)");

}


///////////////////////////////////////////////////////////////////////////////
//                          implementation                                   //
///////////////////////////////////////////////////////////////////////////////

namespace {

  class renamer {

  public:
    renamer(const std::string templat,
            const fs::path   &output,
            bool              dry_run,
            bool              rename,
            bool              verbose);

  public:

    void operator()(const fs::path &pth) const;

  private:

    void process_directory(const fs::path &pth) const;
    void process_file(const fs::path &pth) const;

  private:
    fs::path                    output_;
    scribbu::template_processor P_;
    bool                        dry_run_;
    bool                        rename_;
    bool                        verbose_;

  };

  renamer::renamer(const std::string templat,
                   const fs::path   &output,
                   bool              dry_run,
                   bool              rename,
                   bool              verbose):
    output_ (output                              ),
    P_      (scribbu::template_processor(templat)),
    dry_run_(dry_run                             ),
    rename_ (rename                              ),
    verbose_(verbose                             )
  { }

  void renamer::operator()(const fs::path &pth) const
  {
    if (! fs::exists(pth)) {
      throw std::invalid_argument(pth.string() + " does not appear to exist.");
    }

    if (fs::is_directory(pth)) {
      process_directory(pth);
    } else {
      process_file(pth);
    }

  }

  void renamer::process_file(const fs::path &pth) const
  {

    try {

      // Figure out the destination; if 'output' was given, all files will be
      // copied into that directory. Otherwise, they will be renamed in-place.
      fs::path out = output_.empty() ? pth.parent_path() : output_;
      out /= P_(pth);

      if (dry_run_ || verbose_) {
        std::cout << pth << " => " << out << std::endl;
      }
      if (! dry_run_ && pth != out) {
        fs::copy_file(pth, out);
        if (rename_) {
          fs::remove(pth);
        }
      }

    } catch (const scribbu::tbt_support::error &ex) {
      std::cerr << pth << ": " << ex.what() << std::endl;
    }

  }

  void renamer::process_directory(const fs::path &pth) const
  {
    const boost::regex REGEX(".*\\.mp3");

    std::for_each(fs::recursive_directory_iterator(pth),
                  fs::recursive_directory_iterator(),
                  [&](const fs::directory_entry &p) {
                    // the iterators dereference to a directory_entry
                    fs::path pth = p.path();
                    // Only process files ending in .mp3
                    if (fs::is_regular_file(pth) && boost::regex_match(pth.string(), REGEX)) {
                      process_file(pth);
                    }
                  });

  }

}


///////////////////////////////////////////////////////////////////////////////
//                             handler                                       //
///////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_rename(int argc, char **argv)
  {
    using namespace std;

    const std::string DEFAULT_TEMPLATE("%A - %T.mp3");
    const bool        DEFAULT_RENAME = false;
    const bool        DEFAULT_VERBOSE = false;

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
      ("dry-run,n", po::bool_switch(), "Dry-run; only print what would "
       "happen")
      ("output,o", po::value<fs::path>(), "If specified, copy the output files "
       "to this directory, rather than renaming in-place.")
      ("rename,r", po::bool_switch()->default_value(DEFAULT_RENAME),
       "Remove the source file (ignored if --dry-run is given)")
      ("template,t", po::value<std::string>()->default_value(DEFAULT_TEMPLATE),
       "Template by which to rename the files.")
      ("verbose,v", po::bool_switch()->default_value(DEFAULT_VERBOSE),
       "Produce verbose output.");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-rename",
                        "(scribbu) Invoking scribbu rename");

      po::store(parsed, vm);

      const map<string, string> ENV_OPTS {
        make_pair("SCRIBBU_DRY_RUN", "dry-run"),
        make_pair("SCRIBBU_OUTPUT", "output"),
        make_pair("SCRIBBU_VERBOSE", "verbose"),
      };
      parsed = po::parse_environment(opts, [&ENV_OPTS](const string &var) {
        auto p = ENV_OPTS.find(var);
        return ENV_OPTS.end() == p ? "" : p->second.c_str();
      });
      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...

      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      std::vector<fs::path> arguments;
      for (auto s: vm["arguments"].as<std::vector<string>>()) {
        arguments.push_back(fs::path(s));
      }

      bool dry_run = vm["dry-run"].as<bool>();
      fs::path output;
      if (vm.count("output")) {
        output = vm["output"].as<fs::path>();
      }
      std::string templat = vm["template"].as<std::string>();
      bool rename = vm["rename"].as<bool>();
      bool verbose = vm["verbose"].as<bool>();

      std::unique_ptr<renamer> pr(new renamer(templat, output, dry_run,
                                              rename, verbose));

      std::for_each(arguments.begin(), arguments.end(), std::ref(*pr));

    } catch (const po::error &ex) {

      cerr << ex.what() << endl;
      print_usage(cerr, docopts, USAGE);
      status = EXIT_INCORRECT_USAGE;

    } catch (const std::exception &ex) {

      cerr << ex.what() << endl;
      status = EXIT_FAILURE;

    }

    return status;

  } // End free function handle_rename.

  register_command r("rename", handle_rename);

}
