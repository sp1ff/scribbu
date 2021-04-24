/**
 * \file encodings.cc
 *
 * Copyright (C) 2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/charsets.hh>

namespace po = boost::program_options;

const std::string USAGE(R"usage(scribbu encodings -- list all supported character encodings

scribbu encodings [OPTION...]

Several scribbu sub-commands take character encodings as arguments. It is not
always clear how to specify them textually. This sub-command will print the
list of supported encodings along with their textual representations.

Summary of options:

  -h              display this usage message
  --help, --man   display this sub-command's man page instead of listing the
                  encodings to stdout
  --info          display the corresponding Info node instead of listing the 
                  encodings to stdout

For detailed help, say `scribbu encodings --help'. To see the manual, say
`info "scribbu (encodings)"'.
)usage");

namespace {

  /**
   * \brief `encodings' sub-command handler
   *
   * \sa handler_type
   * \sa register_command
   *
   *
   * `scribbu encodings' is a sub-command for listing the textual
   * representations of the character encodings supported by scribbu.
   *
   *
   */

  int handle_encodings(int argc, char **argv)
  {
    using namespace std;
    using namespace scribbu;

    int status = EXIT_SUCCESS;

    // Options-parsing is considerably simpler for this sub-command.
    po::options_description opts("options");
    opts.add_options()
      ("help,h", "Display help & exit; `--help' will display"
       "the man page for this sub-command & `-h' will display this sub-"
       "command's usage message")
      ("info", po::bool_switch(), "Display this sub-command's node in the "
       "scribbu Info manual")
      ("man", po::bool_switch(), "Display the man page for this sub-command");

    try {

      vector<string> tokens;
      convert_tokens(argc, argv, back_inserter(tokens));

      po::variables_map vm;
      po::parsed_options parsed = po::command_line_parser(tokens).
        options(opts).
        run();

      maybe_handle_help(parsed, opts, USAGE, "scribbu-encodings",
                        "(scribbu) Invoking scribbu encodings");

      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      for (int i = static_cast<int>(encoding::ASCII);
           i < static_cast<int>(encoding::MAX_ENCODING);
           ++i) {
        cout << static_cast<encoding>(i) << "\n";
      }

    } catch (const po::error &ex) {
      cerr << ex.what() << endl;
      print_usage(cerr, opts, USAGE);
      status = EXIT_INCORRECT_USAGE;
    } catch (const std::exception &ex) {
      cerr << ex.what() << endl;
      status = EXIT_FAILURE;
    }

    return status;
  }

  register_command r("encodings", handle_encodings);
}

