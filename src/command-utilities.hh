/**
 * \file command-utilities.hh
 *
 * \brief Assorted utilities for scribbu & its sub-commands
 *
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#ifndef COMMAND_UTILITIES_HH_INCLUDED
#define COMMAND_UTILITIES_HH_INCLUDED 1

#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/optional/optional.hpp>
#include <boost/program_options.hpp>


/// Like EXIT_SUCCESS & EXIT_FAILURE, but reflecting the convention that
/// calling with incorrect parameters shall exit with status code 2
const int EXIT_INCORRECT_USAGE = 2;


////////////////////////////////////////////////////////////////////////////
//                      types & methods related to help                   //
////////////////////////////////////////////////////////////////////////////

/// help can be requested at three levels
enum class help_level {
  none, regular, verbose
};

/// at level verbose, help can be requested as man or info
enum class verbose_flavor {
  man, info
};

/// Extract help_level from a set of parsed_options-- this has to be done
/// on a set of parsed_options because that's the only time we can get
/// to the raw tokens (e.g. to distinguish between "-h" & "--help").
std::tuple<help_level, boost::optional<verbose_flavor>>
help_level_for_parsed_opts(const boost::program_options::parsed_options &opts);

/// Print a usage message-- this is help ast level `regular'
void
print_usage(std::ostream                              &os,
    const boost::program_options::options_description &opts,
    const std::string                                 &usage);

void
show_man_page(const std::string &page);

void
show_info_node(const std::string &node);

/**
 * \brief Provide stock handling for help requests
 *
 *
 * \param opts [in] const reference to parseed_options (this has to be done on a
 * set of parsed_options because that's the only time we can get to the raw
 * tokens (e.g. to distinguish between "-h" & "--help")
 *
 * \param page [in] name of this (sub-)command's man page
 *
 * \param node [in] name of this (sub-)command's info node
 *
 *
 * If this function returns, help was not requested-- the implementation
 * should carry out it's work. If help was requested at any level, this
 * function will never return.
 *
 *
 */

void
maybe_handle_help(const boost::program_options::parsed_options &opts,
    const boost::program_options::options_description          &dsc,
    const std::string                                          &usage,
    const std::string                                          &page,
    const std::string                                          &node);

////////////////////////////////////////////////////////////////////////////
//                           sub-command utilities                        //
////////////////////////////////////////////////////////////////////////////

/// Convenience typedef for a sub-command implementation
typedef
std::function<int(int argc, char **argv)>
handler_type;

/**
 * \brief Trivial struct for use as a static initializer
 *
 *
 * scribbu commands, having implemented their logic in the form of a
 * handler_type, can register themselves by constructing an instance
 * of this class like so:
 *
 \code

 namespace {

   int handler(blahblahblah...) {
     ...
   }

   register_command("<sub-command name>", handler);

 }
 \endcode
 *
 *
 */

struct register_command
{
  register_command(const std::string &s, handler_type f);
};

/// Return true if a sub-command named \a s has been registered
bool has_sub_command(const char*);

/// Retrieve a sub-command my name
handler_type get_sub_command(const char *);

namespace detail {
  typedef std::map<std::string, handler_type> handler_map;
  handler_map& get_handler_map();
}

/// Retrieve the names of all sub-commands
template <typename forward_output_iterator>
forward_output_iterator
get_sub_command_names(forward_output_iterator pout)
{
  const detail::handler_map &H = detail::get_handler_map();

  for (auto x: H) {
    *pout++ = x.first;
  }

  return pout;
}

/// Convert (argc, argv) style parameters to a collection of tokens
template <typename forward_output_iterator>
forward_output_iterator convert_tokens(int argc,
                                       char **argv,
                                       forward_output_iterator pout,
                                       bool skip_first = false)
{
  while (argc) {
    if (skip_first) {
      skip_first = false;
    } else {
      *pout++ = std::string(argv[0]);
    }
    --argc; ++argv;
  }
  return pout;
}

#endif // not COMMAND_UTILITIES_HH_INCLUDED
