/**
 * \file command-utilities.hh
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
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


enum class help_level {
  none, regular, verbose
};

/// Convenience typedef for a sub-command implementation
typedef
std::function<int(const std::vector<std::string> 		 &opts,
                  help_level                     		  help,
                  const boost::optional<boost::filesystem::path> &cfg)>
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

   int handler(vector<string> &tokens, help_level help, const optional<path> &cfg)
   {
     ...
   }

   register_command("<sub-command name>", handler);

 \endcode
 *
 *
 */

struct register_command
{
  register_command(const std::string &s, handler_type f);
};

/// Return true if a sub-command named \a s has been registered
bool has_sub_command(const std::string &s);

/// Retrieve a sub-command my bame
handler_type get_sub_command(const std::string &s);

void
print_usage(std::ostream                                      &os,
	    const boost::program_options::options_description &opts,
	    const std::string                                 &usage);

#endif // not COMMAND_UTILITIES_HH_INCLUDED
