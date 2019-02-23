/**
 * \file command-utilities.cc
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

#include "command-utilities.hh"

#include <sstream>
#include <unordered_map>

#include <boost/algorithm/string/replace.hpp>


namespace po = boost::program_options;


/// Extract help_level from a set of parsed_options
help_level
help_level_for_parsed_opts(const po::parsed_options &opts)
{
  using namespace std;

  help_level out = help_level::none;
  
  for (auto o: opts.options) {
    for (auto t: o.original_tokens) {
      if (t == "-h") {
        out = help_level::regular;
      } else if (t == "--help") {
        out = help_level::verbose;
      }
    }
  }
  
  return out;
  
}

namespace {

  typedef std::unordered_map<std::string, handler_type> handler_map;

  handler_map& get_handler_map()
  {
    static handler_map the_map;
    return the_map;
  }

}

register_command::register_command(const std::string &s, handler_type f)
{
  handler_map &M = get_handler_map();
  if (M.count(s)) {
    throw std::out_of_range(s);
  }
  M[s] = f;
}

bool
has_sub_command(const char *s)
{
  return 0 != get_handler_map().count(s);
}

handler_type get_sub_command(const char *s)
{
  return get_handler_map().at(s);
}

void
print_usage(std::ostream                  &os,
            const po::options_description &opts,
            const std::string             &usage)
{
  using namespace std;

  // Workaround for https://svn.boost.org/trac10/ticket/4644; marked as fixed
  // some time ago, but still happening...
  stringstream stm4644;
  stm4644 << opts;
  string str4644 = stm4644.str();
  boost::algorithm::replace_all(str4644, "--h ", "-h  ");
  os << usage << str4644  << std::endl;
}

void
show_man_page(const std::string &page)
{
  using namespace std;

  execlp("man", "man", page.c_str(), (char *)NULL);
  // If we're here, `execlp' failed.
  stringstream stm;
  stm << "Failed to exec man: [" << errno << "]: " << strerror(errno);
  throw runtime_error(stm.str());
}

