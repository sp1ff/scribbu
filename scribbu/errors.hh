/**
 * \file errors.hh
 *
 * Copyright (C) 2015-2018 Michael Herstine <sp1ff@pobox.com>
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

#ifndef ERRORS_H_INCLUDED
#define ERRORS_H_INCLUDED 1

#include <exception>
#include <boost/exception/all.hpp>

namespace scribbu {

  /**
   * \class error
   *
   * \brief Base class for all scribbu errors
   *
   *
   * This class was designed in accordance with the practices outlined in
   * \ref * error_refs_1 "[1]" using boost::exception \ref error_refs_2 "[2]". There's
   * also a nice discussion in \ref error_refs_3 "[3]".
   *
   *
   * - \anchor error_refs_1 "Error and Exception Handling", retrieved March 18,
   *   2016 from http://www.boost.org/community/error_handling.html
   *
   * - \anchor error_refs_2 "Boost Exception", retrieved March 18, 2016 from
   *   http://www.boost.org/doc/libs/1_57_0/libs/exception/doc/boost-exception.html
   *
   * - \anchor error_refs_3 "How to design exception “types” in C++?",
   *   retrieved March 18, 2016 from
   *   http://stackoverflow.com/questions/9387377/how-to-design-exception-types-in-c
   *
   *
   */

  class error: public virtual boost::exception,
               public virtual std::exception
  {
  public:
    error()
    { }
  };

} // End namespace scribbu.

#endif // not ERRORS_H_INCLUDED
