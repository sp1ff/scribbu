/**
 * \file errors.hh
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
   * A few guidelines for how I like to use C++ exceptions:
   *
   * 1. application- or library-defined exception classes should subclass
   *    (directly or indirectly) std::exception so that their consumer can catch
   *    everything with a "catch (const std::exception&)" instead of having to do
   *    "catch (...)" (see \ref error_refs_1 "[1]" & \ref error_refs_3 "[3]" for
   *    more discussion)
   *
   * 2. furthermore, they should inherit std::exception virtually to prevent
   *    ambiguity at the catch site in the event that the exception thrown
   *    inherits multiply from bases that have a common ancestor (see
   *    \ref error_refs_1 "[1]" for an example of this)
   *
   * 3. this concern (MI) is salient here because I like to use boost::exception,
   *    primarily so as to be able to "attach" context on the way up the call
   *    stack to provide more information on what went wrong
   *
   * 4. don't embed a string in your exception type; in fact keep it as lean as
   *    possible to prevent the ctor from throwing when you construct your
   *    exception type (which could lead to std::terminate being called, or a
   *    different sort of exception being thrown than the one you intended)
   *
   * 5. pursuant to that, format the "what()" message lazily on demand; also,
   *    don't make consumers depend solely on the text: expose the pertinent
   *    information programattically as well
   *
   * \ref error_refs_3 "[3]" argues that you should root your exceptions at
   * std::runtime_error, because "It is the standard exception class that
   * supports custom messages (the others generally have hardcoded messages that
   * one preferably should not change, since they have value in being
   * recognizable)." and "because is the standard exception class that
   * represents recoverable failures (as opposed to unrecoverable logic errors,
   * which can’t be fixed at run time)." I find neither of these arguments
   * persuasive; std::logic_error also provides for a custom error message, and
   * also represents errors that are recoverable (std::length_error,
   * e.g.). Therefore I choose to root mine at std::exception.
   *
   * 6. prefer to throw exceptions derived from scribbu::error; they use
   *    the boost::exception context mechanism
   *
   * 7. sometimes I get lazy and throw logic_error or runtime_error subclasses,
   *    when they make sense (invalid_argument, e.g.)
   *
   * 8. do not throw logic_error or runtime_error directly; they violate 5 & 6,
   *    above
   *
   *
   * - \anchor error_refs_1 [1] unknown. Error and Exception Handling
   *   http://www.boost.org/community/error_handling.html (updated March 18,
   *   2016)
   *
   * - \anchor error_refs_2 [2] unknown. Boost Exception
   *   http://www.boost.org/doc/libs/1_57_0/libs/exception/doc/boost-exception.html
   *   (updated March 18, 2016)
   *
   * - \anchor error_refs_3 [3] multiple authors. "How to design exception
   *   'types' in C++?"
   *   http://stackoverflow.com/questions/9387377/how-to-design-exception-types-in-c++
   *   (updated March 18, 2016)
   *
   *
   */

  struct error: public virtual boost::exception,
                public virtual std::exception
  {
    error()
    { }
    virtual const char * what() const noexcept(true) = 0;
  };

} // End namespace scribbu.

#endif // not ERRORS_H_INCLUDED
