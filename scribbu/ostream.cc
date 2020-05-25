/**
 * \file ostream.cc
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

#include "ostream.hh"

///////////////////////////////////////////////////////////////////////////////
//                                class ienc                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ int
scribbu::ienc::index()
{
  static int idx = std::ios_base::xalloc();
  return idx;
}

/*static*/ void
scribbu::ienc::insert(const ienc &manip, std::ios_base &ios)
{
  int idx = index();

  // Retrieve the current pword value...
  void *&p = ios.pword(idx);
  // if it's null, then this stream has not yet been imbued with an
  // internal encoding; register our callback.
  if (nullptr == p) {
    ios.register_callback(callback, idx);
  }
  // nil or otherwise, regard this as the "old" encoding &
  // make a copy..
  ienc *pold = reinterpret_cast<ienc*>(p);
  // copy the instance in `manip' *into* the stream...
  p = new ienc(manip.enc_, manip.rsp_);
  // and, finally, clean up the old string.
  delete pold;
}

/*static*/ scribbu::ienc
scribbu::ienc::retrieve(std::ios_base &ios)
{
  int idx = index();

  // Retrieve the current pword value...
  void *&p = ios.pword(idx);
  // if it's null, then this stream has not yet been imbued with an
  // internal encoding; register our callback.
  if (nullptr == p) {
    return ienc(encoding::UTF_8, on_no_encoding::fail);
  }
  else {
    return *(reinterpret_cast<ienc*>(p));
  }

}

/*static*/ void
scribbu::ienc::callback(std::ios_base::event e, std::ios_base &ios, int idx)
{
  using namespace std;

  if (ios_base::erase_event == e) {
    try {
      delete reinterpret_cast<char*>(ios.pword(idx));
    }
    catch (...) {
      // Nothing to be done-- either the stream is being destroyed, or
      // we're at the beginning of a copyfmt call. In the former case
      // there'll be no one to read the iword, and the latter it will
      // be overwritten before we can read it, anyway.
    }
  }
  else if (ios_base::copyfmt_event == e) {
    void *&p = ios.pword(idx);
    char *pold = reinterpret_cast<char*>(p);
    if (nullptr != pold) {
      try {
        p = strdup(pold);
      }
      catch (...) {
        // We can't throw, and we have no return code. The only thing
        // we can do is use the iword:
        int old = ios.iword(idx);
        ios.iword(idx) = old | ios_base::badbit;
      }
    }
  }
}

namespace scribbu {

  template <>
  std::basic_ostream<char, std::char_traits<char>>&
  operator<<(std::basic_ostream<char, std::char_traits<char>> &os,
             const ienc &x) {

    using namespace std;

    typedef detail::trivial_sentry<char, std::char_traits<char>> sentry_type;
    typedef manip_helper<const ienc, ostream, sentry_type>  impl_type;

    if (!scribbu::char_traits<char>::is_code_unit(x.get_encoding())) {
      throw bad_code_unit(x.get_encoding(), sizeof(char));
    }

    impl_type::do_manip(x, &ienc::insert, os);
    return os;

  }

  template <>
  std::basic_ostream<wchar_t, std::char_traits<wchar_t>>&
  operator<<(std::basic_ostream<wchar_t, std::char_traits<wchar_t>> &os,
             const ienc &x) {

    using namespace std;

    typedef detail::trivial_sentry<wchar_t, std::char_traits<wchar_t>> sentry_type;
    typedef manip_helper<const ienc, wostream, sentry_type>  impl_type;

    if (!scribbu::char_traits<wchar_t>::is_code_unit(x.get_encoding())) {
      throw bad_code_unit(x.get_encoding(), sizeof(wchar_t));
    }

    impl_type::do_manip(x, &ienc::insert, os);
    return os;

  }

  template <>
  std::basic_istream<char, std::char_traits<char>>&
  operator>>(std::basic_istream<char, std::char_traits<char>> &is,
             const ienc &x) {

    using namespace std;

    typedef detail::trivial_sentry<char, std::char_traits<char>> sentry_type;
    typedef manip_helper<const ienc, istream, sentry_type>  impl_type;

    impl_type::do_manip(x, &ienc::insert, is);
    return is;

  }

  template <>
  std::basic_istream<wchar_t, std::char_traits<wchar_t>>&
  operator>>(std::basic_istream<wchar_t, std::char_traits<wchar_t>> &is,
             const ienc &x) {

    using namespace std;

    typedef detail::trivial_sentry<wchar_t, std::char_traits<wchar_t>> sentry_type;
    typedef manip_helper<const ienc, wistream, sentry_type>       impl_type;

    impl_type::do_manip(x, &ienc::insert, is);

    return is;

  }

}
