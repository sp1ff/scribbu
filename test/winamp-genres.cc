/**
 * \file winamp-genres.cc
 *
 * Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/winamp-genres.hh>

#include "unit.hh"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( smoke_test_match_winamp_genre )
{
  using namespace std;
  using namespace scribbu;

  string text;
  unsigned char genre;
  size_t dl;
  tie(text, genre, dl) = match_winamp_genre("Blues");
  BOOST_CHECK(string("Blues") == text);
  BOOST_CHECK(0 == genre);
  BOOST_CHECK(0 == dl);

  tie(text, genre, dl) = match_winamp_genre("blus");
  BOOST_CHECK(string("Blues") == text);
  BOOST_CHECK(0 == genre);
  BOOST_CHECK(1 == dl);

  // renders as Bluês using U+00EA (LATIN SMALL LETTER E WITH CIRCUMFLEX)
  tie(text, genre, dl) = match_winamp_genre("blu\u00eas");
  BOOST_CHECK(string("Blues") == text);
  BOOST_CHECK(0 == genre);
  BOOST_CHECK(1 == dl);

  // renders as Bluês using 0065;LATIN SMALL LETTER E & 0302;COMBINING CIRCUMFLEX ACCENT
  tie(text, genre, dl) = match_winamp_genre("Blu\u0065\u0302s");
  BOOST_CHECK(string("Blues") == text);
  BOOST_CHECK(0 == genre);
  BOOST_CHECK(1 == dl);

  tie(text, genre, dl) = match_winamp_genre("Progresiev rock");
  BOOST_CHECK(string("Progressive Rock") == text);
  BOOST_CHECK(92 == genre);
  BOOST_CHECK(2 == dl);
}
