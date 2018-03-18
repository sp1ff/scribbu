/**
 * \file framesv22.cc
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

#include <scribbu/framesv22.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_unknown_id3v2_2_frame )
{
  using scribbu::frame_id3;
  using scribbu::unknown_id3v2_2_frame;

  const unsigned char DATA[] = {
    0xba, 0xbe, 0xba, 0xbe
  };

  unknown_id3v2_2_frame F("XXX", DATA, DATA + sizeof(DATA));

  BOOST_CHECK( frame_id3("XXX") == F.id() );

  unsigned char x[4];
  BOOST_CHECK( x + 4 == F.data(x) );
  BOOST_CHECK( 0 == memcmp(DATA, x, sizeof(DATA)) );

} // End test_id3v2_2_frame.

BOOST_AUTO_TEST_CASE( test_com )
{
  using scribbu::comments;
  using scribbu::frame_id3;
  using scribbu::COM;

  const unsigned char DATA[] = {
    00,                               // Unicode
    'e', 'n', 'g',                    // Language
    'f', 'o', 'o', 0,                 // short description
    'b', 'a', 'r'
  };

  COM C(DATA, DATA + sizeof(DATA));

  BOOST_CHECK( frame_id3("COM") == C.id() );

  BOOST_CHECK( 0 == C.unicode() );

  unsigned char buf[3];
  BOOST_CHECK( buf + 3 == C.lang(buf) );
  BOOST_CHECK( 'e' == buf[0] && 'n' == buf[1] && 'g' == buf[2] );

  BOOST_CHECK( buf + 3 == C.descriptionb(buf) );
  BOOST_CHECK( 'f' == buf[0] && 'o' == buf[1] && 'o' == buf[2] );

  BOOST_CHECK( buf + 3 == C.textb(buf) );
  BOOST_CHECK( 'b' == buf[0] && 'a' == buf[1] && 'r' == buf[2] );

} // End test_com.
