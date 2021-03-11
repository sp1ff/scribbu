/**
 * \file framesv24.cc
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

#include <scribbu/framesv24.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_encr_2_4 )
{
  using scribbu::encryption_method;
  using scribbu::id3v2_3_plus_frame;
  using scribbu::ENCR_2_4;

  const unsigned char DATA[] = {
    'm', 'e', 0,
    1,
    1, 2, 3
  };

  ENCR_2_4 encr(DATA, DATA + sizeof(DATA),
                id3v2_3_plus_frame::tag_alter_preservation::preserve,
                id3v2_3_plus_frame::file_alter_preservation::preserve,
                id3v2_3_plus_frame::read_only::set,
                boost::none,
                boost::none,
                false,
                false,
                boost::none);

  unsigned char buf[3];
  BOOST_CHECK( buf + 2 == encr.emailb(buf) );
  BOOST_CHECK( 'm' == buf[0] && 'e' == buf[1] );
  BOOST_CHECK( 1 == encr.method_symbol() );
  BOOST_CHECK( buf + 3 == encr.datab(buf) );
  BOOST_CHECK( 1 == buf[0] && 2 == buf[1] && 3 == buf[2] );

} // End test_encr_2_4.

BOOST_AUTO_TEST_CASE( test_xtag_2_4 )
{
  using namespace std;
  using namespace scribbu;

  typedef id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef id3v2_3_plus_frame::read_only read_only;

  const vector<unsigned char> buf01{
    0x01, 0x66, 0x6f, 0x6f, 0x40, 0x62, 0x61, 0x72,
    0x2e, 0x63, 0x6f, 0x6d, 0x00, 0x39, 0x30, 0x73,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x73, 0x75, 0x62,
    0x67, 0x65, 0x6e, 0x72, 0x65, 0x73, 0x00, 0x00,
    0x00, 0x00, 0x0e, 0x72, 0x6f, 0x63, 0x6b, 0x00,
    0x61, 0x6c, 0x74, 0x2d, 0x72, 0x6f, 0x63, 0x6b,
    0x00, 0x6d, 0x6f, 0x6f, 0x64, 0x00, 0x00, 0x00,
    0x00, 0x07, 0x6d, 0x65, 0x6c, 0x6c, 0x6f, 0x77,
    0x00,
  };

  XTAG_2_4 xtg01(buf01.begin(), buf01.end(),
                 tag_alter_preservation::preserve,
                 file_alter_preservation::preserve,
                 read_only::clear,
                 boost::none,
                 boost::none,
                 false,
                 false,
                 boost::none);
  BOOST_CHECK( "foo@bar.com" == xtg01.owner() );
  BOOST_CHECK( xtg01.has_key("90s") );
  BOOST_CHECK( !xtg01.has_key("80s") );

  BOOST_CHECK( 65 == xtg01.size() );

} // End test_xtaxg.
