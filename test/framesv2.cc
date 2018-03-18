/**
 * \file framesv2.cc
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

#include <scribbu/framesv2.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_frame_id3 )
{
  using scribbu::frame_id3;

  const unsigned char ID[3] = {'T', 'I', 'T'};
  frame_id3 fid1('T', 'I', 'T'), fid2(ID), fid3("TIT");

  BOOST_CHECK( std::string("TIT") == fid1.as_string() );
  BOOST_CHECK( std::string("TIT") == fid2.as_string() );
  BOOST_CHECK( std::string("TIT") == fid3.as_string() );

  unsigned char id[3];
  BOOST_CHECK( id + 3 == fid1.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );
  BOOST_CHECK( id + 3 == fid2.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );
  BOOST_CHECK( id + 3 == fid3.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );

  BOOST_CHECK( fid1 == fid2 );
  BOOST_CHECK( fid2 == fid3 );

  std::stringstream stm;
  stm << fid1;
  BOOST_CHECK( std::string("TIT") == stm.str() );

  BOOST_CHECK( ! fid1.experimental() );

} // End test_frame_id3.

BOOST_AUTO_TEST_CASE( test_frame_id4 )
{
  using scribbu::frame_id4;

  const unsigned char ID[4] = {'T', 'I', 'T', '2'};
  frame_id4 fid1('T', 'I', 'T', '2'), fid2(ID),
    fid3("TIT2");

  BOOST_CHECK( std::string("TIT2") == fid1.as_string() );
  BOOST_CHECK( std::string("TIT2") == fid2.as_string() );
  BOOST_CHECK( std::string("TIT2") == fid3.as_string() );

  unsigned char id[4];
  BOOST_CHECK( id + 4 == fid1.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );
  BOOST_CHECK( id + 4 == fid2.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );
  BOOST_CHECK( id + 4 == fid3.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );

  BOOST_CHECK( fid1 == fid2 );
  BOOST_CHECK( fid2 == fid3 );

  std::stringstream stm;
  stm << fid1;
  BOOST_CHECK( std::string("TIT2") == stm.str() );

  BOOST_CHECK( ! fid1.experimental() );

} // End test_frame_id4.
