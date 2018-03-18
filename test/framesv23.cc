/**
 * \file framesv23.cc
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

#include <scribbu/framesv23.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_ufid )
{
  using scribbu::unique_file_id;
  using scribbu::id3v2_3_plus_frame;
  using scribbu::UFID;

  const unsigned char DATA[] = {
    's', 'p', '1', 'f', 'f', 0,
    1, 2, 3
  };

  UFID ufid(DATA, DATA + sizeof(DATA),
            id3v2_3_plus_frame::tag_alter_preservation::preserve,
            id3v2_3_plus_frame::file_alter_preservation::preserve,
            id3v2_3_plus_frame::read_only::set,
            boost::none,
            boost::none,
            boost::none);

  BOOST_CHECK( "UFID" == ufid.id() );

  unsigned char buf[5];
  BOOST_CHECK( buf + 5 == ufid.ownerb(buf) );
  BOOST_CHECK( 's' == buf[0] && 'p' == buf[1] &&
               '1' == buf[2] && 'f' == buf[3] &&
               'f' == buf[4] );
  BOOST_CHECK( buf + 3 == ufid.idb(buf) );
  BOOST_CHECK( 1 == buf[0] && 2 == buf[1] && 3 == buf[2] );

} // End test_ufid.
