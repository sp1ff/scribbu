/**
 * \file framesv23.cc
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

#include "unit.hh"

#include <scribbu/framesv23.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v23.hh>

namespace fs = boost::filesystem;

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

BOOST_AUTO_TEST_CASE( test_xtag )
{
  using namespace std;
  using namespace scribbu;
  
  typedef id3v2_3_plus_frame::tag_alter_preservation 
    tag_alter_preservation;
  typedef id3v2_3_plus_frame::file_alter_preservation 
    file_alter_preservation;
  typedef id3v2_3_plus_frame::read_only read_only;

  const fs::path CERULEAN(get_data_directory() / "cerulean.mp3");

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

  //////////////////////////////////////////////////////////////////////////
  //                                smoke tests
  //////////////////////////////////////////////////////////////////////////

  XTAG xtg01(buf01.begin(), buf01.end(), 
             tag_alter_preservation::preserve,
             file_alter_preservation::preserve,
             read_only::clear,
             boost::none,
             boost::none,
             boost::none);
  BOOST_CHECK( "foo@bar.com" == xtg01.owner() );
  BOOST_CHECK( xtg01.has_key("90s") );
  BOOST_CHECK( !xtg01.has_key("80s") );
  BOOST_CHECK( 65 == xtg01.size() );
  
  //////////////////////////////////////////////////////////////////////////
  //                         something more interesting
  //////////////////////////////////////////////////////////////////////////

  // Read a tagset in...
  fs::ifstream ifs(CERULEAN, ios_base::binary);
  vector<unique_ptr<id3v2_tag>> tags;
  read_all_id3v2(ifs, back_inserter(tags));

  // grab the first (and only) tag...
  BOOST_CHECK( 1 == tags.size() );
  id3v2_3_tag &tag = dynamic_cast<id3v2_3_tag&>(*tags.front());
  
  // now let's add an XTAG frame...
  XTAG xtag("sp1ff@pobox.com", {{ "90s", {}}, { "sub-genres", {"shoegazer"} }});
  tag.push_back(xtag);
  
  // write the thing back out...
  fs::path tmp = fs::temp_directory_path() / fs::unique_path(); 
  fs::ofstream ofs(tmp, ios_base::binary);
  tag.write(ofs);
  ofs.close();
  
  // re-read it in...
  fs::ifstream tmpifs(tmp, ios_base::binary);
  vector<unique_ptr<id3v2_tag>> new_tags;
  read_all_id3v2(tmpifs, back_inserter(new_tags));
  
  // grab the first (and only)...
  BOOST_REQUIRE( 1 == new_tags.size() );
  id3v2_3_tag &new_tag = dynamic_cast<id3v2_3_tag&>(*new_tags.front());
  
  // extract the XTAG frame...
  const frame_id4 id("XTAG");

  id3v2_3_tag::iterator p = find_if(new_tag.begin(), new_tag.end(),
                                    [&](const id3v2_3_frame &F) { return id == F.id(); });
  BOOST_REQUIRE( p != new_tag.end() );

  // `p' is an id3v2_3_tag::iterator, which dereferences to 
  // a mutable_frame_proxy
  id3v2_3_frame &f = *p;
  BOOST_CHECK( id == f.id() );

  XTAG* pF = dynamic_cast<XTAG*>(&f);
  BOOST_REQUIRE(pF);
  BOOST_CHECK( "sp1ff@pobox.com" == pF->owner() );

} // End test_xtaxg.
