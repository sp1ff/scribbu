/**
 * \file scribbu.cc
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

#include <scribbu/scribbu.hh>

#include "unit.hh"

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/ostream.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/csv-pprinter.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_track_data )
{
  const fs::path TEST_DATA(get_data_directory() / "cerulean.mp3");

  using namespace std;
  using namespace scribbu;

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  unique_ptr<scribbu::id3v2_tag> pid3v2 = maybe_read_id3v2(ifs); // ID3v2 tags...
  track_data td(ifs);                                            // the track itself...
  unique_ptr<id3v1_tag> pid3v1 = process_id3v1(ifs);             // & the ID3v1 tag.

  stringstream stm1;
  stm1 << *pid3v2 << td;
  if (pid3v1) {
    stm1 << *pid3v1;
  }

  static const string GOLD1("ID3v2.3(.0) Tag:\n"
"295607 bytes, synchronised\n"
"flags: 0x00\n"
"The Ocean Blue - Questions Of Travel (LP Version)\n"
"Cerulean (US Release) (track 6/12), 2005\n"
"Content-type Alternative Rock\n"
"POPM: rating@winamp.com\n"
"rating: 255\n"
"counter: 00000000\n"
"frame PRIV (8207 bytes)\n"
"TIT2: Questions Of Travel (LP Version)\n"
"TPE1: The Ocean Blue\n"
"TALB: Cerulean (US Release)\n"
"TCON: Alternative Rock\n"
"TPE3: \n"
"TRCK: 6/12\n"
"TYER: 2005\n"
"TPE2: The Ocean Blue\n"
"COMM (<no description>):\n"
"tags=90s,sub-genres=shoegazer\n"
"TCOP: 2005 Warner Bros. Records Inc. Manufactured & Marketed by Warner Strategic Marketing.\n"
"TPOS: 1/1\n"
"frame APIC (286673 bytes)\n"
"82 bytes of padding\n"
"7760096 bytes of track data:\n"
"MD5: 32e73c84659bcf729333d7dd2efca044\n"
"ID3v1.1: The Ocean Blue - Questions Of Travel (LP Versio\n"
"Cerulean (US Release) (track 6), 2005\n"
"tags=90s,sub-genres=shoegaze\n"
"unknown genre 255\n"
"");

  string text = stm1.str();
  BOOST_TEST_MESSAGE( text );
  BOOST_CHECK( GOLD1 == text );

  static const string GOLD2("3,0,295607,0x00,0,The Ocean Blue,Questions Of Travel (LP Version),Cerulean (US Release),Alternative Rock,,2005,,0,,1,\"tags=90s,sub-genres=shoegazer\",,,,7760096,32e73c84659bcf729333d7dd2efca044");

  stringstream stm2;
  stm2 << print_as_csv(4, encoding::ASCII, boost::none) << *pid3v2 << "," << td;

  text = stm2.str();
  BOOST_TEST_MESSAGE( GOLD2 );
  BOOST_TEST_MESSAGE( text );
  BOOST_CHECK( text == GOLD2 );

}

BOOST_AUTO_TEST_CASE( test_track_data2 )
{
  const fs::path TEST_DATA(get_data_directory() / "searchresults.dat");

  using namespace std;
  using namespace scribbu;

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  ios_base::iostate state = ifs.rdstate();
  unique_ptr<scribbu::id3v2_tag> pid3v2 = maybe_read_id3v2(ifs); // ID3v2 tags...
  state = ifs.rdstate();
  track_data td(ifs);                                            // the track itself...
  state = ifs.rdstate();
  unique_ptr<id3v1_tag> pid3v1 = process_id3v1(ifs);             // & the ID3v1 tag.

  stringstream stm1;
  if (pid3v2) {
    stm1 << *pid3v2;
  }
  stm1 << td;
  if (pid3v1) {
    stm1 << *pid3v1;
  }

  string text = stm1.str();
  BOOST_TEST_MESSAGE( text );

  static const string GOLD1(R"(8 bytes of track data:
MD5: 69c1753bd5f81501d95132d08af04464
)");

  BOOST_CHECK(text == GOLD1);

}
