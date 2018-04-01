/**
 * \file pprinter.cc
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

#include <scribbu/pprinter.hh>

#include "unit.hh"

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_pprinting_id3v1 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V1(get_data_directory() / "id3v1.tag");

  const string GOLDEN(R"(ID3v1.1: The Pogues - Lorca's Novena
Hell's Ditch [Expanded] (US Ve (track 5), 1990
Amazon.com Song ID: 20355825
unknown genre 255
)");

  fs::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_2 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  const string GOLDEN(R"(ID3v2.2(.0) Tag:
2192 bytes, synchronised
flags: 0x00
Murley Braid Quartet - Sheep Walking
Mnemosyne's March (Demo), 2006
Content-type (8)
Encoded by iTunes v6.0.4
TT2: Sheep Walking
TP1: Murley Braid Quartet
TCM: Mike Murley
TAL: Mnemosyne's March (Demo)
TYE: 2006
TCO: (8)
TEN: iTunes v6.0.4
COM (iTunNORM):
 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78
COM (iTunSMPB):
 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
1802 bytes of padding
)");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_3 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.3.tag");

  const string GOLDEN("ID3v2.3(.0) Tag:\n"
                      "452951 bytes, synchronised\n"
                      "flags: 0x00\n"
                      "The Pogues - Lorca's Novena\n"
                      "Hell's Ditch [Expanded] (US Version) (track 5), 1990\n"
                      "Content-type Pop\n"
                      "TIT2: Lorca's Novena\n"
                      "TPE1: The Pogues\n"
                      "TALB: Hell's Ditch [Expanded] (US Version)\n"
                      "TCON: Pop\n"
                      "TCOM: \n"
                      "TPE3: \n"
                      "TRCK: 5\n"
                      "TYER: 1990\n"
                      "TPE2: The Pogues\n"
                      "COMM (<no description>):\n"
                      "Amazon.com Song ID: 203558254\n"
                      "TCOP: 2004 Warner Music UK Ltd.\n"
                      "TPOS: 1\n"
                      "frame APIC (115554 bytes)\n"
                      "frame PRIV (1122 bytes)\n"
                      "335921 bytes of padding\n"
                      "");

  fs::ifstream ifs_3(TEST_DATA, fs::ifstream::binary);
  id3v2_3_tag tag(ifs_3);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

  const fs::path TEST_DATA2(get_data_directory() / "cerulean.mp3");

  fs::ifstream ifs2(TEST_DATA2, fs::fstream::binary);
  id3v2_3_tag tag2(ifs2);

  stringstream stm2;
  stm2 << tag2;
  text = stm2.str();
  BOOST_TEST_MESSAGE(text);

}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_4 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.4.tag");

  const string GOLDEN(R"(ID3v2.4(.0) Tag:
1091 bytes, synchronised
flags: 0x00
Joao Gilberto - Acapulco
Ela E Carioca, <no year>
TPE1: Joao Gilberto
TIT2: Acapulco
TALB: Ela E Carioca
1024 bytes of padding
)");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}
