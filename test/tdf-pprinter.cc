/**
 * \file csv-pprinter.cc
 *
 * Copyright (C) 2019-2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/tdf-pprinter.hh>

#include "unit.hh"

#include <sstream>

#include <fstream>
#include <boost/test/unit_test.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>

namespace fs = std::filesystem;

BOOST_AUTO_TEST_CASE( test_tdf_pprinting_id3v1 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V1(get_data_directory() / "id3v1.tag");

  const string GOLDEN("1\t0\tThe Pogues\tLorca's Novena\tHell's Ditch [Expanded] (US Ve\t1990\tAmazon.com Song ID: 20355825\t255");

  std::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << print_as_tdf(4, encoding::ASCII) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

  stringstream stm2;
  stm2 << print_as_tdf(4, encoding::ASCII, encoding::ISO_8859_1, true) << tag;

  const string GOLDEN2("1\x1f""0\x1fThe Pogues\x1fLorca's Novena\x1fHell's Ditch [Expanded] (US Ve\x1f""1990\x1f""Amazon.com Song ID: 20355825\x1f""255");

  text = stm2.str();
  BOOST_TEST_MESSAGE( GOLDEN2 );
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN2 == text);
}

BOOST_AUTO_TEST_CASE( test_tdf_pprinting_id3v22 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  const string GOLDEN("2\t0\t2192\t0x00\t0\tMurley Braid Quartet\tSheep Walking\tMnemosyne's March (Demo)\t(8)\tiTunes v6.0.4\t\t\t0\t\t2\t 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78\t 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000\t\t");
  std::ifstream ifs(TEST_DATA, std::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << print_as_tdf(4, encoding::ASCII, encoding::UTF_8) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_tdf_pprinting_id3v23 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE_1(get_data_directory() / "id3v2.3.tag");
  const string GOLDEN_1("3\t0\t452951\t0x00\t0\tThe Pogues\tLorca's Novena\tHell's Di"
                        "tch [Expanded] (US Version)\tPop\t\t1990\t\t0\t\t1\tAmazon.co"
                        "m Song ID: 203558254\t\t\t");

  const fs::path TEST_FILE_2(get_data_directory() / "life.mp3");
  const string GOLDEN_2("3\t0\t1397\t0x00\t0\tFrank Sinatra\tThat's Life\tThe Very Go"
                        "od Years\tVocal\t\t\t\t2\t*\t5\t\tPretty slow\tEvening\t1966\t\t");

  std::ifstream ifs_1(TEST_FILE_1, std::ifstream::binary);
  id3v2_3_tag tag_1(ifs_1);

  stringstream stm1;
  stm1 << print_as_tdf(4, encoding::ASCII, boost::none) << tag_1;

  string text = stm1.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  stringstream stm2;
  stm2 << print_as_tdf() << tag_1;

  text = stm2.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  std::ifstream ifs_2(TEST_FILE_2, std::ifstream::binary);
  id3v2_3_tag tag_2(ifs_2);

  stringstream stm3;
  stm3 << print_as_tdf(6, encoding::ASCII, encoding::UTF_8) << tag_2;

  text = stm3.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_2 == text);


}

BOOST_AUTO_TEST_CASE( test_tdf_pprinting_id3v24 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.4.tag");

  const string GOLDEN("4\t0\t2115\t0x00\t0\tJoao Gilberto\tAcapulco\tEla E Carioca\t\t\t\t\t0\t\t0\t\t\t\t");

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << print_as_tdf(4, encoding::ASCII, boost::none) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_tdf_pprinting_xtag )
{
  using namespace std;
  using namespace scribbu;

  XTAG tc01("sp1ff@pobox.com",
                { { "tag1", { "has ,", "has %" } },
                  { "tag2", { "你好" } } });

  stringstream stm01;
  stm01 << print_as_tdf(1, encoding::ASCII, boost::none) << tc01;

  string text01 = stm01.str();
  BOOST_TEST_MESSAGE( text01 );
  BOOST_CHECK( text01 == "sp1ff@pobox.com\ttag1=has%20%25,has%20%2c&"
               "tag2=%e4%bd%a0%e5%a5%bd" );

} // End test_csv_pprinting_xtag.
