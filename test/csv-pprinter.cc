/**
 * \file csv-pprinter.cc
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

#include <scribbu/csv-pprinter.hh>

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

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v1 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V1(get_data_directory() / "id3v1.tag");

  const string GOLDEN("1,0,The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Ve,1990,Amazon.com Song ID: 20355825,255");

  fs::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v22 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  const string GOLDEN("2,0,2192,0x00,0,Murley Braid Quartet,Sheep Walking,Mnemosyne's March (Demo),(8),iTunes v6.0.4,,,0,,2, 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78, 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000,,");
  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, encoding::UTF_8) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v23 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE_1(get_data_directory() / "id3v2.3.tag");
  const string GOLDEN_1("3,0,452951,0x00,0,The Pogues,Lorca's Novena,Hell's Di"
                        "tch [Expanded] (US Version),Pop,,1990,,0,,1,Amazon.co"
                        "m Song ID: 203558254,,,");

  const fs::path TEST_FILE_2(get_data_directory() / "life.mp3");
  const string GOLDEN_2("3,0,1397,0x00,0,Frank Sinatra,That's Life,The Very Go"
                        "od Years,Vocal,,,,2,*,5,,Pretty slow,Evening,1966,,");

  fs::ifstream ifs_1(TEST_FILE_1, fs::ifstream::binary);
  id3v2_3_tag tag_1(ifs_1);

  stringstream stm1;
  stm1 << print_as_csv(4, encoding::ASCII, boost::none) << tag_1;

  string text = stm1.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  stringstream stm2;
  stm2 << print_as_csv() << tag_1;

  text = stm2.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  fs::ifstream ifs_2(TEST_FILE_2, fs::ifstream::binary);
  id3v2_3_tag tag_2(ifs_2);

  stringstream stm3;
  stm3 << print_as_csv(6, encoding::ASCII, encoding::UTF_8) << tag_2;

  text = stm3.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_2 == text);


}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v24 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.4.tag");

  const string GOLDEN("4,0,1091,0x00,0,Joao Gilberto,Acapulco,Ela E Carioca,,,,,0,,0,,,,");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, boost::none) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_xtag )
{
  using namespace std;
  using namespace scribbu;
  
  XTAG tc01("sp1ff@pobox.com",
	    { { "tag1", { "has ,", "has %" } },
	      { "tag2", { "你好" } } }); 
  
  stringstream stm01;
  stm01 << print_as_csv(1, encoding::ASCII, boost::none) << tc01;
  
  string text01 = stm01.str();
  BOOST_TEST_MESSAGE( text01 );
  BOOST_CHECK( "sp1ff@pobox.com,\"tag1=has%20%25,has%20%2c&"
	       "tag2=%e4%bd%a0%e5%a5%bd\"" == text01 );

} // End test_csv_pprinting_xtag.

BOOST_AUTO_TEST_CASE( test_csv_pprinting_rfc4180 )
{
  using namespace std;
  using namespace scribbu;

  typedef id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef id3v2_3_plus_frame::read_only read_only;

  static const char COMMA = ',';

  string s;

  //////////////////////////////////////////////////////////////////////////////
  // test basic escaping
  //////////////////////////////////////////////////////////////////////////////

  s = csv_pprinter::escape("text", COMMA);
  BOOST_CHECK(s == "text");

  s = csv_pprinter::escape("\"text\"", COMMA);
  BOOST_CHECK(s == "\"text\"");

  s = csv_pprinter::escape("text,and more", COMMA);
  BOOST_CHECK(s == "\"text,and more\"");

  s = csv_pprinter::escape("text, and \"quotes\"", COMMA);
  BOOST_CHECK(s == "\"text, and \"\"quotes\"\"\"");

  s = csv_pprinter::escape(R"NL(text, "quotes" and
a newline)NL", COMMA);
  BOOST_CHECK(s == R"NL("text, ""quotes"" and
a newline")NL");

  //////////////////////////////////////////////////////////////////////////////
  // test on a frame
  ////////////////////////////////////////////////////////////////////////////

  const unsigned char B[] = {
    0x00,                   // non-Unicode
    0x61, 0x20, 0x22, 0x64, 0x73, 0x63, 0x22, 0x2c,
    0x20, 0x77, 0x2c, 0x00, // 'a "dsc", w,'
    0x74, 0x65, 0x78, 0x74, 0x0a, 0x74, 0x65, 0x78,
    0x74,                   // 'text\ntext'
  };

  TXXX txxx(B, B + sizeof(B), tag_alter_preservation::preserve,
            file_alter_preservation::preserve, read_only::clear,
            boost::none, boost::none, boost::none);
  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, boost::none) << txxx;

  string text = stm.str();

  const string GOLDEN(R"foo(0,"a ""dsc"", w,","text
text")foo");
  BOOST_CHECK(text == GOLDEN);

  const unsigned char C[] = {
    0x00,                   // non-Unicode
    0x64, 0x73, 0x63, 0x3b, 0x20, 0x66, 0x6f, 0x6f, 0x00, // 'dsc; foo'
    0x31, 0x2c, 0x32, 0x33, 0x34,                         // '1,234'
  };

  TXXX txxx2(C, C + sizeof(C), tag_alter_preservation::preserve,
             file_alter_preservation::preserve, read_only::clear,
             boost::none, boost::none, boost::none);

  stringstream stm2;
  stm2 << print_as_csv(1, encoding::ASCII, boost::none, ';');
  stm2 << txxx2;
  text = stm2.str();

  const string GOLDEN2(R"foo(0;"dsc; foo";1,234)foo");
  BOOST_TEST_MESSAGE("checkpoint: " << GOLDEN2 << "!=" << text);
  BOOST_CHECK(text == GOLDEN2);

} // End test_csv_pprinting_rfc4180.
