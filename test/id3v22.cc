/**
 * \file id3v22.cc
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

#include <scribbu/id3v22.hh>

#include "unit.hh"

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;

/**
 * \brief id3v2_2_ tag unit tests
 *
 * Here's the test data:
 *
 \code

   mgh@Crickhollow[2-1:...ts/scribbu/test/data]: od -A x -t x1z id3v2.2.tag
   000000 49 44 33 02 00 00 00 00 11 10 54 54 32 00 00 0f  >ID3.......TT2...<
   000010 00 53 68 65 65 70 20 57 61 6c 6b 69 6e 67 00 54  >.Sheep Walking.T<
   000020 50 31 00 00 16 00 4d 75 72 6c 65 79 20 42 72 61  >P1....Murley Bra<
   000030 69 64 20 51 75 61 72 74 65 74 00 54 43 4d 00 00  >id Quartet.TCM..<
   000040 0d 00 4d 69 6b 65 20 4d 75 72 6c 65 79 00 54 41  >..Mike Murley.TA<
   000050 4c 00 00 1a 00 4d 6e 65 6d 6f 73 79 6e 65 27 73  >L....Mnemosyne's<
   000060 20 4d 61 72 63 68 20 28 44 65 6d 6f 29 00 54 59  > March (Demo).TY<
   000070 45 00 00 06 00 32 30 30 36 00 54 43 4f 00 00 05  >E....2006.TCO...<
   000080 00 28 38 29 00 54 45 4e 00 00 0f 00 69 54 75 6e  >.(8).TEN....iTun<
   000090 65 73 20 76 36 2e 30 2e 34 00 43 4f 4d 00 00 68  >es v6.0.4.COM..h<
   0000a0 00 65 6e 67 69 54 75 6e 4e 4f 52 4d 00 20 30 30  >.engiTunNORM. 00<
   0000b0 30 30 30 36 45 31 20 30 30 30 30 30 30 44 33 20  >0006E1 000000D3 <
   0000c0 30 30 30 30 34 46 38 44 20 30 30 30 30 31 39 39  >00004F8D 0000199<
   0000d0 30 20 30 30 30 30 36 37 32 39 20 30 30 30 30 31  >0 00006729 00001<
   0000e0 45 31 41 20 30 30 30 30 36 34 44 31 20 30 30 30  >E1A 000064D1 000<
   0000f0 30 37 45 31 30 20 30 30 30 30 35 35 38 32 20 30  >07E10 00005582 0<
   000100 30 30 30 44 46 37 38 00 43 4f 4d 00 00 82 00 65  >000DF78.COM....e<
   000110 6e 67 69 54 75 6e 53 4d 50 42 00 20 30 30 30 30  >ngiTunSMPB. 0000<
   000120 30 30 30 30 20 30 30 30 30 30 32 31 30 20 30 30  >0000 00000210 00<
   000130 30 30 30 37 41 32 20 30 30 30 30 30 30 30 30 30  >0007A2 000000000<
   000140 30 34 41 36 43 34 45 20 30 30 30 30 30 30 30 30  >04A6C4E 00000000<
   000150 20 30 30 30 30 30 30 30 30 20 30 30 30 30 30 30  > 00000000 000000<
   000160 30 30 20 30 30 30 30 30 30 30 30 20 30 30 30 30  >00 00000000 0000<
   000170 30 30 30 30 20 30 30 30 30 30 30 30 30 20 30 30  >0000 00000000 00<
   000180 30 30 30 30 30 30 20 30 30 30 30 30 30 30 30 00  >000000 00000000.<
   000190 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000890 00 00 00 00 00 00 00 00 00 00                    >..........<
   00089a

 \endcode
 *
 * broken out:
 *
 \code

   000000 49 44 33                                         ID3
   000003          02 00                                   ID3v2 version 2.0
   000005                00                                no unsync, no compression [1]
   000006                   00 00 11 10                    tag size is 2192 bytes [2]
   00000a                               54 54 32           TT2
   00000d                                        00 00 0f  frame size is 15 bytes
   000010 00                                               ISO-8859-1 encoding
   000011    53 68 65 65 70 20 57 61 6c 6b 69 6e 67 00     "Sheep Walking"
   00001f                                              54  TP1
   000020 50 31
   000022       00 00 16                                   frame size is 22 bytes
   000025                00                                ISO-8859-1 encoding
   000026                   4d 75 72 6c 65 79 20 42 72 61  "Murley Braid Quartet"
   000030 69 64 20 51 75 61 72 74 65 74 00
   00003b                                 54 43 4d         TCM
   00003e                                           00 00  frame size is 13 bytes
   000040 0d
   000041    00                                            ISO-8859-1 encoding
   000042       4d 69 6b 65 20 4d 75 72 6c 65 79 00        "Mike Murley"
   00004e 54 41 4c                                         TAL
   000051          00 00 1a                                frame size is 26 bytes
   000054                   00                             ISO-8859-1 encoding
   000055                      4d 6e 65 6d 6f 73 79 6e 65  "Mnemosyne's March (Demo)"
   00005e 27 73 20 4d 61 72 63 68 20 28 44 65 6d 6f 29 00
   00006e 54 59 45                                         TYE
   000071          00 00 06                                frame size is 6 bytes
   000074 00                                               ISO-8859-1 encoding
   000075 32 30 30 36 00                                   "2006"
   00007a                54 43 4f                          TCO
   00007d                         00 00 05                 frame size is 5 bytes
   000080 00                                               ISO-8859-1 encoding
   000081    28 38 29 00                                   "(8)"
   000085                54 45 4e                          TEN
   000088                         00 00 0f                 frame size is 15 bytes
   00008b                                  00              ISO-8859-1 encoding
   00008c                                     69 54 75 6e  "iTunes v6.0.4"
   000090 65 73 20 76 36 2e 30 2e 34 00
   00009a                               43 4f 4d           COM
   00009d                                        00 00 68  frame size is 104 bytes
   0000a0 00                                               ISO-8859-1 encoding
   0000a1 65 6e 67                                         eng
   0000a4 69 54 75 6e 4e 4f 52 4d 00                       "iTunNORM"
   0000ad 20 30 30                                         " 00
   0000b0 30 30 30 36 45 31 20 30 30 30 30 30 30 44 33 20  0006E1 000000D3
   0000c0 30 30 30 30 34 46 38 44 20 30 30 30 30 31 39 39  00004F8D 0000199
   0000d0 30 20 30 30 30 30 36 37 32 39 20 30 30 30 30 31  0 00006729 00001
   0000e0 45 31 41 20 30 30 30 30 36 34 44 31 20 30 30 30  E1A 000064D1 000
   0000f0 30 37 45 31 30 20 30 30 30 30 35 35 38 32 20 30  07E10 00005582 0
   000100 30 30 30 44 46 37 38 00                          000DF78"
   000108                         43 4f 4d                 COM
   00010b                                  00 00 82        frame size is 130 bytes
   00010e                                           00     ISO-8859-1 encoding
   00010f                                              65  eng
   000110 6e 67
   000112 69 54 75 6e 53 4d 50 42 00                       "iTunSMPB"
   00011a                            20 30 30 30 30        " 0000
   000120 30 30 30 30 20 30 30 30 30 30 32 31 30 20 30 30  0000 00000210 00
   000130 30 30 30 37 41 32 20 30 30 30 30 30 30 30 30 30  0007A2 000000000
   000140 30 34 41 36 43 34 45 20 30 30 30 30 30 30 30 30  04A6C4E 00000000
   000150 20 30 30 30 30 30 30 30 30 20 30 30 30 30 30 30   00000000 000000
   000160 30 30 20 30 30 30 30 30 30 30 30 20 30 30 30 30  00 00000000 0000
   000170 30 30 30 30 20 30 30 30 30 30 30 30 30 20 30 30  0000 00000000 00
   000180 30 30 30 30 30 30 20 30 30 30 30 30 30 30 30 00  000000 00000000"
   000190

   1. 00 = %00000000
   2. 00 00 11 10 => %0000 0000 %0000 0000 %0001 0001 %0001 0000
                     % 000 0000 % 000 0000 % 001 0001 % 001 0000
                     %0000 0000 0000 0000 1000 1001 0000
                     0x   0   0     0    0    8    9    0
                     0x0000890 = 0x890 => ID3v2 header is 2192 bytes in size

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v2_2_tag )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V2_2(get_data_directory() / "id3v2.2.tag");

  fs::ifstream ifsv2_2(TEST_DATA_V2_2, fs::ifstream::binary);

  id3v2_2_tag tag(ifsv2_2);

  BOOST_CHECK(2 == tag.version());
  BOOST_CHECK(0 == tag.revision());

  /////////////////////////////////////////////////////////////////////////////

  id3v2_2_tag::iterator p = tag.begin();
  BOOST_CHECK("TT2" == p->id());
  BOOST_CHECK(21  == p->serialized_size(true)); ++p;
  BOOST_CHECK(28  == p->serialized_size(true)); ++p;
  BOOST_CHECK(19  == p->serialized_size(true)); ++p;
  BOOST_CHECK(32  == p->serialized_size(true)); ++p;
  BOOST_CHECK(12  == p->serialized_size(true)); ++p;
  BOOST_CHECK(11  == p->serialized_size(true)); ++p;
  BOOST_CHECK(21  == p->serialized_size(true)); ++p;
  BOOST_CHECK(110 == p->serialized_size(true)); ++p; // COM
  BOOST_CHECK(136 == p->serialized_size(true)); ++p; // COM
  BOOST_CHECK(p == tag.end());

  /////////////////////////////////////////////////////////////////////////////

  BOOST_CHECK(2192 == tag.size());
  BOOST_CHECK(0 == tag.flags());
  boost::optional<bool> unsync = tag.unsynchronised();
  BOOST_CHECK(unsync && ! *unsync);
  BOOST_CHECK(!tag.compression());

  BOOST_TEST_MESSAGE( "Album: '"        << tag.album()        << "'." );
  BOOST_TEST_MESSAGE( "Artist: '"       << tag.artist()       << "'." );
  BOOST_TEST_MESSAGE( "Content Type: '" << tag.content_type() << "'." );
  BOOST_TEST_MESSAGE( "Encoded by: '"   << tag.encoded_by()   << "'." );
  BOOST_TEST_MESSAGE( "Title: '"        << tag.title()        << "'." );

  BOOST_CHECK( tag.has_album()        );
  BOOST_CHECK( tag.has_artist()       );
  BOOST_CHECK( tag.has_content_type() );
  BOOST_CHECK( tag.has_encoded_by()   );
  BOOST_CHECK(!tag.has_languages()    );
  BOOST_CHECK( tag.has_title()        );
  BOOST_CHECK(!tag.has_track()        );
  BOOST_CHECK( tag.has_year()         );

  BOOST_CHECK("Mnemosyne's March (Demo)" == tag.album()        );
  BOOST_CHECK("Murley Braid Quartet"     == tag.artist()       );
  BOOST_CHECK("(8)"                      == tag.content_type() );
  BOOST_CHECK("iTunes v6.0.4"            == tag.encoded_by()   );
  BOOST_CHECK("Sheep Walking"            == tag.title()        );
  BOOST_CHECK("2006"                     == tag.year()         );

  vector<COM> C;
  tag.get_comments(back_inserter(C));
  BOOST_CHECK(2 == C.size());

  const vector<unsigned char> DSC0({{'i', 'T', 'u', 'n', 'N', 'O', 'R', 'M', }});
  const vector<unsigned char> TXT0({{' ', '0', '0', '0', '0', '0', '6', 'E',
                                     '1', ' ', '0', '0', '0', '0', '0', '0',
                                     'D', '3', ' ', '0', '0', '0', '0', '4',
                                     'F', '8', 'D', ' ', '0', '0', '0', '0',
                                     '1', '9', '9', '0', ' ', '0', '0', '0',
                                     '0', '6', '7', '2', '9', ' ', '0', '0',
                                     '0', '0', '1', 'E', '1', 'A', ' ', '0',
                                     '0', '0', '0', '6', '4', 'D', '1', ' ',
                                     '0', '0', '0', '0', '7', 'E', '1', '0',
                                     ' ', '0', '0', '0', '0', '5', '5', '8',
                                     '2', ' ', '0', '0', '0', '0', 'D', 'F',
                                     '7', '8', 0}});
  const COM &C0 = C[0];
  char lang[3];
  vector<unsigned char> dsc, text;
  BOOST_CHECK(0 == C0.unicode());
  C0.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  C0.descriptionb(back_inserter(dsc));
  BOOST_CHECK(dsc == DSC0);
  C0.textb(back_inserter(text));
  BOOST_CHECK(text == TXT0);

  dsc.resize(0);
  text.resize(0);

  const vector<unsigned char> DSC1({{'i', 'T', 'u', 'n', 'S', 'M', 'P', 'B'}});
  const vector<unsigned char> TXT1({{' ', '0', '0', '0', '0', '0', '0', '0',
                                     '0', ' ', '0', '0', '0', '0', '0', '2',
                                     '1', '0', ' ', '0', '0', '0', '0', '0',
                                     '7', 'A', '2', ' ', '0', '0', '0', '0',
                                     '0', '0', '0', '0', '0', '0', '4', 'A',
                                     '6', 'C', '4', 'E', ' ', '0', '0', '0',
                                     '0', '0', '0', '0', '0', ' ', '0', '0',
                                     '0', '0', '0', '0', '0', '0', ' ', '0',
                                     '0', '0', '0', '0', '0', '0', '0', ' ',
                                     '0', '0', '0', '0', '0', '0', '0', '0',
                                     ' ', '0', '0', '0', '0', '0', '0', '0',
                                     '0', ' ', '0', '0', '0', '0', '0', '0',
                                     '0', '0', ' ', '0', '0', '0', '0', '0',
                                     '0', '0', '0', ' ', '0', '0', '0', '0',
                                     '0', '0', '0', '0', 0}});

  const COM &C1 = C[1];
  BOOST_CHECK(0 == C1.unicode());
  C1.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  C1.descriptionb(back_inserter(dsc));
  BOOST_CHECK(dsc == DSC1);
  C1.textb(back_inserter(text));
  BOOST_CHECK(text == TXT1);

} // End test_id3v2_2_tag.

/**
 * \brief Test against an ID3v2.2 tag I found in the taglib test suite
 *
 *
 * The raw binary:
 *
 \code

 000000 49 44 33 02 00 00 00 00 03 76 54 44 41 00 00 06  >ID3......vTDA...<
 000010 00 30 33 30 34 00 54 52 4b 00 00 03 00 31 00 54  >.0304.TRK....1.T<
 000020 59 45 00 00 06 00 32 30 31 30 00 00 00 00 00 00  >YE....2010......<
 000030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
 *
 000200 ff fb 10 64 00 0f f0 75 00 3d 00 40 00 00 0e 80  >...d...u.=.@....<

 \endcode
 *
 * and broken out:
 *
 \code

 000000 49 44 33 02 00 00 00 00 03 76                    ID3v2.2, no flags, 502 bytes [1]
 00000a                               54 44 41 00 00 06  TDA, 6 bytes
 000010 00 30 33 30 34 00                                ISO-8859-1 "0304"
 000016                   54 52 4b 00 00 03              TRK, 3 bytes
 00001c                                     00 31 00     ISO-8859-1 "1"
 00001f                                              54  TYE, 6 bytes
 000020 59 45 00 00 06
 000025                00 32 30 31 30 00                 ISO-8859-1, "2010"
 00002b                                  00 00 00 00 00  469 bytes padding
 000030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
 *
 000200 ff fb 10 64 00 0f f0 75 00 3d 00 40 00 00 0e 80  sync

 1. 0x0376 = b0000 0011 0111 0110 -> b000 0011 111 0110 -> b00 0001 1111 0110 = 0x01f6

 2. 0x1d5 =

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v2_2_tag_2 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V2_2(get_data_directory() / "id3v22-tda.mp3");

  fs::ifstream ifsv2_2(TEST_DATA_V2_2, fs::ifstream::binary);

  id3v2_2_tag tag(ifsv2_2);

  BOOST_CHECK(2 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(3 == tag.num_frames());
  BOOST_CHECK(469 == tag.padding());

  string s = tag.track();
  BOOST_CHECK("1" == s);
  s = tag.year();
  BOOST_CHECK("2010" == s);

} // End test_id3v2_2_tag_2.

BOOST_AUTO_TEST_CASE( test_id3v2_2_tag_as_container )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_2_tag tag(ifs);

  BOOST_CHECK(2 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(9 == tag.num_frames());

  vector<unsigned char> D = { 0xba, 0xbe };
  UFI ufi(D.begin(), D.end());
  auto p = tag.insert(tag.begin(), ufi);
  BOOST_CHECK(p == tag.begin());

  id3v2_2_text_frame F("TCO",  string("test"), encoding::UTF_8);
  p = tag.insert(p, F);
  BOOST_CHECK( p->id() == "TCO" );

  tag.erase(p);
  BOOST_CHECK( 10 == tag.num_frames() );
}


BOOST_AUTO_TEST_CASE( test_id3v22_text_frames )
{
  using namespace std;
  using namespace scribbu;
  
  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_2_tag tag(ifs);
  
  BOOST_CHECK("Murley Braid Quartet" == tag.text(id3v2_text_frames::tpe1));
  tag.text(id3v2_text_frames::tpe1, "foo");
  BOOST_CHECK("foo" == tag.text(id3v2_text_frames::tpe1));
  tag.delete_frame(id3v2_text_frames::tpe1);
  BOOST_CHECK( 9 == tag.num_frames() );
  
  BOOST_CHECK( !tag.has_artist() );
  
  // https://github.com/sp1ff/scribbu/issues/4
  // BOOST_CHECK_THROW( tag.artist(), ... );

  // Check to be sure all the other text frames still work
  BOOST_CHECK( "(8)"           == tag.content_type() );
  BOOST_CHECK( "iTunes v6.0.4" == tag.encoded_by()   );
  BOOST_CHECK( "Sheep Walking" == tag.title()        );
  BOOST_CHECK( "2006"          == tag.year()         );

} // End test_text_frames.
