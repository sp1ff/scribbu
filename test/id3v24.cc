/**
 * \file id3v24.cc
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

#include <scribbu/id3v24.hh>

#include "unit.hh"

#include <fstream>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>

namespace fs = std::filesystem;

/**
 * \brief Test against one of the few ID3v2.4 frames I have
 *
 *
 * The test data:
 *
 \code

   vagrant@vagrant-ubuntu-trusty-64:/vagrant/test/data$ od -A x -t x1z id3v2.4.tag
   000000 49 44 33 04 00 00 00 00 08 43 54 50 45 31 00 00  >ID3......CTPE1..<
   000010 00 0e 00 00 00 4a 6f 61 6f 20 47 69 6c 62 65 72  >.....Joao Gilber<
   000020 74 6f 54 49 54 32 00 00 00 09 00 00 00 41 63 61  >toTIT2.......Aca<
   000030 70 75 6c 63 6f 54 41 4c 42 00 00 00 0e 00 00 00  >pulcoTALB.......<
   000040 45 6c 61 20 45 20 43 61 72 69 6f 63 61 00 00 00  >Ela E Carioca...<
   000050 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000440 00 00 00 00 00 00 00 00 00 00 00 00              >............<
   00044d

 \endcode
 *
 * Broken out:
 *
 \code

   000000 49 44 33                                         "ID3"
   000003          04 00                                   ID3v2.4
   000005                00                                no unsynch, no extended header, not experimental, no footer
   000006                   00 00 08 43                    tag is 1091 bytes in size [1]
   00000a                               54 50 45 31        TPE1 frame (Lead performer(s)/Soloist(s))
   00000e                                           00 00  frame is 14 bytes in size
   000010 00 0e
   000012       00 00                                      no frame flags
   000014             00                                   ISO-8859-1 encoding
   000015                4a 6f 61 6f 20 47 69 6c 62 65 72  "Joao Gilberto"
   000020 74 6f
   000022       54 49 54 32                                TIT2 frame (Title/songname/content description)
   000026                    00 00 00 09                   frame is 9 bytes in size
   00002a                               00 00              no frame flags
   00002c                                     00           ISO-8859-1 encoding
   00002d                                        41 63 61  "Acapulco"
   000030 70 75 6c 63 6f
   000035                54 41 4c 42                       TALB frame (Album/Movie/Show title)
   000039                            00 00 00 0e           frame is 14 bytes in size
   00003d                                        00 00     no frame flags
   00003f                                              00  ISO-8859-1 encoding
   000040 45 6c 61 20 45 20 43 61 72 69 6f 63 61           "Ela E Carioca"
   00004d                                        00 00 00  padding
   000050 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  padding
   *
   000440 00 00 00 00 00 00 00 00 00 00 00 00              >............< (1024 bytes of padding)
   00044d

   1. $00 00 08 43 => %0000 0000 %0000 0000 %0000 1000 %0100 0011
                      % 000 0000 % 000 0000 % 000 1000 % 100 0011
                      %0000 0000 0000 0000 0100 0100 0011
                      0x   0    0    0    0    4    4    3
                      0x0000443 => 1091
 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v2_4_tag )
{
  using namespace std;
  using namespace scribbu;

  const fs::path DATA1(get_data_directory() / "id3v2.4.tag");

  std::ifstream ifsv2_4(DATA1, std::ifstream::binary);

  id3v2_4_tag tag(ifsv2_4);
  BOOST_CHECK(4 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(2115 == tag.size());
  BOOST_CHECK(0 == tag.flags());
  BOOST_CHECK(!tag.has_extended_header());
  BOOST_CHECK(!tag.has_footer());
  boost::optional<bool> unsync = tag.unsynchronised();
  BOOST_CHECK(unsync && ! *unsync);
  BOOST_CHECK(1024 == tag.padding());

  BOOST_TEST_MESSAGE( "Album: '" << tag.album() << "'." );
  BOOST_TEST_MESSAGE( "Artist: '" << tag.artist() << "'." );
  BOOST_TEST_MESSAGE( "Title: '" << tag.title() << "'." );

  BOOST_CHECK( tag.has_album());
  BOOST_CHECK( tag.has_artist());
  BOOST_CHECK(!tag.has_content_type());
  BOOST_CHECK(!tag.has_encoded_by());
  BOOST_CHECK(!tag.has_languages());
  BOOST_CHECK( tag.has_title());
  BOOST_CHECK(!tag.has_track());
  BOOST_CHECK(!tag.has_year());

  BOOST_CHECK("Ela E Carioca" == tag.album());
  BOOST_CHECK("Joao Gilberto" == tag.artist());
  BOOST_CHECK("Acapulco" == tag.title());

} // End test_id3v2_4_tag.

/**
 * \brief Test another ID3v2.4 frame I found in the taglib test suite
 *
 *
 * The raw data:
 *
 \code

 000000 49 44 33 04 00 00 00 00 07 65 43 4f 4d 4d 00 00  >ID3......eCOMM..<
 000010 00 0e 00 00 00 58 58 58 00 41 20 43 4f 4d 4d 45  >.....XXX.A COMME<
 000020 4e 54 54 58 58 58 00 00 00 31 00 00 00 75 73 65  >NTTXXX...1...use<
 000030 72 54 65 78 74 44 65 73 63 72 69 70 74 69 6f 6e  >rTextDescription<
 000040 31 00 75 73 65 72 54 65 78 74 44 61 74 61 31 00  >1.userTextData1.<
 000050 75 73 65 72 54 65 78 74 44 61 74 61 32 54 58 58  >userTextData2TXX<
 000060 58 00 00 00 3c 00 00 00 51 75 6f 64 4c 69 62 65  >X...<...QuodLibe<
 000070 74 3a 3a 75 73 65 72 54 65 78 74 44 65 73 63 72  >t::userTextDescr<
 000080 69 70 74 69 6f 6e 32 00 75 73 65 72 54 65 78 74  >iption2.userText<
 000090 44 61 74 61 31 00 75 73 65 72 54 65 78 74 44 61  >Data1.userTextDa<
 0000a0 74 61 32 54 43 4f 4e 00 00 00 03 00 00 00 31 33  >ta2TCON.......13<
 0000b0 57 58 58 58 00 00 00 1a 00 00 00 75 73 65 72 55  >WXXX.......userU<
 0000c0 72 6c 00 68 74 74 70 3a 2f 2f 61 2e 75 73 65 72  >rl.http://a.user<
 0000d0 2e 75 72 6c 57 58 58 58 00 00 00 2a 00 00 00 00  >.urlWXXX...*....<
 0000e0 68 74 74 70 3a 2f 2f 61 2e 75 73 65 72 2e 75 72  >http://a.user.ur<
 0000f0 6c 2f 77 69 74 68 2f 65 6d 70 74 79 2f 64 65 73  >l/with/empty/des<
 000100 63 72 69 70 74 69 6f 6e 55 46 49 44 00 00 00 19  >criptionUFID....<
 000110 00 00 73 75 70 65 72 6d 69 68 69 40 77 65 62 2e  >..supermihi@web.<
 000120 64 65 00 31 32 33 34 35 36 37 38 00 00 00 00 00  >de.12345678.....<
 000130 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
 *
 0003e0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ff  >................<
 0003f0 fb a1 04 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<

 \endcode
 *
 * & broken out:
 *
 \code

 000000 49 44 33 04 00 00                                ID3v2.4, no flags
 000006                   00 00 07 65                    997 bytes [1]
 00000a                               43 4f 4d 4d        COMM comment frame
 00000e                                           00 00
 000010 00 0e                                            14 bytes
 000012       00 00                                      no flags
 000014             00                                   ISO-8859-1
 000015                58 58 58                          Language "XXX"
 000018                         00                       nil description
 000019                            41 20 43 4f 4d 4d 45  "A COMMENT"
 000020 4e 54
 000022       54 58 58 58 00 00 00 31 00 00              TXXX, 49 bytes, no flags
 00002c                                     00           ISO-8859-1
 00002d                                        75 73 65  "userTextDescription1"
 000030 72 54 65 78 74 44 65 73 63 72 69 70 74 69 6f 6e
 000040 31 00
 000042       75 73 65 72 54 65 78 74 44 61 74 61 31 00  "userTextData1.userTextData2"
 000050 75 73 65 72 54 65 78 74 44 61 74 61 32
 00005d                                        54 58 58  TXXX, 60 bytes, no flags
 000060 58 00 00 00 3c 00 00
 000067                      00                          ISO-8859-1
 000068                         51 75 6f 64 4c 69 62 65  "QuodLibe<t::userTextDescription2.userTextData1.userTextData2"
 000070 74 3a 3a 75 73 65 72 54 65 78 74 44 65 73 63 72
 000080 69 70 74 69 6f 6e 32 00 75 73 65 72 54 65 78 74
 000090 44 61 74 61 31 00 75 73 65 72 54 65 78 74 44 61
 0000a0 74 61 32
 0000a3          54 43 4f 4e 00 00 00 03 00 00           TCON, 3 bytes, no flags
 0000ad                                        00 31 33  ISO-8859-1, "13"
 0000b0 57 58 58 58 00 00 00 1a 00 00                    WXXX, 26 bytes, no flags
 0000ba                               00 75 73 65 72 55  ISO-8859-1, "userUrl"
 0000c0 72 6c 00
 0000c3          68 74 74 70 3a 2f 2f 61 2e 75 73 65 72  "http://a.user.url"
 0000d0 2e 75 72 6c
 0000d4             57 58 58 58 00 00 00 2a 00 00        WXXXX, 42 bytes, no flags
 0000de                                           00 00  ISO-8859-1, ""
 0000e0 68 74 74 70 3a 2f 2f 61 2e 75 73 65 72 2e 75 72  http://a.user.url/with/empty/description
 0000f0 6c 2f 77 69 74 68 2f 65 6d 70 74 79 2f 64 65 73
 000100 63 72 69 70 74 69 6f 6e
 000108                         55 46 49 44 00 00 00 19  UFID, 25 bytes, no flags
 000110 00 00
 000112       73 75 70 65 72 6d 69 68 69 40 77 65 62 2e  supermihi@web.de
 000120 64 65 00
 000123          31 32 33 34 35 36 37 38                 "12345678"
 00012b                                  00 00 00 00 00  padding (708 bytes)
 000130 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
 *
 0003e0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
 0003ef                                              ff  sync
 0003f0 fb ...

 1. 0x0765 = b0000 0111 0110 0101 ->  000 0111  110 0101 =
    0000 0011 1110 0101 = 0x03e5 = 997

 2. 0x3ef - 0x12b = 0x2c4 = 708

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_rare_frames )
{
  using namespace std;
  using namespace scribbu;

  const fs::path DATA(get_data_directory() / "rare_frames.mp3");

  std::ifstream ifsv2_4(DATA, std::ifstream::binary);

  id3v2_4_tag tag(ifsv2_4);
  BOOST_CHECK(4 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(7 == tag.num_frames());
  size_t cbp = tag.padding();
  BOOST_CHECK(708 == cbp);

  vector<COMM_2_4> comments;
  tag.get_comments(back_inserter(comments));
  BOOST_CHECK(1 == comments.size());

  string s = comments[0].text<string>();
  BOOST_CHECK("A COMMENT" == s);

  s = tag.content_type();
  BOOST_CHECK("13" == s);
}

/**
 * \brief Test an ID3v2.4 frame with an extended header
 *
 *
 * Raw data:
 *
 \code

   od -Ax -tx1z id3v2.4.ext.tag
   000000 49 44 33 04 00 40 00 00 00 5c 00 00 00 0c 01 20  >ID3..@...\..... <
   000010 05 0a 47 3d 24 71 54 52 43 4b 00 00 00 02 00 00  >..G=$qTRCK......<
   000020 00 38 54 58 58 58 00 00 00 24 00 00 00 45 6e 63  >.8TXXX...$...Enc<
   000030 6f 64 65 64 20 62 79 00 52 69 70 70 65 64 20 77  >oded by.Ripped w<
   000040 69 74 68 20 53 74 72 65 61 6d 72 69 70 70 65 72  >ith Streamripper<
   000050 54 49 54 32 00 00 00 0c 00 00 00 53 68 61 6e 61  >TIT2.......Shana<
   000060 67 6f 6c 64 65 6e ff fb 90 c4 00 00 00 00 00 00  >golden..........<
   000070 00 00 00 00 00 00 00 00 00 00 00 58 69 6e 67 00  >...........Xing.<
   000080 00 00 0f 00 00 2e 10 00 32 a5 fa 00 02 05 07 09  >........2.......<
   000090 0c 0f 11 13 16 18 1b 1d 1f 22 25 27 2a 2c 2f 31  >........."%'*,/1<
   0000a0 34 36 39 3c 3f 41 44 46 49 4b 4e 50 53 56 58 5b  >469<?ADFIKNPSVX[<
   0000b0 5d 60 62 65 67 6a 6d 6f 72 74 77 79 7c 7f 81 84  >]`begjmortwy|...<
   0000c0 87 89 8c 8e 91 93 96 98 9b 9e a0 a3 a6 a8 aa ad  >................<
   0000d0 af b2 b5 b7 ba bd bf c2 c4 c7 c9 cc cf d1 d4 d7  >................<
   0000e0 d9 dc de e0 e3 e6 e8 eb ee f0 f3 f6 f8 fa fd 00  >................<
   0000f0 00 00 50 4c 41 4d 45 33 2e 39 39 72 04 b9 00 00  >..PLAME3.99r....<
   000100 00 00 00 00 00 00 35 20 24 03 f0 41 00 01 e0 00  >......5 $..A....<
   000110 32 a5 fa 80 44 d8 cd 00 00 00 00 00 00 00 00 00  >2...D...........<
   000120 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000200 00 00 00 00 00 00 00                             >.......<
   000207

 \endcode
 *
 * broken out:
 *
 \code

   000000 49 44 33 04 00                                   ID3 v2.4 tag
   000005                40                                flags: ext. header present
   000006                   00 00 00 5c                    size: 0x5c = 92 bytes
   00000a                               00 00 00 0c        Ext. header is 0x0c = 12 bytes
   00000e                                           01 20  1 flag byte: CRC data present
   000010 05 0a 47 3d 24 71                                CRC-32 := 0xa8ef5271 [1]
   000016                   54 52 43 4b 00 00 00 02 00 00  TRCK, 2 bytes, no flags
   000020 00 38                                            ISO-8859-1 "8"
   000022       54 58 58 58 00 00 00 24 00 00              TXXX, 36 bytes, no flags
   00002c                                     00 45 6e 63  ISO-8859-1, "Encoded by",
   000030 6f 64 65 64 20 62 79 00 52 69 70 70 65 64 20 77  "Ripped with Streamripper"
   000040 69 74 68 20 53 74 72 65 61 6d 72 69 70 70 65 72
   000050 54 49 54 32 00 00 00 0c 00 00                    TIT2, 0x0c = 12 bytes, no flags
   00005a                               00 53 68 61 6e 61  ISO-8859-1, "Shanagolden"
   000060 67 6f 6c 64 65 6e
   00006g                   ff fb 90 c4 00 00 00 00 00 00  track data...
   *
   000200 00 00 00 00 00 00 00                             >.......<
   000207

   1. 0x0a 47 3d 24 71 = b0000 1010 0100 0111 0011 1101 0010 0100 0111 0001 =>

      b 000 1010 100 0111 011 1101 010 0100 111 0001
      b 1010 1000 1110 1111 0101 0010 0111 0001
     0x    a


      b  000 1010  100 0111  011 1101  010 0100  111 0001 =
      b 0000 1010 1000 1110 1111 0101 0010 0111 0001 =
      0x   0    a    8    e    f    5    2    7    1 = 0x0a8ef5271

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v24_ext_header )
{
  using namespace std;
  using namespace scribbu;

  const fs::path DATA(get_data_directory() / "id3v2.4.ext.tag");

  std::ifstream ifs(DATA, std::ifstream::binary);
  id3v2_4_tag tag(ifs);

  BOOST_CHECK(4 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(false == tag.unsynchronised());
  BOOST_CHECK(0x40 == tag.flags());

  BOOST_CHECK(92 == tag.size());
  BOOST_CHECK(false == tag.needs_unsynchronisation());
  BOOST_CHECK(3 == tag.num_frames());
  size_t cb = tag.padding();
  BOOST_CHECK(0 == cb);

  string s = tag.track();
  BOOST_CHECK("8" == s);
  s = tag.title();
  BOOST_CHECK("Shanagolden" == s);

  BOOST_CHECK(0 == tag.has_album());
  BOOST_CHECK(0 == tag.has_artist());
  BOOST_CHECK(0 == tag.has_content_type());
  BOOST_CHECK(0 == tag.has_encoded_by());
  BOOST_CHECK(0 == tag.has_languages());
  BOOST_CHECK(0 == tag.has_play_count());
  BOOST_CHECK(0 == tag.has_year());

  BOOST_CHECK(false == tag.experimental());
  BOOST_CHECK(true  == tag.has_extended_header());
  BOOST_CHECK(false == tag.has_footer());

  id3v2_4_tag::ext_header H = tag.extended_header();
  BOOST_CHECK(12 == H.size());
  BOOST_CHECK(H.has_crc());
  BOOST_CHECK(0xa8ef5271 == H.crc());

}

BOOST_AUTO_TEST_CASE( test_trailing_ff_4 )
{
  using namespace std;
  using namespace scribbu;

  // Let's construct a tag by hand that ends in 0xff
  const unsigned char TAG[] = {
    0x49, 0x44, 0x33,       // "ID3"
    0x04, 0x00,             // version 4
    0x80,                   // unsync, no extended header, not experimental
    0x00, 0x00, 0x00, 0x0b, // tag is eleven bytes
    0x50, 0x43, 0x4e, 0x54, // "PCNT"
    0x00, 0x00, 0x00, 0x01, // frame size (less header)
    0x00, 0x00,             // no flags set
    0xff,                   // 255 plays
  }; // 21 bytes

  stringstream stm(string((const char*)TAG, sizeof(TAG)));
  id3v2_4_tag tag(stm);

  // Smoke checks
  BOOST_CHECK( 1 == tag.num_frames() );
  BOOST_TEST_MESSAGE( "padding is " << tag.padding() );
  BOOST_CHECK( 0 == tag.padding() );

  // OK-- the heart of the matter. Given the trailing 0xff, in ID3v2.4 we
  // *should* append a trailing null when writing when applying
  // unsynchronisation.
  BOOST_TEST_MESSAGE( "size is " << tag.size(true) );
  BOOST_CHECK( sizeof(TAG) - 10 + 1 == tag.size(true) );
  BOOST_CHECK( tag.needs_unsynchronisation() );
}
