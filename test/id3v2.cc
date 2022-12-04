/**
 * \file id3v2.cc
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/id3v2-utils.hh>

#include "unit.hh"

#include <fstream>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/id3v2.hh>

namespace fs = std::filesystem;

BOOST_AUTO_TEST_CASE( test_from_sync_safe)
{
  using namespace std;
  using namespace scribbu::detail;

  size_t x = unsigned_from_sync_safe(0x7f, 0x7f, 0x7f);
  BOOST_CHECK( ((1 << 21) - 1) == x);

  x = unsigned_from_sync_safe(0x7f, 0x7f, 0x7f, 0x7f);
  BOOST_CHECK(((1 << 28) - 1) == x);

  // NB. this overload only takes into account the first 32
  // bits
  x = unsigned_from_sync_safe(0x7f, 0x7f, 0x7f, 0x7f, 0x7f);
  BOOST_CHECK((((size_t)1 << 32) - 1) == x);

  uint32_t x32 = uint32_from_sync_safe(0x7f, 0x7f, 0x7f, 0x7f);
  BOOST_TEST_MESSAGE( "x32 is " << x32 );
  BOOST_CHECK( ((1 << 28) - 1) == x32);

  // NB. this overload only takes into account the first 32
  // bits
  x32 = uint32_from_sync_safe(0x7f, 0x7f, 0x7f, 0x7f, 0x7f);
  BOOST_CHECK(~0 == x32);

  x = unsigned_from_non_sync_safe(0xff, 0xff, 0xff);
  BOOST_CHECK(((1 << 24) - 1) == x);

  x = unsigned_from_non_sync_safe(0xff, 0xff, 0xff, 0xff);
  BOOST_TEST_MESSAGE("x ix " << x );
  BOOST_CHECK(0xffffffff == x);

}

BOOST_AUTO_TEST_CASE( test_looking_at )
{
  using namespace scribbu;

  const fs::path TEST_DATA_V1(get_data_directory() / "id3v1.tag");
  const fs::path TEST_DATA_V2_2(get_data_directory() / "id3v2.2.tag");
  const fs::path TEST_DATA_V2_3(get_data_directory() / "id3v2.3.tag");
  const fs::path TEST_DATA_V2_4(get_data_directory() / "id3v2.4.tag");

  std::ifstream ifsv1(TEST_DATA_V1, std::ifstream::binary);
  id3v2_info I = looking_at_id3v2(ifsv1);
  BOOST_CHECK( ! I.present_ );

  std::ifstream ifsv2_2(TEST_DATA_V2_2, std::ifstream::binary);
  I = looking_at_id3v2(ifsv2_2);
  BOOST_CHECK( I.present_ );
  BOOST_CHECK(2 == I.version_);
  BOOST_CHECK(0 == I.revision_);
  BOOST_CHECK(0 == I.flags_ );
  BOOST_CHECK(2192 == I.size_);
  BOOST_CHECK(0 == ifsv2_2.tellg());

  std::ifstream ifsv2_3(TEST_DATA_V2_3, std::ifstream::binary);
  I = looking_at_id3v2(ifsv2_3);
  BOOST_CHECK( I.present_ );
  BOOST_CHECK(3 == I.version_);
  BOOST_CHECK(0 == I.revision_);
  BOOST_CHECK(0 == I.flags_);
  BOOST_CHECK(452951 == I.size_);
  BOOST_CHECK(0 == ifsv2_3.tellg());

  std::ifstream ifsv2_4(TEST_DATA_V2_4, std::ifstream::binary);
  I = looking_at_id3v2(ifsv2_4, false);
  BOOST_CHECK( I.present_ );
  BOOST_CHECK(4 == I.version_);
  BOOST_CHECK(0 == I.revision_);
  BOOST_CHECK(0 == I.flags_);
  BOOST_CHECK(1091 == I.size_);
  BOOST_CHECK(10 == ifsv2_4.tellg());

}

BOOST_AUTO_TEST_CASE( test_resync )
{
  using scribbu::resynchronise;

  // This is mostly testing corner cases; cf. test_unsync.
  unsigned char BUF0[] = { };
  BOOST_CHECK(0 == resynchronise(BUF0, sizeof(BUF0)));

  unsigned char BUF1[] = { 0x01 };
  static const unsigned char GOLD1[] = { 0x01 };
  BOOST_CHECK(1 == resynchronise(BUF1, sizeof(BUF1)));
  BOOST_CHECK(0 == memcmp(GOLD1, BUF1, sizeof(GOLD1)));

  unsigned char BUF2[] = { 0x01, 0x02 };
  static const unsigned char GOLD2[] = { 0x01, 0x02 };
  BOOST_CHECK(2 == resynchronise(BUF2, sizeof(BUF2)));
  BOOST_CHECK(0 == memcmp(GOLD2, BUF2, sizeof(GOLD2)));

  unsigned char BUF3[] = { 0xff, 0x00 };
  static const unsigned char GOLD3[] = { 0xff };
  BOOST_CHECK(1 == resynchronise(BUF3, sizeof(BUF3)));
  BOOST_CHECK(0 == memcmp(GOLD3, BUF3, sizeof(GOLD3)));

  unsigned char BUF4[] = { 0x01, 0xff, 0x00 };
  static const unsigned char GOLD4[] = { 0x01, 0xff };
  BOOST_CHECK(2 == resynchronise(BUF4, sizeof(BUF4)));
  BOOST_CHECK(0 == memcmp(GOLD4, BUF4, sizeof(GOLD4)));

  unsigned char BUF5[] = { 0xff, 0x00, 0x01 };
  static const unsigned char GOLD5[] = { 0xff, 0x01 };
  BOOST_CHECK(2 == resynchronise(BUF5, sizeof(BUF5)));
  BOOST_CHECK(0 == memcmp(GOLD5, BUF5, sizeof(GOLD5)));

  unsigned char BUF6[] = { 0x01, 0xff, 0x00, 0x02 };
  static const unsigned char GOLD6[] = { 0x01, 0xff, 0x02 };
  BOOST_CHECK(3 == resynchronise(BUF6, sizeof(BUF6)));
  BOOST_CHECK(0 == memcmp(GOLD6, BUF6, sizeof(GOLD6)));

  unsigned char BUF7[] = { 0xff, 0x00, 0x01, 0xff, 0x00 };
  static const unsigned char GOLD7[] = { 0xff, 0x01, 0xff };
  BOOST_CHECK(3 == resynchronise(BUF7, sizeof(BUF7)));
  BOOST_CHECK(0 == memcmp(GOLD7, BUF7, sizeof(GOLD7)));

  unsigned char BUF8[] = { 0x01, 0xff, 0x00, 0x02, 0x03, 0xff, 0x00, 0x04 };
  static const unsigned char GOLD8[] = { 0x01, 0xff, 0x02, 0x03, 0xff, 0x04 };
  BOOST_CHECK(6 == resynchronise(BUF8, sizeof(BUF8)));
  BOOST_CHECK(0 == memcmp(GOLD8, BUF8, sizeof(GOLD8)));

  unsigned char BUF9[] = { 0xff, 0x00, 0x01, 0xff, 0x00, 0x02, 0x03,
                           0xff, 0x00, 0x04 };
  static const unsigned char GOLD9[] = { 0xff, 0x01, 0xff, 0x02, 0x03,
                                         0xff, 0x04 };
  BOOST_CHECK(7 == resynchronise(BUF9, sizeof(BUF9)));
  BOOST_CHECK(0 == memcmp(GOLD9, BUF9, sizeof(GOLD9)));

  unsigned char BUFA[] = { 0x01, 0xff, 0x00, 0x02, 0x03, 0xff, 0x00,
                           0x04, 0xff, 0x00 };
  static const unsigned char GOLDA[] = { 0x01, 0xff, 0x02, 0x03, 0xff,
                                         0x04, 0xff };
  BOOST_CHECK(7 == resynchronise(BUFA, sizeof(BUFA)));
  BOOST_CHECK(0 == memcmp(GOLDA, BUFA, sizeof(GOLDA)));

}

/**
 *
 * \code

   mgh@Crickhollow[2-0:...ts/scribbu/test/data]: od -A x -t x1z  红颜旧.mp3
   000000 49 44 33                                         ID3
   000003          03 00                                   ID3 version 2.3.0
   000005                00                                no flags
   000006                   00 00 20 64                    0x1064 = 4196 bytes (0x106e)
   00000a                               50 52 49 56        PRIV
   00000e                                           00 00  0x29 = 41 bytes(0x3d)
   000010 00 29
   000012       00 00                                      no flags
   000014             57 4d 2f 4d 65 64 69 61 43 6c 61 73  "WM/MediaClassSecondaryID"
   000020 73 53 65 63 6f 6e 64 61 72 79 49 44 00
   00002d                                        00 00 00  ...
   000030 00 00 00 00 00 00 00 00 00 00 00 00 00
   00003d                                        50 52     PRIV
   000040 56
   000041    00 00 00 27                                   0x27 = 39 bytes (0x6e)
   000045                00 00                             no flags
   000047                      57 4d 2f 4d 65 64 69 61 43  "WM/MediaClassPrimaryID"
   000050 6c 61 73 73 50 72 69 6d 61 72 79 49 44 00
   00005e                                           bc 7d  .}`.#..K..H.*(D...
   000060 60 d1 23 e3 e2 4b 86 a1 48 a4 2a 28 44 1e        `.#..K..H.*(D.
   00006e                                           00 00                ..
   000070 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
   *
   001060 00 00 00 00 00 00 00 00 00 00 00 00 00 00
   00106e                                           49 44  ID3
   001070 33
   001071    04 00                                         ID3 version 2.4.0
   001073          00                                      no flags
   001074             00 00 00 23                          0x23 = 35 bytes (0x109b)
   001078                         54 53 53 45              TSSE
   00107c                                     00 00 00 0f  0x0f = 15 bytes (0x1094)
   001080 00 00                                            no flags
   001082       03                                         UTF-8 Encoding
   001083          4c 61 76 66 35 35 2e 32 32 2e 31 30 33  Lavf55.22.103
   001090 00 00 00 00
   001094             00 00 00 00 00 00 00
   00109b                                  ff fb 40 00 00  ..@..

  \endcode
  *
  *
  */

BOOST_AUTO_TEST_CASE( test_jing_jing_2 )
{
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "红颜旧.mp3");

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);
  id3v2_info I2 = looking_at_id3v2(ifs);
  BOOST_CHECK(I2.present_);

  std::unique_ptr<id3v2_tag> pT = maybe_read_id3v2(ifs);
  BOOST_CHECK(3 == pT->version());
  BOOST_CHECK(0 == pT->revision());

  BOOST_CHECK( ! pT->has_album()        );
  BOOST_CHECK( ! pT->has_artist()       );
  BOOST_CHECK( ! pT->has_content_type() );
  BOOST_CHECK( ! pT->has_encoded_by()   );
  BOOST_CHECK( ! pT->has_languages()    );
  BOOST_CHECK( ! pT->has_title()        );
  BOOST_CHECK( ! pT->has_year()         );

  std::unique_ptr<id3v2_tag> pT2 = maybe_read_id3v2(ifs);
  BOOST_CHECK(4 == pT2->version());
  BOOST_CHECK(0 == pT2->revision());

  BOOST_CHECK( ! pT2->has_album()        );
  BOOST_CHECK( ! pT2->has_artist()       );
  BOOST_CHECK( ! pT2->has_content_type() );
  BOOST_CHECK( ! pT2->has_encoded_by()   );
  BOOST_CHECK( ! pT2->has_languages()    );
  BOOST_CHECK( ! pT2->has_title()        );
  BOOST_CHECK( ! pT2->has_year()         );

  ifs.close();

  ifs.open(TEST_DATA, std::ifstream::binary);
  std::vector<std::unique_ptr<id3v2_tag>> V;
  read_all_id3v2(ifs, std::back_inserter(V));
  ifs.close();

  BOOST_CHECK(2 == V.size());
}
