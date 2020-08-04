/**
 * \file framesv2.cc
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#include <sstream>

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

BOOST_AUTO_TEST_CASE( test_unique_file_id )
{
  using namespace std;
  using namespace scribbu;

  const char EMAIL[] = "sp1ff@pobox.com";
  const size_t NEMAIL = strlen(EMAIL);

  vector<unsigned char> outbuf;

  //////////////////////////////////////////////////////////////////////////////
  // happy case
  //////////////////////////////////////////////////////////////////////////////
  vector<unsigned char> B1 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d, 0x00, // "sp1ff@pobox.com"
      0x01, 0x02, 0x03, 0x04 };                       // ID: 0x01020304

  unique_file_id id1(B1.begin(), B1.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  id1.ownerb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  id1.idb(back_inserter(outbuf));
  const vector<unsigned char> ID = { 0x01, 0x02, 0x03, 0x04 };
  BOOST_CHECK_EQUAL_COLLECTIONS(ID.begin(), ID.end(),
                                outbuf.begin(), outbuf.end());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case-- trailing NULL, no ID
  //////////////////////////////////////////////////////////////////////////////
  vector<unsigned char> D2 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d, 0x00 };
  unique_file_id id2(D2.begin(), D2.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  id2.ownerb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  id2.idb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case-- no trailing NULL, no ID
  //////////////////////////////////////////////////////////////////////////////
  vector<unsigned char> D3 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d };
  unique_file_id id3(D3.begin(), D3.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  id3.ownerb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  id3.idb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  BOOST_CHECK(id3.size() == NEMAIL + 1);
  BOOST_CHECK(id3.serialized_size(true) == NEMAIL + 1);
  BOOST_CHECK(!id3.needs_unsynchronisation());

  stringstream stm;
  id3.write(stm);
  string text = stm.str();
  BOOST_CHECK(0 == strcmp(EMAIL, text.c_str()));

} // End test_unique_file_id.

BOOST_AUTO_TEST_CASE( test_encryption_method )
{
  using namespace std;
  using namespace scribbu;

  const char EMAIL[] = "sp1ff@pobox.com";
  const size_t NEMAIL = strlen(EMAIL);

  vector<unsigned char> outbuf;

  //////////////////////////////////////////////////////////////////////////////
  // happy case
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B1 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d, 0x00, // "sp1ff@pobox.com"
      0x0b,                                           // method
      0x01, 0x02, 0x03, 0x04 };                       // key

  encryption_method M1(B1.begin(), B1.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  M1.emailb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());
  BOOST_CHECK(0x0b == M1.method_symbol());

  const unsigned char KEY1[] = { 0x01, 0x02, 0x03,0x04 };
  outbuf.erase(outbuf.begin(), outbuf.end());
  M1.datab(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(KEY1, KEY1 + sizeof(KEY1),
                                outbuf.begin(), outbuf.end());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: no binary data
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B2 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d, 0x00, // "sp1ff@pobox.com"
      0x0b };                                         // method

  encryption_method M2(B2.begin(), B2.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  M2.emailb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());
  BOOST_CHECK(0x0b == M2.method_symbol());

  outbuf.erase(outbuf.begin(), outbuf.end());
  M2.datab(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: no method symbol & no binary data
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B3 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d, 0x00 }; // "sp1ff@pobox.com"

  encryption_method M3(B3.begin(), B3.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  M3.emailb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());

  // Can't check  M3.method_symbol()-- undefined in this case.

  outbuf.erase(outbuf.begin(), outbuf.end());
  M3.datab(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: no trailing nul, no method symbol & no binary data
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B4 =
    { 0x73, 0x70, 0x31, 0x66, 0x66, 0x40, 0x70, 0x6f,
      0x62, 0x6f, 0x78, 0x2e, 0x63, 0x6f, 0x6d }; // "sp1ff@pobox.com"

  encryption_method M4(B4.begin(), B4.end());

  outbuf.erase(outbuf.begin(), outbuf.end());
  M4.emailb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(EMAIL, EMAIL + NEMAIL,
                                outbuf.begin(), outbuf.end());

  // Can't check  M4.method_symbol()-- undefined in this case.

  outbuf.erase(outbuf.begin(), outbuf.end());
  M4.datab(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  // LATER(sp1ff): test the following
  // - size()
  // - serialized_size()
  // - needs_unsynchronisation()
  // - write()

} // End test_encryption_method.

BOOST_AUTO_TEST_CASE( test_user_defined_text )
{
  using namespace std;
  using namespace scribbu;

  const char DSC[] = "dsc";
  const size_t NDSC = strlen(DSC);
  const char VAL[] = "val";
  const size_t NVAL = strlen(VAL);
  const unsigned char UDSC[] =
    { 0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63 };
  const size_t NUDSC = sizeof(UDSC);
  const unsigned char UVAL[] =
    { 0xfe, 0xff, 0x00, 0x76, 0x00, 0x61, 0x00, 0x6c };
  const size_t NUVAL = sizeof(UVAL);

  vector<unsigned char> outbuf;

  //////////////////////////////////////////////////////////////////////////////
  // happy case-- ASCII
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B1 =
    { 0x00, // ISO-8859-1
      0x64, 0x73, 0x63, 0x00, // "dsc"
      0x76, 0x61, 0x6c, // "val"
    };

  user_defined_text T1(id3v2_version::v3, B1.begin(), B1.end());
  BOOST_CHECK(0 == T1.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T1.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T1.textb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(VAL, VAL + NVAL,
                                outbuf.begin(), outbuf.end());

  //////////////////////////////////////////////////////////////////////////////
  // happy case-- UCS2
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B2 =
    { 0x01, // UCS-2
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, 0x00, 0x00, // "dsc"
      0xfe, 0xff, 0x00, 0x76, 0x00, 0x61, 0x00, 0x6c, // "val"
    };

  user_defined_text T2(id3v2_version::v3, B2.begin(), B2.end());
  BOOST_CHECK(1 == T2.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T2.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T2.textb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UVAL, UVAL + NUVAL,
                                outbuf.begin(), outbuf.end());


  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B3 =
    { 0x00, // ISO-8859-1
      0x64, 0x73, 0x63, 0x00, // "dsc"
    };

  user_defined_text T3(id3v2_version::v3, B3.begin(), B3.end());
  BOOST_CHECK(0 == T3.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T3.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T3.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no trailing null, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B4 =
    { 0x00, // ISO-8859-1
      0x64, 0x73, 0x63, // "dsc"
    };

  user_defined_text T4(id3v2_version::v3, B4.begin(), B4.end());
  BOOST_CHECK(0 == T4.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T4.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T4.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no description, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B5 =
    { 0x00, // ISO-8859-1
    };

  user_defined_text T5(id3v2_version::v3, B5.begin(), B5.end());
  BOOST_CHECK(0 == T5.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T5.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T5.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B6 =
    { 0x01, // UCS-2
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, 0x00, 0x00, // "dsc"
    };

  user_defined_text T6(id3v2_version::v3, B6.begin(), B6.end());
  BOOST_CHECK(1 == T6.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T6.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T6.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no trailing null, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B7 =
    { 0x01, // UCS-2
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, // "dsc"
    };

  user_defined_text T7(id3v2_version::v3, B7.begin(), B7.end());
  BOOST_CHECK(1 == T7.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T7.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T7.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no description, no value
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B8 =
    { 0x01, // UCS-2
    };

  user_defined_text T8(id3v2_version::v3, B8.begin(), B8.end());
  BOOST_CHECK(1 == T8.unicode());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T8.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  T8.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

} // End test_user_defined_text.

BOOST_AUTO_TEST_CASE( test_comments )
{
  using namespace std;
  using namespace scribbu;

  const char DSC[] = "dsc";
  const size_t NDSC = strlen(DSC);
  const char VAL[] = "val";
  const size_t NVAL = strlen(VAL);
  const unsigned char UDSC[] =
    { 0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63 };
  const size_t NUDSC = sizeof(UDSC);
  const unsigned char UVAL[] =
    { 0xfe, 0xff, 0x00, 0x76, 0x00, 0x61, 0x00, 0x6c };
  const size_t NUVAL = sizeof(UVAL);

  vector<unsigned char> outbuf;
  unsigned char lang[3];

  //////////////////////////////////////////////////////////////////////////////
  // happy case-- ASCII
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B1 =
    { 0x00, // ISO-8859-1
      0x65, 0x6e, 0x67, // eng
      0x64, 0x73, 0x63, 0x00, // "dsc"
      0x76, 0x61, 0x6c, // "val"
    };

  comments C1(id3v2_version::v3, B1.begin(), B1.end());
  BOOST_CHECK(0 == C1.unicode());
  C1.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C1.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C1.textb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(VAL, VAL + NVAL,
                                outbuf.begin(), outbuf.end());

  //////////////////////////////////////////////////////////////////////////////
  // happy case-- UCS2
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B2 =
    { 0x01, // UCS-2
      0x65, 0x6e, 0x67, // eng
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, 0x00, 0x00, // "dsc"
      0xfe, 0xff, 0x00, 0x76, 0x00, 0x61, 0x00, 0x6c, // "val"
    };

  comments C2(id3v2_version::v3, B2.begin(), B2.end());
  BOOST_CHECK(1 == C2.unicode());
  C2.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C2.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C2.textb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UVAL, UVAL + NUVAL,
                                outbuf.begin(), outbuf.end());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no text
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B3 =
    { 0x00, // ISO-8859-1
      0x65, 0x6e, 0x67, // eng
      0x64, 0x73, 0x63, 0x00, // "dsc"
    };

  comments C3(id3v2_version::v3, B3.begin(), B3.end());
  BOOST_CHECK(0 == C3.unicode());
  C3.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C3.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C3.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no text, no trailing null
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B4 =
    { 0x00, // ISO-8859-1
      0x65, 0x6e, 0x67, // eng
      0x64, 0x73, 0x63, // "dsc"
    };

  comments C4(id3v2_version::v3, B4.begin(), B4.end());
  BOOST_CHECK(0 == C4.unicode());
  C4.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C4.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(DSC, DSC + NDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C4.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no text, no description
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B5 =
    { 0x00, // ISO-8859-1
      0x65, 0x6e, 0x67, // eng
    };

  comments C5(id3v2_version::v3, B5.begin(), B5.end());
  BOOST_CHECK(0 == C5.unicode());
  C5.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C5.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C5.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: ASCII, no text, no description, partial language
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B6 =
    { 0x00, // ISO-8859-1
      0x65, // 'e'
    };

  comments C6(id3v2_version::v3, B6.begin(), B6.end());
  BOOST_CHECK(0 == C6.unicode());
  // No test for `lang'
  outbuf.erase(outbuf.begin(), outbuf.end());
  C6.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C6.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no text
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B7 =
    { 0x01, // UCS-2
      0x65, 0x6e, 0x67, // eng
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, 0x00, 0x00, // "dsc"
    };

  comments C7(id3v2_version::v3, B7.begin(), B7.end());
  BOOST_CHECK(1 == C7.unicode());
  C7.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C7.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C7.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no text, no trailing null
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B8 =
    { 0x01, // UCS-2
      0x65, 0x6e, 0x67, // eng
      0xfe, 0xff, 0x00, 0x64, 0x00, 0x73, 0x00, 0x63, // "dsc"
    };

  comments C8(id3v2_version::v3, B8.begin(), B8.end());
  BOOST_CHECK(1 == C8.unicode());
  C8.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C8.descriptionb(back_inserter(outbuf));
  BOOST_CHECK_EQUAL_COLLECTIONS(UDSC, UDSC + NUDSC,
                                outbuf.begin(), outbuf.end());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C8.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no text, no description
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B9 =
    { 0x01, // UCS-2
      0x65, 0x6e, 0x67, // eng
    };

  comments C9(id3v2_version::v3, B9.begin(), B9.end());
  BOOST_CHECK(1 == C9.unicode());
  C9.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  outbuf.erase(outbuf.begin(), outbuf.end());
  C9.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C9.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

  //////////////////////////////////////////////////////////////////////////////
  // pathological case: UCS-2, no text, no description, partial language
  //////////////////////////////////////////////////////////////////////////////

  vector<unsigned char> B10 =
    { 0x01, // UCS-2
      0x65, 0x6e, // eng
    };

  comments C10(id3v2_version::v3, B10.begin(), B10.end());
  BOOST_CHECK(1 == C10.unicode());
  // no check possible
  outbuf.erase(outbuf.begin(), outbuf.end());
  C10.descriptionb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());
  outbuf.erase(outbuf.begin(), outbuf.end());
  C10.textb(back_inserter(outbuf));
  BOOST_CHECK(outbuf.empty());

}

BOOST_AUTO_TEST_CASE( test_play_count )
{
  using namespace std;
  using namespace scribbu;


  //////////////////////////////////////////////////////////////////////////
  // happy case: play count as a four-byte, big-endian unsigned
  //////////////////////////////////////////////////////////////////////////

  vector<unsigned char> buf01 = { 0x00, 0x00, 0x00, 0x0b };
  play_count p01(buf01.begin(), buf01.end());

  BOOST_CHECK( 11 == p01.count() );
  BOOST_CHECK( 4 == p01.size() );
  BOOST_CHECK( 4 == p01.serialized_size(true) );
  BOOST_CHECK( !p01.needs_unsynchronisation() );

  stringstream stm01;
  p01.write(stm01);
  unsigned char out01[4];
  stm01.read((char*)out01, 4);
  BOOST_CHECK( buf01[0] == out01[0] &&
               buf01[1] == out01[1] &&
               buf01[2] == out01[2] &&
               buf01[3] == out01[3] );

  p01.inc();
  BOOST_CHECK( 12 == p01.count() );
  p01.count(13);
  BOOST_CHECK( 13 == p01.count() );

  //////////////////////////////////////////////////////////////////////////
  // happy case: play count as an unsigned
  //////////////////////////////////////////////////////////////////////////

  unsigned x = 11;
  play_count p02(x);

  BOOST_CHECK( 11 == p02.count() );
  BOOST_CHECK( 1 == p02.size() );
  BOOST_CHECK( 1 == p02.serialized_size(true) );
  BOOST_CHECK( !p02.needs_unsynchronisation() );

  //////////////////////////////////////////////////////////////////////////
  // interesting case: play count of zero
  //////////////////////////////////////////////////////////////////////////

  play_count p03(0);
  BOOST_CHECK( 0 == p03.count() );
  BOOST_CHECK( 1 == p03.size() );

} // End test_play_count.

BOOST_AUTO_TEST_CASE( test_popularimeter )
{
  using namespace std;
  using namespace scribbu;

  //////////////////////////////////////////////////////////////////////////
  // happy case
  //////////////////////////////////////////////////////////////////////////

  popularimeter p01("sp1ff@pobox.com", 255, 11);

  BOOST_CHECK( p01.count() == 11 );
  BOOST_CHECK( p01.rating() == 255 );

} // End test_popularimeter.

BOOST_AUTO_TEST_CASE( test_tag_cloud )
{
  using namespace std;
  using namespace scribbu;

  //////////////////////////////////////////////////////////////////////////
  // happy case
  //////////////////////////////////////////////////////////////////////////

  tag_cloud c01("sp1ff@pobox.com",
                { { "90s", { } },
                  { "sub-genres", { "rock", "alt-rock" } },
                  { "mood", { "mellow" } } });
  BOOST_CHECK( "sp1ff@pobox.com" == c01.owner() );
  BOOST_CHECK( c01.has_key("90s") );
  BOOST_CHECK( c01.has_value("sub-genres", "rock") );
  BOOST_CHECK( c01.add_key("speed") );
  BOOST_CHECK( !c01.add_value("sub-genres", "alt-rock") );

  string text = c01.urlencoded();
  BOOST_TEST_MESSAGE( text );
  BOOST_CHECK( text == "90s&mood=mellow&speed&sub-genres=alt-rock,rock" );

  //////////////////////////////////////////////////////////////////////////
  // check URL-encoding
  //////////////////////////////////////////////////////////////////////////

  tag_cloud c02("sp1ff@pobox.com",
                { { "tag1", { "has ,", "has %" } },
                  { "tag2", { "你好" } } });

  text = c02.urlencoded();
  BOOST_TEST_MESSAGE( text );
  BOOST_CHECK( text == "tag1=has%20%25,has%20%2c&tag2=%e4%bd%a0%e5%a5%bd" );

  //////////////////////////////////////////////////////////////////////////
  // test URL-decoding
  //////////////////////////////////////////////////////////////////////////

  tag_cloud c03("sp1ff@pobox.com", text);
  BOOST_REQUIRE( c03.has_key("tag1") );
  BOOST_CHECK(c03.has_value("tag1", "has ,"));
  BOOST_CHECK(c03.has_value("tag1", "has %"));
  BOOST_REQUIRE( c03.has_key("tag2") );
  BOOST_CHECK(c03.has_value("tag2", "你好"));

} // End test_tag_cloud.

BOOST_AUTO_TEST_CASE( test_id3v2_text_frames )
{
  using namespace std;
  using namespace scribbu;

  {
    stringstream stm("artist");
    id3v2_text_frames x;
    stm >> x;
    BOOST_CHECK( id3v2_text_frames::tpe1 == x );

    frame_id3 id3(x);
    BOOST_CHECK( id3 == "TP1" );

    frame_id4 id4(x);
    BOOST_CHECK( id4 == "TPE1" );

    stringstream out;
    out << x;
    BOOST_CHECK( out.str() == "tpe1" );
  }

} // End test_id3v2_text_frames.
