/**
 * \file charsets.cc
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

#include <scribbu/charsets.hh>

#include <boost/test/unit_test.hpp>

namespace utf = boost::unit_test;

BOOST_AUTO_TEST_CASE( test_charsets )
{
  using namespace std;
  using namespace scribbu;

  istringstream istm("ISO-8859-1");
  istream is(istm.rdbuf());
  encoding e;
  is >> e;
  BOOST_CHECK(encoding::ISO_8859_1 == e);

  stringstream os;
  os << encoding::KOI8_R;
  BOOST_CHECK(os.str() == "KOI8-R");

  BOOST_CHECK(  scribbu::char_traits<char>::is_code_unit(encoding::ISO_8859_1));
  BOOST_CHECK(! scribbu::char_traits<char>::is_code_unit(encoding::UCS_2));

  string s;

  // Let's test some edge cases, first...
  s = convert_encoding<string>((const char*)0, 0, encoding::UTF_8,
                               encoding::ISO_8859_1);
  BOOST_CHECK(s.empty());

  unsigned char buf0[] = { 0 };
  s = convert_encoding<string>(buf0, sizeof(buf0), encoding::UTF_8,
                               encoding::ISO_8859_1);
  BOOST_CHECK(s.empty());

  unsigned char buf1[] = { 0, 0 };
  s = convert_encoding<string>(buf1, sizeof(buf1), encoding::UTF_8,
                               encoding::ISO_8859_1);
  BOOST_CHECK(s.empty());

  unsigned char buf2[] = { 0xff };
  BOOST_CHECK_THROW(convert_encoding<string>(buf2, sizeof(buf2),
                                             encoding::ASCII,
                                             encoding::ISO_8859_1),
    iconv_error);

  // Trivial, but this was actually failing at one point.
  vector<unsigned char> v;
  v = convert_encoding<string>("", encoding::UTF_8, encoding::ISO_8859_1);
  BOOST_CHECK(v.empty());
}

BOOST_AUTO_TEST_CASE( test_utf8 )
{
  using namespace std;
  using namespace scribbu;

  string s;

  // UTF-8 encoding of "a" with BOM
  unsigned char buf3[] = { 0xef, 0xbb, 0xbf, 'a' };
  s = convert_encoding<string>(buf3, sizeof(buf3), encoding::UTF_8,
                               encoding::UTF_8);
  BOOST_CHECK("a" == s);

  // UTF-8 encoding, of "Hello, 世界", no BOM
  unsigned char buf4[] = { 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0xe4,
                           0xb8, 0x96, 0xe7, 0x95, 0x8c };
  s = convert_encoding<string>(buf4, sizeof(buf4), encoding::UTF_8,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // If there is a BOM, we can't convert to ASCII...
  BOOST_CHECK_THROW(convert_encoding<string>(buf3, sizeof(buf3),
                                             encoding::UTF_8,
                                             encoding::ASCII),
                    iconv_error);
  // or any other non-Unicode encoding.
  BOOST_CHECK_THROW(convert_encoding<string>(buf3, sizeof(buf3),
                                             encoding::UTF_8,
                                             encoding::ISO_8859_1),
                    iconv_error);
}

BOOST_AUTO_TEST_CASE( test_utf16 )
{
  using namespace std;
  using namespace scribbu;

  string s;

  // UTF-16 *and* UCS2 Little Endian encoding of "Hello, 世界" *with* a BOM
  unsigned char buf5[] = {
    0xff, 0xfe, 0x48, 0x00, 0x65, 0x00, 0x6C, 0x00,
    0x6C, 0x00, 0x6F, 0x00, 0x2C, 0x00, 0x20, 0x00,
    0x16, 0x4e, 0x4c, 0x75,
  };

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16BE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2BE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // UTF-16 *and* UCS2 Little Endian encoding of "Hello, 世界" with *no* BOM
  unsigned char buf6[] = {
    0x48, 0x00, 0x65, 0x00, 0x6C, 0x00, 0x6C, 0x00,
    0x6F, 0x00, 0x2C, 0x00, 0x20, 0x00, 0x16, 0x4e,
    0x4c, 0x75,
  };

  // LATER(sp1ff): default is big endian, per GNU iconv source (1.15)-- should
  // fail. Works on Debian, not on MacOS.
  // s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16, encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): default is big endian, per GNU iconv source (1.15)-- should
  // fail. Works on Debian, not on MacOS
  // s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2, encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16BE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2BE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // UTF-16 *and* UCS2 Big Endian encoding of "Hello, 世界" with BOM
  unsigned char buf7[] = {
    0xfe, 0xff, 0x00, 0x48, 0x00, 0x65, 0x00, 0x6C,
    0x00, 0x6C, 0x00, 0x6F, 0x00, 0x2C, 0x00, 0x20,
    0x4e, 0x16, 0x75, 0x4c,
  };

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UTF_16,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UTF_16BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): default is big endian (per GNU iconv source 1.15),
  // and there's a BOM, so I would expect this to pass. However,
  // it fails on MacOS.
  // s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UCS_2,
  //                              encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UCS_2BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UCS_2LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // UTF-16 *and* UCS2 Big Endian encoding of "Hello, 世界" with *no* BOM
  unsigned char buf8[] = {
    0x00, 0x48, 0x00, 0x65, 0x00, 0x6C, 0x00, 0x6C,
    0x00, 0x6F, 0x00, 0x2C, 0x00, 0x20, 0x4e, 0x16,
    0x75, 0x4c,
  };

  // LATER(sp1ff): should work-- big endian is the default-- works on Debian,
  // not on MacOS
  // s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16, encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): should work-- big endian is the default. Works on Debian, not
  // on MacOS.
  // s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2, encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): No BOM and wrong byte order specified in encoding-- I would
  // expect this to fail, but it does not.
  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16LE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // LATER(sp1ff): No BOM and wrong byte order specified in encoding-- I would
  // expect this to fail, but it does not.
  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2LE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);
}

BOOST_AUTO_TEST_CASE( test_utf32 )
{
  using namespace std;
  using namespace scribbu;

  string s;

  // UTF-32 Little Endian encoding of "Hello, 世界" with BOM
  unsigned char buf9[] = {
    0xff, 0xfe, 0x00, 0x00,
    0x48, 0x00, 0x00, 0x00,
    0x65, 0x00, 0x00, 0x00,
    0x6C, 0x00, 0x00, 0x00,
    0x6C, 0x00, 0x00, 0x00,
    0x6F, 0x00, 0x00, 0x00,
    0x2C, 0x00, 0x00, 0x00,
    0x20, 0x00, 0x00, 0x00,
    0x16, 0x4E, 0x00, 0x00,
    0x4C, 0x75, 0x00, 0x00,
  };

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UTF_32, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UTF_32LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(s = convert_encoding<string>(buf9, sizeof(buf9),
                                                 encoding::UTF_32BE,
                                                 encoding::UTF_8),
                    iconv_error);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): default is big endian (per GNU iconv 1.15 source
  // code)-- should fail. Throws on Debain, not on MacOS
  // BOOST_CHECK_THROW(s = convert_encoding<string>(buf9, sizeof(buf9),
  //                                                encoding::UCS_4,
  //                                                encoding::UTF_8),
  //                   iconv_error);

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UCS_4LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): I'm lying about the byte order in the encoding, but there is
  // a BOM... throws on Debian, but not on MacOS
  // BOOST_CHECK_THROW(s = convert_encoding<string>(buf9, sizeof(buf9),
  //                                                encoding::UCS_4BE,
  //                                                encoding::UTF_8),
  //                   iconv_error);

  // UTF-32 Big Endian encoding of "Hello, 世界" with BOM
  unsigned char buf10[] = {
    0x00, 0x00, 0xfe, 0xff,
    0x00, 0x00, 0x00, 0x48,
    0x00, 0x00, 0x00, 0x65,
    0x00, 0x00, 0x00, 0x6C,
    0x00, 0x00, 0x00, 0x6C,
    0x00, 0x00, 0x00, 0x6F,
    0x00, 0x00, 0x00, 0x2C,
    0x00, 0x00, 0x00, 0x20,
    0x00, 0x00, 0x4E, 0x16,
    0x00, 0x00, 0x75, 0x4C,
  };

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UTF_32,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UTF_32BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(s = convert_encoding<string>(buf10, sizeof(buf10),
                                                 encoding::UTF_32LE,
                                                 encoding::UTF_8),
                    iconv_error);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UCS_4,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UCS_4BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // LATER(sp1ff): No BOM, wrong byte order-- throws on Debian, not on MacOS.
  // BOOST_CHECK_THROW(convert_encoding<string>(buf10, sizeof(buf10),
  //                                            encoding::UCS_4LE,
  //                                            encoding::UTF_8),
  //                   iconv_error);

  // UTF-32 Little Endian encoding of "Hello, 世界" with *no* BOM
  unsigned char buf11[] = {
    0x48, 0x00, 0x00, 0x00,
    0x65, 0x00, 0x00, 0x00,
    0x6C, 0x00, 0x00, 0x00,
    0x6C, 0x00, 0x00, 0x00,
    0x6F, 0x00, 0x00, 0x00,
    0x2C, 0x00, 0x00, 0x00,
    0x20, 0x00, 0x00, 0x00,
    0x16, 0x4E, 0x00, 0x00,
    0x4C, 0x75, 0x00, 0x00,
  };

  // LATER(sp1ff): default is big endian (per GNU iconv 1.15 source), there's no
  // BOM and I'm not specifying a byte-order in the encoding-- I would expect
  // this to fail.  It does on MacOS, but not on Debian.
  // BOOST_CHECK_THROW(s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UTF_32,
  //                                                encoding::UTF_8),
  //                   iconv_error);
  // BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UTF_32LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf11, sizeof(buf11),
                                             encoding::UTF_32BE,
                                             encoding::UTF_8),
                    iconv_error);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UCS_4, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UCS_4LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UCS_4BE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // UTF-32 Big Endian encoding of "Hello, 世界" with *no* BOM
  unsigned char buf12[] = {
    0x00, 0x00, 0x00, 0x48,
    0x00, 0x00, 0x00, 0x65,
    0x00, 0x00, 0x00, 0x6C,
    0x00, 0x00, 0x00, 0x6C,
    0x00, 0x00, 0x00, 0x6F,
    0x00, 0x00, 0x00, 0x2C,
    0x00, 0x00, 0x00, 0x20,
    0x00, 0x00, 0x4E, 0x16,
    0x00, 0x00, 0x75, 0x4C,
  };

  // LATER(sp1ff): I would expect this to work, but it throws on Debian
  // s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UTF_32, encoding::UTF_8);
  // BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UTF_32BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(s = convert_encoding<string>(buf12, sizeof(buf12),
                                                 encoding::UTF_32LE,
                                                 encoding::UTF_8),
                    iconv_error);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4LE, encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);


} // End test_charsets.

BOOST_AUTO_TEST_CASE( test_iso_639 )
{
  using namespace std;
  using namespace scribbu;

  char c2[] = { 'e', 'n' };
  language lang = language_from_iso_639_1(c2);
  BOOST_CHECK(language::eng == lang);

  unsigned char c3[3];
  language_to_iso_639_2(language::eng, c3);
  BOOST_CHECK(c3[0] == 'e' && c3[1] == 'n' && c3[2] == 'g');

  istringstream istm("chi");
  istream is(istm.rdbuf());
  is >> lang;
  BOOST_CHECK(language::chi == lang);

  stringstream os;
  os << language::chi;
  BOOST_CHECK(os.str() == "chi");

}
