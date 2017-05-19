#include <scribbu/charsets.hh>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( test_charsets )
{
  using namespace std;
  using namespace scribbu;

  string s;

  // Let's test some edge cases, first...
  s = convert_encoding<string>(0, 0, encoding::UTF_8, encoding::ISO_8859_1);
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
  // UTF-16 *and* UCS2 Little Endian encoding of "Hello, 世界" with BOM
  unsigned char buf5[] = {
    0xff, 0xfe, 0x48, 0x00, 0x65, 0x00, 0x6C, 0x00,
    0x6C, 0x00, 0x6F, 0x00, 0x2C, 0x00, 0x20, 0x00,
    0x16, 0x4e, 0x4c, 0x75,
  };

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // Should fail:
  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UTF_16BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf5, sizeof(buf5), encoding::UCS_2BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  // UTF-16 *and* UCS2 Little Endian encoding of "Hello, 世界" with *no* BOM
  unsigned char buf6[] = {
    0x48, 0x00, 0x65, 0x00, 0x6C, 0x00, 0x6C, 0x00,
    0x6F, 0x00, 0x2C, 0x00, 0x20, 0x00, 0x16, 0x4e,
    0x4c, 0x75,
  };

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // Should fail:
  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UTF_16BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf6, sizeof(buf6), encoding::UCS_2BE,
                               encoding::UTF_8);
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

  // This one fails-- no idea why.
  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UCS_2,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf7, sizeof(buf7), encoding::UCS_2BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // Should fail:
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

  // Fails-- no idea why...
  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // This one fails-- no idea why.
  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  // Should fail:
  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UTF_16LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf8, sizeof(buf8), encoding::UCS_2LE,
                               encoding::UTF_8);
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

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UTF_32,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UTF_32LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf9, sizeof(buf9),
                                             encoding::UTF_32BE,
                                             encoding::UTF_8),
                    iconv_error);

  // Throws -- no idea why
  BOOST_CHECK_THROW(convert_encoding<string>(buf9, sizeof(buf9),
                                             encoding::UCS_4,
                                             encoding::UTF_8),
                    iconv_error);

  s = convert_encoding<string>(buf9, sizeof(buf9), encoding::UCS_4LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf9, sizeof(buf9),
                                             encoding::UCS_4BE,
                                             encoding::UTF_8),
                    iconv_error);

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

  BOOST_CHECK_THROW(convert_encoding<string>(buf10, sizeof(buf10),
                                             encoding::UTF_32LE,
                                             encoding::UTF_8),
                    iconv_error);

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UCS_4,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf10, sizeof(buf10), encoding::UCS_4BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf10, sizeof(buf10),
                                             encoding::UCS_4LE,
                                             encoding::UTF_8),
                    iconv_error);

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

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UTF_32,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UTF_32LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf11, sizeof(buf11),
                                             encoding::UTF_32BE,
                                             encoding::UTF_8),
                    iconv_error);

  // Fails-- no idea why
  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UCS_4,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);

  s = convert_encoding<string>(buf11, sizeof(buf11), encoding::UCS_4LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf11, sizeof(buf11),
                               encoding::UCS_4BE, encoding::UTF_8);
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

  // Throws-- no idea hy
  BOOST_CHECK_THROW(convert_encoding<string>(buf12, sizeof(buf12),
                                             encoding::UTF_32,
                                             encoding::UTF_8),
                     iconv_error);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UTF_32BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  BOOST_CHECK_THROW(convert_encoding<string>(buf12, sizeof(buf12),
                                             encoding::UTF_32LE,
                                             encoding::UTF_8),
                    iconv_error);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4BE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" == s);

  s = convert_encoding<string>(buf12, sizeof(buf12), encoding::UCS_4LE,
                               encoding::UTF_8);
  BOOST_CHECK("Hello, 世界" != s);


} // End test_charsets.
