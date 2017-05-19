#include <scribbu/id3v2-utils.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/id3v2.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_looking_at )
{
  using namespace scribbu;

  const fs::path TEST_DATA_V1("/vagrant/test/data/id3v1.tag");
  const fs::path TEST_DATA_V2_2("/vagrant/test/data/id3v2.2.tag");
  const fs::path TEST_DATA_V2_3("/vagrant/test/data/id3v2.3.tag");
  const fs::path TEST_DATA_V2_4("/vagrant/test/data/id3v2.4.tag");

  fs::ifstream ifsv1(TEST_DATA_V1, fs::ifstream::binary);
  id3v2_info I = looking_at_id3v2(ifsv1);
  BOOST_CHECK( ! I.present_ );

  fs::ifstream ifsv2_2(TEST_DATA_V2_2, fs::ifstream::binary);
  I = looking_at_id3v2(ifsv2_2);
  BOOST_CHECK( I.present_ );
  BOOST_CHECK(2 == I.version_);
  BOOST_CHECK(0 == I.revision_);
  BOOST_CHECK(0 == I.flags_ );
  BOOST_CHECK(2192 == I.size_);
  BOOST_CHECK(0 == ifsv2_2.tellg());

  fs::ifstream ifsv2_3(TEST_DATA_V2_3, fs::ifstream::binary);
  I = looking_at_id3v2(ifsv2_3);
  BOOST_CHECK( I.present_ );
  BOOST_CHECK(3 == I.version_);
  BOOST_CHECK(0 == I.revision_);
  BOOST_CHECK(0 == I.flags_);
  BOOST_CHECK(452951 == I.size_);
  BOOST_CHECK(0 == ifsv2_3.tellg());

  fs::ifstream ifsv2_4(TEST_DATA_V2_4, fs::ifstream::binary);
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

  unsigned char buf[] = {
    0x01, 0xff, 0x00, 0x02, 0x03, 0x04, 0xff, 0x00, 0x05, 0x06, 0xff, 0x00,
    0x07, 0xff, 0x00
  };

  BOOST_CHECK( 11 == resynchronise(buf, sizeof(buf)) );

  const unsigned char GOLD[] = {
    0x01, 0xff, 0x02, 0x03, 0x04, 0xff, 0x05, 0x06, 0xff, 0x07, 0xff
  };

  BOOST_CHECK( 0 == memcmp(buf, GOLD, sizeof(GOLD)) );

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

  const fs::path TEST_DATA("/vagrant/test/data/红颜旧.mp3");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
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

  ifs.open(TEST_DATA, fs::ifstream::binary);
  std::vector<std::unique_ptr<id3v2_tag>> V;
  read_all_id3v2(ifs, std::back_inserter(V));
  ifs.close();

  BOOST_CHECK(2 == V.size());
}
