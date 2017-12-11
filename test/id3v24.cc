#include <scribbu/id3v24.hh>

#include "unit.hh"

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;

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

  fs::ifstream ifsv2_4(DATA1, fs::ifstream::binary);

  id3v2_4_tag tag(ifsv2_4);
  BOOST_CHECK(4 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(1091 == tag.size());
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

