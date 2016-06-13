#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v24.hh>

namespace fs = boost::filesystem;

/**
 *
 *
 \code

   vagrant@vagrant-ubuntu-trusty-64:/vagrant/test/data$ od -A x -t x1z -N 1024 id3v2.4.tag
   000000 49 44 33 04 00 00 00 00 08 43 54 50 45 31 00 00  >ID3......CTPE1..<
   000010 00 0e 00 00 00 4a 6f 61 6f 20 47 69 6c 62 65 72  >.....Joao Gilber<
   000020 74 6f 54 49 54 32 00 00 00 09 00 00 00 41 63 61  >toTIT2.......Aca<
   000030 70 75 6c 63 6f 54 41 4c 42 00 00 00 0e 00 00 00  >pulcoTALB.......<
   000040 45 6c 61 20 45 20 43 61 72 69 6f 63 61 00 00 00  >Ela E Carioca...<
   000050 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000400

 \endcode
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
   000400

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
  using scribbu::id3v2_4_tag;

  std::vector< id3v2_4_tag::frame_parser_registration > regs;
  id3v2_4_tag::get_default_frame_parsers( std::back_inserter(regs) );
  BOOST_CHECK( 0 != regs.size() );

  const fs::path TEST_DATA_V2_4("/vagrant/test/data/id3v2.4.tag");

  fs::ifstream ifsv2_4(TEST_DATA_V2_4, fs::ifstream::binary);

  id3v2_4_tag tagv2_4(ifsv2_4);
  BOOST_CHECK(4 == tagv2_4.version());
  BOOST_CHECK(0 == tagv2_4.revision());
  BOOST_CHECK(1091 == tagv2_4.size());
  BOOST_CHECK(0 == tagv2_4.flags());
  BOOST_CHECK(!tagv2_4.unsynchronised());

  BOOST_TEST_MESSAGE( "Album: '" << tagv2_4.album() << "'." );
  BOOST_TEST_MESSAGE( "Artist: '" << tagv2_4.artist() << "'." );
  BOOST_TEST_MESSAGE( "Title: '" << tagv2_4.title() << "'." );

  BOOST_CHECK("Ela E Carioca" == tagv2_4.album());
  BOOST_CHECK("Joao Gilberto" == tagv2_4.artist());
  BOOST_CHECK("Acapulco" == tagv2_4.title());

} // End test_id3v2_4_tag.
