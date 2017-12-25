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

  fs::ifstream ifsv2_4(DATA, fs::ifstream::binary);

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

