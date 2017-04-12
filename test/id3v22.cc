#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v22.hh>

namespace fs = boost::filesystem;

/**
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
 */

BOOST_AUTO_TEST_CASE( test_id3v2_2_tag )
{
  using scribbu::comments;
  using scribbu::id3v2_2_tag;

  using std::vector;
  using std::back_inserter;

  std::vector< id3v2_2_tag::frame_parser_registration > regs;
  id3v2_2_tag::get_default_frame_parsers( std::back_inserter(regs) );
  BOOST_CHECK( 0 != regs.size() );

  const fs::path TEST_DATA_V2_2("/vagrant/test/data/id3v2.2.tag");

  fs::ifstream ifsv2_2(TEST_DATA_V2_2, fs::ifstream::binary);

  id3v2_2_tag tagv2_2(ifsv2_2);

  BOOST_CHECK(2 == tagv2_2.version());
  BOOST_CHECK(0 == tagv2_2.revision());
  BOOST_CHECK(2192 == tagv2_2.size());
  BOOST_CHECK(0 == tagv2_2.flags());
  BOOST_CHECK(!tagv2_2.unsynchronised());

  BOOST_TEST_MESSAGE( "Album: '" << tagv2_2.album() << "'." );
  BOOST_TEST_MESSAGE( "Artist: '" << tagv2_2.artist() << "'." );
  BOOST_TEST_MESSAGE( "Content Type: '" << tagv2_2.content_type() << "'." );
  BOOST_TEST_MESSAGE( "Encoded by: '" << tagv2_2.encoded_by() << "'." );
  BOOST_TEST_MESSAGE( "Title: '" << tagv2_2.title() << "'." );

  BOOST_CHECK("Mnemosyne's March (Demo)" == tagv2_2.album());
  BOOST_CHECK("Murley Braid Quartet" == tagv2_2.artist());
  BOOST_CHECK("(8)" == tagv2_2.content_type());
  BOOST_CHECK("iTunes v6.0.4" == tagv2_2.encoded_by());
  BOOST_CHECK("Sheep Walking" == tagv2_2.title());

  vector<comments> C;
  tagv2_2.get_all_comments(back_inserter(C));
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
  const comments &C0 = C[0];
  char lang[3];
  vector<unsigned char> dsc, text;
  BOOST_CHECK(0 == C0.unicode());
  C0.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  C0.description(back_inserter(dsc));
  BOOST_CHECK(dsc == DSC0);
  C0.text(back_inserter(text));
  BOOST_CHECK(text == TXT0);

  dsc.resize(0);
  text.resize(0);

  const vector<unsigned char> DSC1({{'i', 'T', 'u', 'n', 'S', 'M', 'P', 'B'}});
  const vector<unsigned char> TXT1({{' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '2', '1', '0', ' ', '0', '0', '0', '0', '0', '7', 'A', '2', ' ', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '4', 'A', '6', 'C', '4', 'E', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', ' ', '0', '0', '0', '0', '0', '0', '0', '0', 0}});

  const comments &C1 = C[1];
  BOOST_CHECK(0 == C1.unicode());
  C1.lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  C1.description(back_inserter(dsc));
  BOOST_CHECK(dsc == DSC1);
  C1.text(back_inserter(text));
  BOOST_CHECK(text == TXT1);


  std::stringstream stm;
  stm << tagv2_2;
  std::string s = stm.str();

  BOOST_CHECK( s == "ID3v2.2 tag" );

} // End test_id3v2_2_tag.
