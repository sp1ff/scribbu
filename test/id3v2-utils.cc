#include <scribbu/id3v2-utils.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_maybe_read_id3 )
{
  using scribbu::id3v2_tag;

  const fs::path TEST_FILE_V2_2("/vagrant/test/data/id3v2.2.tag");
  const fs::path TEST_FILE_V2_3("/vagrant/test/data/lorca.mp3");
  const fs::path TEST_FILE_V2_4("/vagrant/test/data/id3v2.4.tag");

  fs::ifstream ifs(TEST_FILE_V2_2);
  std::unique_ptr<id3v2_tag> ptag = scribbu::maybe_read_id3v2(ifs);

  BOOST_CHECK((bool)ptag);
  BOOST_CHECK(2 == ptag->version());
  BOOST_CHECK(0 == ptag->revision());
  BOOST_CHECK(2192 == ptag->size());
  BOOST_CHECK(0 == ptag->flags());
  BOOST_CHECK(!ptag->unsynchronised());
  BOOST_CHECK("Mnemosyne's March (Demo)" == ptag->album()        );
  BOOST_CHECK("Murley Braid Quartet"     == ptag->artist()       );

  ifs.close();
  ifs.open(TEST_FILE_V2_3);
  ptag = scribbu::maybe_read_id3v2(ifs);

  BOOST_CHECK((bool)ptag);
  BOOST_CHECK(3 == ptag->version());
  BOOST_CHECK(0 == ptag->revision());
  BOOST_CHECK(452951 == ptag->size());
  BOOST_CHECK(0 == ptag->flags());
  BOOST_CHECK(!ptag->unsynchronised());

  BOOST_CHECK("Hell's Ditch [Expanded] (US Version)" == ptag->album());
  BOOST_CHECK("The Pogues" == ptag->artist());

  ifs.close();
  ifs.open(TEST_FILE_V2_4);
  ptag = scribbu::maybe_read_id3v2(ifs);

  BOOST_CHECK((bool)ptag);
  BOOST_CHECK(4 == ptag->version());
  BOOST_CHECK(0 == ptag->revision());
  BOOST_CHECK(1091 == ptag->size());
  BOOST_CHECK(0 == ptag->flags());
  BOOST_CHECK(!ptag->unsynchronised());
  BOOST_CHECK("Joao Gilberto" == ptag->artist());
  BOOST_CHECK("Acapulco" == ptag->title());
}

BOOST_AUTO_TEST_CASE( test_read_all_id3v2 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path DATA1("/vagrant/test/data/searchresults.dat");

  fs::ifstream ifs1(DATA1, fs::ifstream::binary);

  vector<unique_ptr<id3v2_tag>> tags;
  read_all_id3v2(ifs1, back_inserter(tags));
  BOOST_CHECK( tags.empty() );

}

/**
 * \brief Test multiple ID3v2 tags
 *
 * Test data:
 *
 \code
 000000 49 44 33 03 00 00 00 00 0a 59 54 52 43 4b 00 00  >ID3......YTRCK..<
 000010 00 01 00 00 00 54 45 4e 43 00 00 00 01 40 00 00  >.....TENC....@..<
 000020 57 58 58 58 00 00 00 02 00 00 00 00 54 50 4f 53  >WXXX........TPOS<
 000030 00 00 00 01 00 00 00 54 43 4f 50 00 00 00 01 00  >.......TCOP.....<
 000040 00 00 54 4f 50 45 00 00 00 01 00 00 00 54 43 4f  >..TOPE.......TCO<
 000050 4d 00 00 00 01 00 00 00 43 4f 4d 4d 00 00 00 05  >M.......COMM....<
 000060 00 00 00 00 20 90 00 54 59 45 52 00 00 00 01 00  >.... ..TYER.....<
 000070 00 00 54 41 4c 42 00 00 00 01 00 00 00 54 50 45  >..TALB.......TPE<
 000080 31 00 00 00 01 00 00 00 54 49 54 32 00 00 00 01  >1.......TIT2....<
 000090 00 00 00 54 43 4f 4e 00 00 00 0b 00 00 00 45 6c  >...TCON.......El<
 0000a0 65 63 74 72 6f 6e 69 63 00 00 00 00 00 00 00 00  >ectronic........<
 0000b0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
 *
 000560 00 00 00 49 44 33 04 00 00 00 03 74 66 54 45 4e  >...ID3.....tfTEN<
 000570 43 00 00 00 12 00 00 00 69 54 75 6e 65 73 20 76  >C.......iTunes v<
 000580 35 2e 30 2e 30 2e 33 35 00 43 4f 4d 4d 00 00 00  >5.0.0.35.COMM...<
 000590 68 00 00 00 65 6e 67 69 54 75 6e 4e 4f 52 4d 00  >h...engiTunNORM.<
 0005a0 20 30 30 30 30 33 34 44 43 20 30 30 30 30 33 35  > 000034DC 000035<
 0005b0 34 39 20 30 30 30 31 32 30 32 39 20 30 30 30 31  >49 00012029 0001<
 0005c0 35 44 44 30 20 30 30 30 31 31 38 31 41 20 30 30  >5DD0 0001181A 00<
 0005d0 30 31 31 38 31 41 20 30 30 30 30 39 34 31 41 20  >01181A 0000941A <
 0005e0 30 30 30 30 39 39 30 38 20 30 30 30 32 41 45 44  >00009908 0002AED<
 0005f0 36 20 30 30 30 32 41 45 44 36 00 43 4f 4d 4d 00  >6 0002AED6.COMM.<
 000600 00 00 82 00 00 00 65 6e 67 69 54 75 6e 53 4d 50  >......engiTunSMP<
 000610 42 00 20 30 30 30 30 30 30 30 30 20 30 30 30 30  >B. 00000000 0000<
 000620 30 32 31 30 20 30 30 30 30 30 41 38 43 20 30 30  >0210 00000A8C 00<
 000630 30 30 30 30 30 30 30 30 43 39 34 41 36 34 20 30  >00000000C94A64 0<
 000640 30 30 30 30 30 30 30 20 30 30 30 30 30 30 30 30  >0000000 00000000<
 000650 20 30 30 30 30 30 30 30 30 20 30 30 30 30 30 30  > 00000000 000000<
 000660 30 30 20 30 30 30 30 30 30 30 30 20 30 30 30 30  >00 00000000 0000<
 000670 30 30 30 30 20 30 30 30 30 30 30 30 30 20 30 30  >0000 00000000 00<
 000680 30 30 30 30 30 30 00 43 4f 4d 4d 00 00 00 3f 00  >000000.COMM...?.<
 000690 00 00 65 6e 67 69 54 75 6e 65 73 5f 43 44 44 42  >..engiTunes_CDDB<
 0006a0 5f 31 00 32 37 30 35 31 41 30 35 2b 39 38 31 31  >_1.27051A05+9811<
 0006b0 33 2b 35 2b 31 35 30 2b 32 32 35 36 33 2b 34 30  >3+5+150+22563+40<
 0006c0 32 34 34 2b 36 30 32 31 35 2b 37 35 36 37 38 00  >244+60215+75678.<
 0006d0 43 4f 4d 4d 00 00 00 1e 00 00 00 65 6e 67 69 54  >COMM.......engiT<
 0006e0 75 6e 65 73 5f 43 44 44 42 5f 54 72 61 63 6b 4e  >unes_CDDB_TrackN<
 0006f0 75 6d 62 65 72 00 35 00 41 50 49 43 00 00 f8 53  >umber.5.APIC...S<
 000700 00 00 00 69 6d 61 67 65 2f 6a 70 65 67 00 00 00  >...image/jpeg...<
 000710 ff d8 ff e0 00 10 4a 46 49 46 00 01 02 01 00 48  >......JFIF.....H<
 000720 00 48 00 00 ff e1 13 12 45 78 69 66 00 00 4d 4d  >.H......Exif..MM<
 000730 00 2a 00 00 00 08 00 07 01 12 00 03 00 00 00 01  >.*..............<
 000740 00 01 00 00 01 1a 00 05 00 00 00 01 00 00 00 62  >...............b<
 000750 01 1b 00 05 00 00 00 01 00 00 00 6a 01 28 00 03  >...........j.(..<
 000760 00 00 00 01 00 02 00 00 01 31 00 02 00 00 00 1d  >.........1......<
 000770 00 00 00 72 01 32 00 02 00 00 00 14 00 00 00 8f  >...r.2..........<
 000780 87 69 00 04 00 00 00 01 00 00 00 a4 00 00 00 d0  >.i..............<
 000790 00 00 00 48 00 00 00 01 00 00 00 48 00 00 00 01  >...H.......H....<
 0007a0 41 64 6f 62 65 20 50 68 6f 74 6f 73 68 6f 70 20  >Adobe Photoshop <
 0007b0 43 53 20 4d 61 63 69 6e 74 6f 73 68 00 32 30 30  >CS Macintosh.200<
 0007c0 36 3a 30 31 3a 30 34 20 32 30 3a 32 31 3a 34 38  >6:01:04 20:21:48<
 0007d0 00 00 00 03 a0 01 00 03 00 00 00 01 00 01 00 00  >................<
 0007e0 a0 02 00 04 00 00 00 01 00 00 01 2c a0 03 00 04  >...........,....<
 0007f0 00 00 00 01 00 00 01 04 00 00 00 00 00 00 00 06  >................<
 000800 01 03 00 03 00 00 00 01 00 06 00 00 01 1a 00 05  >................<
 000810 00 00 00 01 00 00 01 1e 01 1b 00 05 00 00 00 01  >................<
 000820 00 00 01 26 01 28 00 03 00 00 00 01 00 02 00 00  >...&.(..........<
 000830 02 01 00 04 00 00 00 01 00 00 01 2e 02 02 00 04  >................<
 000840 00 00 00 01 00 00 11 dc 00 00 00 00 00 00 00 48  >...............H<
 000850 00 00 00 01 00 00 00 48 00 00 00 01 ff d8 ff e0  >.......H........<
 000860 00 10 4a 46 49 46 00 01 02 01 00 48 00 48 00 00  >..JFIF.....H.H..<
 000870 ff ed 00 0c 41 64 6f 62 65 5f 43 4d 00 01 ff ee  >....Adobe_CM....<
 000880 00 0e 41 64 6f 62 65 00 64 80 00 00 00 01 ff db  >..Adobe.d.......<
 000890 00 84 00 0c 08 08 08 09 08 0c 09 09 0c 11 0b 0a  >................<
 0008a0 0b 11 15 0f 0c 0c 0f 15 18 13 13 15 13 13 18 11  >................<
 \endcode
 *
 * Broken out:
 *
 \code
 000000 49 44 33 03 00                                   ID3 v2.3 tag
 000005                00                                no flags
 000006                   00 00 0a 59                    1369 bytes [1]
 00000a                               54 52 43 4b 00 00  >ID3......YTRCK..<
 000010 00 01 00 00 00 54 45 4e 43 00 00 00 01 40 00 00  >.....TENC....@..<
 000020 57 58 58 58 00 00 00 02 00 00 00 00 54 50 4f 53  >WXXX........TPOS<
 000030 00 00 00 01 00 00 00 54 43 4f 50 00 00 00 01 00  >.......TCOP.....<
 000040 00 00 54 4f 50 45 00 00 00 01 00 00 00 54 43 4f  >..TOPE.......TCO<
 000050 4d 00 00 00 01 00 00 00 43 4f 4d 4d 00 00 00 05  >M.......COMM....<
 000060 00 00 00 00 20 90 00 54 59 45 52 00 00 00 01 00  >.... ..TYER.....<
 000070 00 00 54 41 4c 42 00 00 00 01 00 00 00 54 50 45  >..TALB.......TPE<
 000080 31 00 00 00 01 00 00 00 54 49 54 32 00 00 00 01  >1.......TIT2....<
 000090 00 00 00 54 43 4f 4e 00 00 00 0b 00 00 00 45 6c  >...TCON.......El<
 0000a0 65 63 74 72 6f 6e 69 63 00 00 00 00 00 00 00 00  >ectronic........<
 0000b0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
 *
 000560 00 00 00 
 000563          49 44 33 04 00                          ID3 v2.4 tag
 000568                         00                       no flags
 000569                            00 03 74 66           64102 bytes
 00056d                                        54 45 4e  TENC
 000570 43
 000571    00 00 00 12                                   18 bytes
 000575                00 00                             no flags
 000577                      00                          ISO-859-1
 000578                         69 54 75 6e 65 73 20 76  iTunes v
 000580 35 2e 30 2e 30 2e 33 35 00                       5.0.0.35.
 000589                            43 4f 4d 4d           COMM
 00058d                                        00 00 00  0x68 = 104 bytes
 000590 68 
 000591    00 00                                         no flags
 000593          00                                      ISO-8859-1
 000594             65 6e 67                             eng
 000597                      69 54 75 6e 4e 4f 52 4d 00  iTunNORM
 0005a0 20 30 30 30 30 33 34 44 43 20 30 30 30 30 33 35   000034DC 000035
 0005b0 34 39 20 30 30 30 31 32 30 32 39 20 30 30 30 31  49 00012029 0001
 0005c0 35 44 44 30 20 30 30 30 31 31 38 31 41 20 30 30  5DD0 0001181A 00
 0005d0 30 31 31 38 31 41 20 30 30 30 30 39 34 31 41 20  01181A 0000941A 
 0005e0 30 30 30 30 39 39 30 38 20 30 30 30 32 41 45 44  00009908 0002AED
 0005f0 36 20 30 30 30 32 41 45 44 36 00 
 0005fb                                  43 4f 4d 4d     COMM
 0005ff                                              00  2 bytes <== ERROR
 000600 00 00 82                                         (really 0x82=130)
 000603          00 00                                   no flags
 000605                00                                ISO-8859-1
 000606                   65 6e 67                       eng
 000609                            69 54 75 6e 53 4d 50  iTunSMP
 000610 42 00 20 30 30 30 30 30 30 30 30 20 30 30 30 30  B. 00000000 0000
 000620 30 32 31 30 20 30 30 30 30 30 41 38 43 20 30 30  0210 00000A8C 00
 000630 30 30 30 30 30 30 30 30 43 39 34 41 36 34 20 30  00000000C94A64 0
 000640 30 30 30 30 30 30 30 20 30 30 30 30 30 30 30 30  0000000 00000000
 000650 20 30 30 30 30 30 30 30 30 20 30 30 30 30 30 30   00000000 000000
 000660 30 30 20 30 30 30 30 30 30 30 30 20 30 30 30 30  00 00000000 0000
 000670 30 30 30 30 20 30 30 30 30 30 30 30 30 20 30 30  0000 00000000 00
 000680 30 30 30 30 30 30 00                             000000.
 000687                      43 4f 4d 4d                 COMM
 00068b                                  00 00 00 3f     0x3f = 63 bytes
 00068f                                              00
 000690 00                                               no flags
 000691    00                                            ISO-8859-1
 000692       65 6e 67                                   eng
 000695                69 54 75 6e 65 73 5f 43 44 44 42  Tunes_CDDB
 0006a0 5f 31 00 32 37 30 35 31 41 30 35 2b 39 38 31 31  _1.27051A05+9811
 0006b0 33 2b 35 2b 31 35 30 2b 32 32 35 36 33 2b 34 30  3+5+150+22563+40
 0006c0 32 34 34 2b 36 30 32 31 35 2b 37 35 36 37 38 00  244+60215+75678.
 0006d0 43 4f 4d 4d                                      COMM
 0006d4             00 00 00 1e                          0x1e = 30 bytes
 0006d8                         00 00                    no flags
 0006da                               00                 ISO-8859-1
 0006db                                  65 6e 67        eng
 0006de                                           69 54  iT
 0006e0 75 6e 65 73 5f 43 44 44 42 5f 54 72 61 63 6b 4e  unes_CDDB_TrackN
 0006f0 75 6d 62 65 72 00 35 00                          umber.5.
 0006f8                         41 50 49 43              APIC
 0006fc                                     00 00 f8 53  (size) <== error
                                                   should be 0x00 03 70 53
 000700 00 00                                             no flags
 000702       00                                         ISO-8859-1
 000703          69 6d 61 67 65 2f 6a 70 65 67 00        image/jpeg.
 00070e                                           00     picture type
 00070f                                              00  description
 000710 ff d8 ff e0 00 10 4a 46 49 46 00 01 02 01 00 48  >......JFIF.....H<
 *
 00ff50 14 ea aa ff d9
 00ff55                54 49 54 32                       TIT2
 00ff59                            00 00 00 15           0x15 bytes
 00ff5d                                        00 00     no flags
 00ff5f                                              00  ISO-8859-1
 00ff60 4c 75 6e 63 68 20 66 6f 72 20 42 72 65 61 6b 66  Lunch for Breakf
 00ff70 61 73 74 00                                      ast.
 00ff74             54 50 45 31                          TPE1
 00ff78                         00 00 00 18              0x18 bytes
 00ff7c                                     00 00        no flags
 00ff7e                                           00     ISO-8859-1
 00ff7e                                              43  C
 00ff80 61 73 73 65 74 74 65 73 20 57 6f 6e 27 74 20 4c  assettes Won't L
 00ff90 69 73 74 65 6e 00                                isten.
 00ff96                   54 41 4c 42                    TALB
 00ff9a                               00 00 00 14        0x14 bytes
 00ff9e                                           00 00  no flags
 00ffa0 00                                               ISO-8859-1
 00ffa1    4e 6f 62 6f 64 79 27 73 20 4d 6f 76 69 6e 67  Nobody's Moving
 00ffb0 20 45 50 00
 00ffb4             54 52 43 4b                          TRCK
 00ffb8                         00 00 00 05              5 bytes
 00ffbc                                     00 00        no flags
 00ffbe                                           00     ISO-8859-1
 00ffbf                                              33  3
 00ffc0 2f 35 00                                         /5.
 00ffc3          54 44 52 43                             TDRC
 00ffc7                      00 00 00 06                 6 bytes
 00ffcb                                  00 00           no flags
 00ffcd                                        00        ISO-8859-1
 00ffce                                           32 30  20
 00ffd0 30 35 00                                         05
 00ffd3          ff fb b2 00 00 00 00 00 00 37 80 00 00  .........7...
 *
 \endcode
 *
 \code
 1.

 0x0a59 = b0000 1010 0101 1001
       => b 000 1010  101 1001
        = b00010101011001
        = b0000 0101 0101 1001
        = 0x05 59
        = 1369
 
 \endcode
 *          
 \code
 2.

  0x00 03 74 66 = b0000 0011 0111 0100 0110 0110
               -> b 000 0011  111 0100  110 0110
                = b000001111101001100110
                = b0 0000 1111 1010 0110 0110
                = 0xfa66
                = 64102

 \endcode
 * 
 * The second comment tag *is* in fact 0x82 bytes in size, but whoever
 * wrote the tag forgot to make the value sync-safe:
 *
 \code

  0x00 00 00 82 = b0000 0000 0000 0000 0000 0000 1000 0010
  -> 0... 000 0001 000 0010
  -> 0... 0000 0001 0000 0010
  -> 0... 0102 = 0x00 00 01 02
 \endcode
 *
 * 
 */

BOOST_AUTO_TEST_CASE( test_multi_id3v2 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path DATA1("/vagrant/test/data/lunch4bfast.mp3");
  const fs::path DATA2("/vagrant/test/data/lunch4bfast2.mp3");
  const fs::path DATA3("/vagrant/test/data/rock-the-joint.id3v2.3.tag");

  fs::ifstream ifs1(DATA1, fs::ifstream::binary);

  vector<unique_ptr<id3v2_tag>> tags;
  read_all_id3v2(ifs1, back_inserter(tags));
  BOOST_CHECK(1 == tags.size());

  fs::ifstream ifs2(DATA2, fs::ifstream::binary);

  tags.erase(tags.begin(), tags.end());
  read_all_id3v2(ifs2, back_inserter(tags));
  BOOST_CHECK(2 == tags.size());

  fs::ifstream ifs3(DATA3, fs::ifstream::binary);

  tags.erase(tags.begin(), tags.end());
  read_all_id3v2(ifs3, back_inserter(tags));
  BOOST_CHECK(1 == tags.size());

}

BOOST_AUTO_TEST_CASE( test_template_text )
{
  const fs::path TEST_FILE("/vagrant/test/data/lorca.mp3");

  using scribbu::template_processor;

  template_processor P1("%(title:output=utf-8&compress) - %A( \\(%(album)\\))?%E");
  std::string S = P1(TEST_FILE);
  BOOST_CHECK("Lorca's Novena - Pogues, The (Hell's Ditch [Expanded] (US Version)).mp3" == S);

  // TODO: Build out template_processor unit tests, if it stays in the lib...
}
