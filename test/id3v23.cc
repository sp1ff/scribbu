#include <scribbu/id3v23.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;

/**
 * \brief Test id3v2.3.tag (Lorca's Novena)
 *
 *
 * Test data: this is a big one-- here's the first kilobyte:
 *
 \code

   mgh@Crickhollow[2-0:...ts/scribbu/test/data]: od -A x -t x1z -N 1024 id3v2.3.tag
   000000 49 44 33 03 00 00 00 1b 52 57 54 49 54 32 00 00  >ID3.....RWTIT2..<
   000010 00 0f 00 00 00 4c 6f 72 63 61 27 73 20 4e 6f 76  >.....Lorca's Nov<
   000020 65 6e 61 54 50 45 31 00 00 00 0b 00 00 00 54 68  >enaTPE1.......Th<
   000030 65 20 50 6f 67 75 65 73 54 41 4c 42 00 00 00 25  >e PoguesTALB...%<
   000040 00 00 00 48 65 6c 6c 27 73 20 44 69 74 63 68 20  >...Hell's Ditch <
   000050 5b 45 78 70 61 6e 64 65 64 5d 20 28 55 53 20 56  >[Expanded] (US V<
   000060 65 72 73 69 6f 6e 29 54 43 4f 4e 00 00 00 04 00  >ersion)TCON.....<
   000070 00 00 50 6f 70 54 43 4f 4d 00 00 00 03 00 00 01  >..PopTCOM.......<
   000080 ff fe 54 50 45 33 00 00 00 03 00 00 01 ff fe 54  >..TPE3.........T<
   000090 52 43 4b 00 00 00 02 00 00 00 35 54 59 45 52 00  >RCK.......5TYER.<
   0000a0 00 00 05 00 00 00 31 39 39 30 54 50 45 32 00 00  >......1990TPE2..<
   0000b0 00 0b 00 00 00 54 68 65 20 50 6f 67 75 65 73 43  >.....The PoguesC<
   0000c0 4f 4d 4d 00 00 00 44 00 00 01 65 6e 67 ff fe 00  >OMM...D...eng...<
   0000d0 00 ff fe 41 00 6d 00 61 00 7a 00 6f 00 6e 00 2e  >...A.m.a.z.o.n..<
   0000e0 00 63 00 6f 00 6d 00 20 00 53 00 6f 00 6e 00 67  >.c.o.m. .S.o.n.g<
   0000f0 00 20 00 49 00 44 00 3a 00 20 00 32 00 30 00 33  >. .I.D.:. .2.0.3<
   000100 00 35 00 35 00 38 00 32 00 35 00 34 00 54 43 4f  >.5.5.8.2.5.4.TCO<
   000110 50 00 00 00 35 00 00 01 ff fe 32 00 30 00 30 00  >P...5.....2.0.0.<
   000120 34 00 20 00 57 00 61 00 72 00 6e 00 65 00 72 00  >4. .W.a.r.n.e.r.<
   000130 20 00 4d 00 75 00 73 00 69 00 63 00 20 00 55 00  > .M.u.s.i.c. .U.<
   000140 4b 00 20 00 4c 00 74 00 64 00 2e 00 54 50 4f 53  >K. .L.t.d...TPOS<
   000150 00 00 00 02 00 00 00 31 41 50 49 43 00 01 c3 62  >.......1APIC...b<
   000160 00 00 00 69 6d 61 67 65 2f 6a 70 65 67 00 03 00  >...image/jpeg...<
   000170 ff d8 ff e0 00 10 4a 46 49 46 00 01 01 00 00 01  >......JFIF......<
   000180 00 01 00 00 ff db 00 43 00 05 03 04 04 04 03 05  >.......C........<
   000190 04 04 04 05 05 05 06 07 0c 08 07 07 07 07 0f 0b  >................<
   0001a0 0b 09 0c 11 0f 12 12 11 0f 11 11 13 16 1c 17 13  >................<
   0001b0 14 1a 15 11 11 18 21 18 1a 1d 1d 1f 1f 1f 13 17  >......!.........<
   0001c0 22 24 22 1e 24 1c 1e 1f 1e ff db 00 43 01 05 05  >"$".$.......C...<
   0001d0 05 07 06 07 0e 08 08 0e 1e 14 11 14 1e 1e 1e 1e  >................<
   0001e0 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e  >................<
   *
   000200 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e 1e ff c0  >................<
   000210 00 11 08 01 f4 01 f4 03 01 22 00 02 11 01 03 11  >........."......<
   000220 01 ff c4 00 1c 00 00 01 04 03 01 00 00 00 00 00  >................<
   000230 00 00 00 00 00 00 06 03 04 05 07 00 02 08 01 ff  >................<
   000240 c4 00 56 10 00 02 01 03 04 01 02 04 03 05 04 07  >..V.............<
   000250 03 07 06 0f 01 02 03 04 05 11 00 06 12 21 31 13  >.............!1.<
   000260 41 07 14 22 51 32 61 71 15 23 42 81 91 16 52 a1  >A.."Q2aq.#B...R.<
   000270 b1 08 24 33 62 c1 d1 d2 43 72 f0 17 25 34 82 a2  >..$3b...Cr..%4..<
   000280 e1 f1 53 55 63 73 93 a3 b2 c2 35 36 44 45 54 64  >..SUcs....56DETd<
   000290 74 84 92 94 a4 b3 27 ff c4 00 1b 01 00 02 03 01  >t.....'.........<
   0002a0 01 01 00 00 00 00 00 00 00 00 00 00 03 04 01 02  >................<
   0002b0 05 00 06 07 ff c4 00 40 11 00 01 03 02 03 04 07  >.......@........<
   0002c0 07 03 04 01 04 02 03 01 00 01 00 02 03 04 11 12  >................<
   0002d0 21 31 05 41 51 91 13 22 61 71 b1 d1 f0 14 32 52  >!1.AQ.."aq....2R<
   0002e0 81 a1 c1 e1 06 15 53 23 42 92 f1 62 16 24 33 72  >......S#B..b.$3r<
   0002f0 34 43 82 a2 b2 c2 ff da 00 0c 03 01 00 02 11 03  >4C..............<
   000300 11 00 3f 00 b1 13 73 6e 49 15 99 2f 37 13 83 83  >..?...snI../7...<
   000310 8a 87 ff 00 9e b2 3d c5 b9 15 80 6b cd d0 61 8e  >......=....k..a.<
   000320 49 a9 73 9f f1 d3 6f 9c a7 48 89 44 2a 7c 1f c8  >I.s...o..H.D*|..<
   000330 eb 24 ae 89 48 76 63 c3 5f 2d f6 89 be 23 cc af  >.$..Hvc._-...#..<
   000340 79 d1 b3 e0 1c 93 d3 b9 b7 22 c8 31 77 b8 f1 07  >y........".1w...<
   000350 07 35 0f df f8 e9 bc 9b a7 73 e1 f1 77 b8 8f 6c  >.5.......s..w..l<
   000360 fc cb e3 ed f7 d2 14 d7 5a 4e 4d fb ec a9 f1 ef  >........ZNM.....<
   000370 df df 5b 9a ea 66 0a a5 5d 90 76 7e 9e bf 5c ea  >..[..f..].v~..\.<
   000380 7a 79 be 23 cc a9 e8 d8 3f b0 72 4a 26 e7 dc dc  >zy.#....?.rJ&...<
   000390 39 35 ea e0 70 3b 06 a5 c6 7f c7 58 bb 97 73 b6  >95..p;.....X..s.<
   0003a0 19 af 37 20 a4 67 aa a7 1f f1 d2 6d 5d 4d ea 71  >..7 .g.....m]M.q<
   0003b0 0f 85 51 df db 5a c3 57 0a 1f c6 3e d8 c6 a3 a7  >..Q..Z.W...>....<
   0003c0 9b e2 3c ca 9e 8d 9f 00 e4 b6 6d c3 ba 0e 1f f6  >..<.......m.....<
   0003d0 f5 d1 53 39 ff 00 d2 9f c7 f5 d6 c2 fb ba c7 d5  >..S9............<
   0003e0 fb 72 ea ca 7d c5 4b f5 d7 eb ad 5a ae 32 4f d6  >.r..}.K....Z.2O.<
   0003f0 30 07 b7 8d 3d 8e a6 26 88 33 4a 80 7e 67 50 6a  >0...=..&.3J.~gPj<
   000400

  \endcode
  *
  * This is tough to read in the raw due to the APIC frame (Attached PICture)
  * at offset 0x0158. This frame is 115554 (0x01c362) bytes in size. 0x0158 +
  * 0x1c362 + 10 bytes (for the frame header) gives 0x1c4c4 = 115908. Resuming
  * the hex dump:
  *
  \code

   vagrant@vagrant-ubuntu-trusty-64:/vagrant/test/data$ od -A x -t x1z -j 115908 id3v2.3.tag
   01c4c4 50 52 49 56 00 00 04 62 00 00 77 77 77 2e 61 6d  >PRIV...b..www.am<
   01c4d4 61 7a 6f 6e 2e 63 6f 6d 00 3c 3f 78 6d 6c 20 76  >azon.com.<?xml v<
   01c4e4 65 72 73 69 6f 6e 3d 22 31 2e 30 22 20 65 6e 63  >ersion="1.0" enc<
   01c4f4 6f 64 69 6e 67 3d 22 55 54 46 2d 38 22 3f 3e 0a  >oding="UTF-8"?>.<
   01c504 3c 75 69 74 73 3a 55 49 54 53 20 78 6d 6c 6e 73  ><uits:UITS xmlns<
   01c514 3a 78 73 69 3d 22 68 74 74 70 3a 2f 2f 77 77 77  >:xsi="http://www<
   01c524 2e 77 33 2e 6f 72 67 2f 32 30 30 31 2f 58 4d 4c  >.w3.org/2001/XML<
   01c534 53 63 68 65 6d 61 2d 69 6e 73 74 61 6e 63 65 22  >Schema-instance"<
   01c544 20 78 6d 6c 6e 73 3a 75 69 74 73 3d 22 68 74 74  > xmlns:uits="htt<
   01c554 70 3a 2f 2f 77 77 77 2e 75 64 69 72 65 63 74 6f  >p://www.udirecto<
   01c564 72 2e 6e 65 74 2f 73 63 68 65 6d 61 73 2f 32 30  >r.net/schemas/20<
   01c574 30 39 2f 75 69 74 73 2f 31 2e 31 22 3e 3c 6d 65  >09/uits/1.1"><me<
   01c584 74 61 64 61 74 61 3e 3c 6e 6f 6e 63 65 3e 68 44  >tadata><nonce>hD<
   01c594 51 67 72 54 7a 42 3c 2f 6e 6f 6e 63 65 3e 3c 44  >QgrTzB</nonce><D<
   01c5a4 69 73 74 72 69 62 75 74 6f 72 3e 41 6d 61 7a 6f  >istributor>Amazo<
   01c5b4 6e 2e 63 6f 6d 3c 2f 44 69 73 74 72 69 62 75 74  >n.com</Distribut<
   01c5c4 6f 72 3e 3c 54 69 6d 65 3e 31 39 37 30 2d 30 31  >or><Time>1970-01<
   01c5d4 2d 30 31 54 30 30 3a 30 30 3a 30 30 5a 3c 2f 54  >-01T00:00:00Z</T<
   01c5e4 69 6d 65 3e 3c 50 72 6f 64 75 63 74 49 44 20 74  >ime><ProductID t<
   01c5f4 79 70 65 3d 22 55 50 43 22 20 63 6f 6d 70 6c 65  >ype="UPC" comple<
   01c604 74 65 64 3d 22 66 61 6c 73 65 22 3e 30 38 31 32  >ted="false">0812<
   01c614 32 37 34 30 36 37 36 39 3c 2f 50 72 6f 64 75 63  >27406769</Produc<
   01c624 74 49 44 3e 3c 41 73 73 65 74 49 44 20 74 79 70  >tID><AssetID typ<
   01c634 65 3d 22 49 53 52 43 22 3e 47 42 41 48 54 30 34  >e="ISRC">GBAHT04<
   01c644 30 30 32 34 35 3c 2f 41 73 73 65 74 49 44 3e 3c  >00245</AssetID><<
   01c654 54 49 44 20 76 65 72 73 69 6f 6e 3d 22 31 22 3e  >TID version="1"><
   01c664 37 33 38 31 39 2d 32 34 36 39 30 30 38 36 34 34  >73819-2469008644<
   01c674 34 33 39 33 32 31 31 32 35 31 34 32 37 36 30 30  >4393211251427600<
   01c684 30 3c 2f 54 49 44 3e 3c 4d 65 64 69 61 20 61 6c  >0</TID><Media al<
   01c694 67 6f 72 69 74 68 6d 3d 22 53 48 41 32 35 36 22  >gorithm="SHA256"<
   01c6a4 3e 32 64 37 33 38 37 61 35 34 32 30 38 30 38 39  >>2d7387a54208089<
   01c6b4 33 30 35 33 66 34 65 30 39 32 64 65 37 32 36 39  >3053f4e092de7269<
   01c6c4 66 62 32 36 33 63 65 38 33 34 61 31 65 38 34 31  >fb263ce834a1e841<
   01c6d4 36 65 32 38 32 64 65 30 36 66 38 31 61 37 38 30  >6e282de06f81a780<
   01c6e4 33 3c 2f 4d 65 64 69 61 3e 3c 50 41 3e 75 6e 73  >3</Media><PA>uns<
   01c6f4 70 65 63 69 66 69 65 64 3c 2f 50 41 3e 3c 43 6f  >pecified</PA><Co<
   01c704 70 79 72 69 67 68 74 3e 3c 2f 43 6f 70 79 72 69  >pyright></Copyri<
   01c714 67 68 74 3e 3c 45 78 74 72 61 20 74 79 70 65 3d  >ght><Extra type=<
   01c724 22 54 72 61 6e 73 61 63 74 69 6f 6e 54 79 70 65  >"TransactionType<
   01c734 22 3e 44 6f 77 6e 6c 6f 61 64 20 2d 20 50 61 69  >">Download - Pai<
   01c744 64 3c 2f 45 78 74 72 61 3e 3c 2f 6d 65 74 61 64  >d</Extra></metad<
   01c754 61 74 61 3e 3c 73 69 67 6e 61 74 75 72 65 20 61  >ata><signature a<
   01c764 6c 67 6f 72 69 74 68 6d 3d 22 52 53 41 32 30 34  >lgorithm="RSA204<
   01c774 38 22 20 63 61 6e 6f 6e 69 63 61 6c 69 7a 61 74  >8" canonicalizat<
   01c784 69 6f 6e 3d 22 6e 6f 6e 65 22 20 6b 65 79 49 44  >ion="none" keyID<
   01c794 3d 22 64 64 30 61 66 32 39 62 34 31 63 64 37 64  >="dd0af29b41cd7d<
   01c7a4 36 64 38 32 35 39 33 63 61 66 31 62 61 39 65 61  >6d82593caf1ba9ea<
   01c7b4 61 36 62 37 35 36 33 38 33 66 22 3e 4e 72 47 41  >a6b756383f">NrGA<
   01c7c4 45 48 4e 2f 58 58 75 65 34 69 64 5a 52 55 43 4f  >EHN/XXue4idZRUCO<
   01c7d4 54 78 46 31 77 57 7a 67 49 74 47 2b 2b 2b 49 48  >TxF1wWzgItG+++IH<
   01c7e4 4b 59 4c 4e 45 4d 7a 78 77 77 32 52 38 53 6b 52  >KYLNEMzxww2R8SkR<
   01c7f4 2b 2b 64 4f 61 41 4b 63 4a 34 50 35 62 6f 45 69  >++dOaAKcJ4P5boEi<
   01c804 41 57 50 64 47 67 53 4b 63 56 69 34 54 63 31 33  >AWPdGgSKcVi4Tc13<
   01c814 46 78 32 50 32 32 36 6f 31 69 6f 33 39 7a 78 55  >Fx2P226o1io39zxU<
   01c824 31 52 41 44 43 52 6e 44 6d 35 32 69 52 37 47 34  >1RADCRnDm52iR7G4<
   01c834 67 49 71 68 51 6e 42 6a 30 2b 72 34 37 4a 4a 64  >gIqhQnBj0+r47JJd<
   01c844 51 67 74 32 4e 4c 58 5a 48 70 2b 37 6e 74 77 61  >Qgt2NLXZHp+7ntwa<
   01c854 52 6f 67 47 47 49 63 56 68 58 34 44 71 67 59 78  >RogGGIcVhX4DqgYx<
   01c864 6f 30 2f 64 42 67 6e 69 4e 31 7a 4a 47 4f 78 4e  >o0/dBgniN1zJGOxN<
   01c874 31 72 61 47 7a 42 39 45 51 49 36 43 58 33 76 58  >1raGzB9EQI6CX3vX<
   01c884 54 35 4d 43 31 35 2f 73 5a 56 46 5a 79 38 49 56  >T5MC15/sZVFZy8IV<
   01c894 38 58 6d 48 6f 56 51 6c 64 51 38 42 47 76 51 77  >8XmHoVQldQ8BGvQw<
   01c8a4 36 4e 39 6e 77 41 45 6c 4a 53 2f 46 4d 37 6e 69  >6N9nwAElJS/FM7ni<
   01c8b4 70 65 58 6a 59 7a 32 4e 68 6e 7a 59 32 6f 52 6e  >peXjYz2NhnzY2oRn<

   01c8c4 4f 58 65 76 56 54 4f 50 67 70 49 58 31 69 4d 62  >OXevVTOPgpIX1iMb<
   01c8d4 48 78 4f 64 71 42 39 31 65 6e 7a 72 34 5a 7a 30  >HxOdqB91enzr4Zz0<
   01c8e4 74 65 42 53 6b 76 41 59 7a 41 2f 48 4d 47 56 46  >teBSkvAYzA/HMGVF<
   01c8f4 70 59 64 57 7a 42 74 53 6d 33 62 74 7a 6a 47 44  >pYdWzBtSm3btzjGD<
   01c904 4f 6c 6a 67 31 65 79 4e 51 69 76 6f 30 41 37 62  >Oljg1eyNQivo0A7b<
   01c914 79 41 3d 3d 3c 2f 73 69 67 6e 61 74 75 72 65 3e  >yA==</signature><
   01c924 3c 2f 75 69 74 73 3a 55 49 54 53 3e 00 00 00 00  ></uits:UITS>....<
   01c934 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   06e954 00 00 00 00 00 00 00 00 00 00 00 00 00           >.............<
   06e961
   *
 \endcode
 *
 * Broken out, we have the ID3v2 header:
 *
 \code

   000000 49 44 33                                         ID3
   000003          03 00                                   ID3v2 version 3, revision 0
   000005                00                                no unsync, no extended header, not experimental
   000006                   00 1b 52 57                    tag size is 452951 bytes [1]
   00000a                               54 49 54 32        TIT2 frame (Title/songname/content description)
   00000e                                           00 00  frame is 15 bytes in size
   000010 00 0f
   000012       00 00                                      no flags
   000014             00                                   ISO-8859-1 encoding
   000015                4c 6f 72 63 61 27 73 20 4e 6f 76  "Lorca's Novena"
   000020 65 6e 61
   000023          54 50 45 31                             TPE1 frame (Lead performer(s)/Soloist(s))
   000027                      00 00 00 0b                 frame is 11 bytes in size
   00002b                                  00 00           no flags
   00002d                                        00        ISO-8859-1 encoding
   00002e                                           54 68  "The Pogues"
   000030 65 20 50 6f 67 75 65 73
   000038                         54 41 4c 42              TALB frame (Album/Movie/Show title)
   000030                                     00 00 00 25  frame is 37 bytes in size
   000040 00 00                                            no flags
   000042       00                                         ISO-8859-1 encoding
   000043          48 65 6c 6c 27 73 20 44 69 74 63 68 20  "Hell's Ditch [Expanded] (US Version)"
   000040 00 00 00 48 65 6c 6c 27 73 20 44 69 74 63 68 20
   000050 5b 45 78 70 61 6e 64 65 64 5d 20 28 55 53 20 56
   000060 65 72 73 69 6f 6e 29
   000067                      54 43 4f 4e                 TCON frame (Content type)
   00006b                                  00 00 00 04     frame is 4 bytes in size
   00006f                                              00  no flags
   000070 00
   000071    00                                            ISO-8859-1 encoding
   000072       50 6f 70                                   "Pop"
   000075                54 43 4f 4d                       TCOM frame (Composer)
   000079                            00 00 00 03           frame is 3 bytes in size
   00007d                                        00 00     no frame flags
   00007f                                              01  UCS2-encoded
   000080 ff fe                                            Little Endian BOM
   000082       54 50 45 33                                TPE3 frame (Conductor/performer refinement)
   000086                   00 00 00 03                    frame is 3 bytes in size
   00008a                               00 00              no frame flags
   00008c                                     01           UCS2-encoded
   00008d                                        ff fe     Little Endian BOM
   00008f                                              54  TRCK frame (Track number/Position in set)
   000090 52 43 4b
   000093          00 00 00 02                             frame is 2 bytes in size
   000097                      00 00                       no frame flags
   000099                            00                    ISO-8859-1 encoding
   00009a                               35                 "5"
   00009b                                  54 59 45 52     TYER frame (Year)
   00009f                                              00  frame is 5 bytes in size
   0000a0 00 00 05
   0000a3          00 00                                   no frame flags
   0000a5                00                                ISO-8859-1 encoding
   0000a6                   31 39 39 30                    "1990"
   0000aa                               54 50 45 32        TPE2 frame (Band/orchestra/accompaniment)
   0000ae                                           00 00  frame is 11 bytes in size
   0000b0 00 0b
   0000b2       00 00                                      no frame flags
   0000b4             00                                   ISO-8859-1 encoding
   0000b5                54 68 65 20 50 6f 67 75 65 73     "The Pogues"
   0000bf                                              43  COMM frame (Comments)
   0000c0 4f 4d 4d
   0000c3          00 00 00 44                             frame is 68 bytes in size
   0000c7                      00 00                       no frame flags
   0000c9                            01                    UCS2-encoded
   0000ca                               65 6e 67           language is English ("eng")
   0000cd                                        ff fe     Little Endian BOM
   0000cf                                              00  description ""
   0000d0 00
   0000d1    ff fe                                         Little Endian BOM
   0000d3          41 00 6d 00 61 00 7a 00 6f 00 6e 00 2e  "Amazon.com Song I.D.: 203558254
   0000e0 00 63 00 6f 00 6d 00 20 00 53 00 6f 00 6e 00 67
   0000f0 00 20 00 49 00 44 00 3a 00 20 00 32 00 30 00 33
   000100 00 35 00 35 00 38 00 32 00 35 00 34 00
   00010d                                        54 43 4f  TCOP frame (Copyright message)
   000110 50
   000111    00 00 00 35                                   frame is 53 bytes in size
   000115                00 00                             no frame flags
   000117                      01                          UCS2-encoded
   000118                         ff fe                    Little Endian BOM
   00011a                               32 00 30 00 30 00  "2004 Warner Music UK Ltd."
   000120 34 00 20 00 57 00 61 00 72 00 6e 00 65 00 72 00
   000130 20 00 4d 00 75 00 73 00 69 00 63 00 20 00 55 00
   000140 4b 00 20 00 4c 00 74 00 64 00 2e 00
   00014c                                     54 50 4f 53  TPOS frame (Part of a set)
   000150 00 00 00 02                                      frame is 2 bytes in size
   000154             00 00                                no frame flags
   000156                   00                             ISO-8859-1 encoding
   000157                      31                          "1"
   000158                         41 50 49 43              APIC frame (Attached picture)
   00015c                                     00 01 c3 62  frame is 115554 bytes in size
   000160 00 00                                            frame flags
   000162       00                                         ISO-8859-1 encoding
   000163          69 6d 61 67 65 2f 6a 70 65 67 00        "image/jpeg"
   00016e                                           03     Picture type (Cover (front))
   00016f                                              00  Description ("")
   000170 ff d8 ff e0 00 10 4a 46 49 46 00 01 01 00 00 01  Picture data
   *
   01c4c4 50 52 49 56                                      PRIV frame (Private frame)
   01c4c8             00 00 04 62                          frame is 1122 bytes in size
   01c4cc                         00 00                    no frame flags
   01c4ce                               77 77 77 2e 61 6d  Owner/ID: "amazon.com"
   01c4d4 61 7a 6f 6e 2e 63 6f 6d 00
   01c4dd                            3c 3f 78 6d 6c 20 76  Private data [2]
   01c4e4 65 72 73 69 6f 6e 3d 22 31 2e 30 22 20 65 6e 63
   01c4f4 6f 64 69 6e 67 3d 22 55 54 46 2d 38 22 3f 3e 0a
   01c504 3c 75 69 74 73 3a 55 49 54 53 20 78 6d 6c 6e 73
   01c514 3a 78 73 69 3d 22 68 74 74 70 3a 2f 2f 77 77 77
   01c524 2e 77 33 2e 6f 72 67 2f 32 30 30 31 2f 58 4d 4c
   01c534 53 63 68 65 6d 61 2d 69 6e 73 74 61 6e 63 65 22
   01c544 20 78 6d 6c 6e 73 3a 75 69 74 73 3d 22 68 74 74
   01c554 70 3a 2f 2f 77 77 77 2e 75 64 69 72 65 63 74 6f
   01c564 72 2e 6e 65 74 2f 73 63 68 65 6d 61 73 2f 32 30
   01c574 30 39 2f 75 69 74 73 2f 31 2e 31 22 3e 3c 6d 65
   01c584 74 61 64 61 74 61 3e 3c 6e 6f 6e 63 65 3e 68 44
   01c594 51 67 72 54 7a 42 3c 2f 6e 6f 6e 63 65 3e 3c 44
   01c5a4 69 73 74 72 69 62 75 74 6f 72 3e 41 6d 61 7a 6f
   01c5b4 6e 2e 63 6f 6d 3c 2f 44 69 73 74 72 69 62 75 74
   01c5c4 6f 72 3e 3c 54 69 6d 65 3e 31 39 37 30 2d 30 31
   01c5d4 2d 30 31 54 30 30 3a 30 30 3a 30 30 5a 3c 2f 54
   01c5e4 69 6d 65 3e 3c 50 72 6f 64 75 63 74 49 44 20 74
   01c5f4 79 70 65 3d 22 55 50 43 22 20 63 6f 6d 70 6c 65
   01c604 74 65 64 3d 22 66 61 6c 73 65 22 3e 30 38 31 32
   01c614 32 37 34 30 36 37 36 39 3c 2f 50 72 6f 64 75 63
   01c624 74 49 44 3e 3c 41 73 73 65 74 49 44 20 74 79 70
   01c634 65 3d 22 49 53 52 43 22 3e 47 42 41 48 54 30 34
   01c644 30 30 32 34 35 3c 2f 41 73 73 65 74 49 44 3e 3c
   01c654 54 49 44 20 76 65 72 73 69 6f 6e 3d 22 31 22 3e
   01c664 37 33 38 31 39 2d 32 34 36 39 30 30 38 36 34 34
   01c674 34 33 39 33 32 31 31 32 35 31 34 32 37 36 30 30
   01c684 30 3c 2f 54 49 44 3e 3c 4d 65 64 69 61 20 61 6c
   01c694 67 6f 72 69 74 68 6d 3d 22 53 48 41 32 35 36 22
   01c6a4 3e 32 64 37 33 38 37 61 35 34 32 30 38 30 38 39
   01c6b4 33 30 35 33 66 34 65 30 39 32 64 65 37 32 36 39
   01c6c4 66 62 32 36 33 63 65 38 33 34 61 31 65 38 34 31
   01c6d4 36 65 32 38 32 64 65 30 36 66 38 31 61 37 38 30
   01c6e4 33 3c 2f 4d 65 64 69 61 3e 3c 50 41 3e 75 6e 73
   01c6f4 70 65 63 69 66 69 65 64 3c 2f 50 41 3e 3c 43 6f
   01c704 70 79 72 69 67 68 74 3e 3c 2f 43 6f 70 79 72 69
   01c714 67 68 74 3e 3c 45 78 74 72 61 20 74 79 70 65 3d
   01c724 22 54 72 61 6e 73 61 63 74 69 6f 6e 54 79 70 65
   01c734 22 3e 44 6f 77 6e 6c 6f 61 64 20 2d 20 50 61 69
   01c744 64 3c 2f 45 78 74 72 61 3e 3c 2f 6d 65 74 61 64
   01c754 61 74 61 3e 3c 73 69 67 6e 61 74 75 72 65 20 61
   01c764 6c 67 6f 72 69 74 68 6d 3d 22 52 53 41 32 30 34
   01c774 38 22 20 63 61 6e 6f 6e 69 63 61 6c 69 7a 61 74
   01c784 69 6f 6e 3d 22 6e 6f 6e 65 22 20 6b 65 79 49 44
   01c794 3d 22 64 64 30 61 66 32 39 62 34 31 63 64 37 64
   01c7a4 36 64 38 32 35 39 33 63 61 66 31 62 61 39 65 61
   01c7b4 61 36 62 37 35 36 33 38 33 66 22 3e 4e 72 47 41
   01c7c4 45 48 4e 2f 58 58 75 65 34 69 64 5a 52 55 43 4f
   01c7d4 54 78 46 31 77 57 7a 67 49 74 47 2b 2b 2b 49 48
   01c7e4 4b 59 4c 4e 45 4d 7a 78 77 77 32 52 38 53 6b 52
   01c7f4 2b 2b 64 4f 61 41 4b 63 4a 34 50 35 62 6f 45 69
   01c804 41 57 50 64 47 67 53 4b 63 56 69 34 54 63 31 33
   01c814 46 78 32 50 32 32 36 6f 31 69 6f 33 39 7a 78 55
   01c824 31 52 41 44 43 52 6e 44 6d 35 32 69 52 37 47 34
   01c834 67 49 71 68 51 6e 42 6a 30 2b 72 34 37 4a 4a 64
   01c844 51 67 74 32 4e 4c 58 5a 48 70 2b 37 6e 74 77 61
   01c854 52 6f 67 47 47 49 63 56 68 58 34 44 71 67 59 78
   01c864 6f 30 2f 64 42 67 6e 69 4e 31 7a 4a 47 4f 78 4e
   01c874 31 72 61 47 7a 42 39 45 51 49 36 43 58 33 76 58
   01c884 54 35 4d 43 31 35 2f 73 5a 56 46 5a 79 38 49 56
   01c894 38 58 6d 48 6f 56 51 6c 64 51 38 42 47 76 51 77
   01c8a4 36 4e 39 6e 77 41 45 6c 4a 53 2f 46 4d 37 6e 69
   01c8b4 70 65 58 6a 59 7a 32 4e 68 6e 7a 59 32 6f 52 6e
   01c8c4 4f 58 65 76 56 54 4f 50 67 70 49 58 31 69 4d 62
   01c8d4 48 78 4f 64 71 42 39 31 65 6e 7a 72 34 5a 7a 30
   01c8e4 74 65 42 53 6b 76 41 59 7a 41 2f 48 4d 47 56 46
   01c8f4 70 59 64 57 7a 42 74 53 6d 33 62 74 7a 6a 47 44
   01c904 4f 6c 6a 67 31 65 79 4e 51 69 76 6f 30 41 37 62
   01c914 79 41 3d 3d 3c 2f 73 69 67 6e 61 74 75 72 65 3e
   01c924 3c 2f 75 69 74 73 3a 55 49 54 53 3e

   01c930 00...  <== padding begins here
   06e961 <== tag ends here [3]

   1. 00 1b 52 57 = b0000 0000 0001 1011 0101 0010 0101 0111 =>
      b 000 0000 001 1011 101 0010 101 0111 =>
      b 0000 0000 0110 1110 1001 0101 0111 =
      0x006e957 = 452951

   2. <?xml version="1.0" encoding="UTF-8"?>\x0a<uits:UITS xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:uits="http://www.udirector.net/schemas/2009/uits/1.1"><metadata><nonce>hDQgrTzB</nonce><Distributor>Amazon.com</Distributor><Time>1970-01-01T00:00:00Z</Time><ProductID type="UPC" completed="false">081227406769</ProductID><AssetID type="ISRC">GBAHT0400245</AssetID><TID version="1">73819-246900864443932112514276000</TID><Media algorithm="SHA256">2d7387a542080893053f4e092de7269fb263ce834a1e8416e282de06f81a7803</Media><PA>unspecified</PA><Copyright></Copyright><Extra type="TransactionType">Download - Paid</Extra></metadata><signature algorithm="RSA2048" canonicalization="none" keyID="dd0af29b41cd7d6d82593caf1ba9eaa6b756383f">NrGAEHN/XXue4idZRUCOTxF1wWzgItG+++IHKYLNEMzxww2R8SkR++dOaAKcJ4P5boEiAWPdGgSKcVi4Tc13Fx2P226o1io39zxU1RADCRnDm52iR7G4gIqhQnBj0+r47JJdQgt2NLXZHp+7ntwaRogGGIcVhX4DqgYxo0/dBgniN1zJGOxN1raGzB9EQI6CX3vXT5MC15/sZVFZy8IV8XmHoVQldQ8BGvQw6N9nwAElJS/FM7nipeXjYz2NhnzY2oRnOXevVTOPgpIX1iMbHxOdqB91enzr4Zz0teBSkvAYzA/HMGVFpYdWzBtSm3btzjGDOljg1eyNQivo0A7byA==</signature></uits:UITS>

   3. 0x052031 = 335921  bytes of padding

 \endcode
 *
 * So we know this ID3v2 tag:
 *
 * - uses version 2.3 of the spec
 * - does not use unsychronization nor compression
 * - is 452,951 bytes in size
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v2_3_tag )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V2_3("/vagrant/test/data/id3v2.3.tag");

  fs::ifstream ifsv2_3(TEST_DATA_V2_3, fs::ifstream::binary);
  id3v2_3_tag tag(ifsv2_3);

  BOOST_CHECK(3 == tag.version());
  BOOST_CHECK(0 == tag.revision());
  BOOST_CHECK(452951 == tag.size());
  BOOST_CHECK(0 == tag.flags());
  BOOST_CHECK(!tag.unsynchronised());
  BOOST_CHECK(!tag.experimental());
  BOOST_CHECK(!tag.has_extended_header());
  BOOST_CHECK(335921 == tag.padding());

  BOOST_CHECK("Hell's Ditch [Expanded] (US Version)" == tag.album());
  BOOST_CHECK("The Pogues" == tag.artist());
  BOOST_CHECK("Pop" == tag.content_type());
  BOOST_CHECK("Lorca's Novena" == tag.title());
  BOOST_CHECK("Pop" == tag.content_type());
  BOOST_CHECK("5" == tag.track());
  BOOST_CHECK("1990" == tag.year());

  BOOST_CHECK( tag.has_album());
  BOOST_CHECK( tag.has_artist());
  BOOST_CHECK( tag.has_content_type());
  BOOST_CHECK(!tag.has_encoded_by());
  BOOST_CHECK(!tag.has_languages());
  BOOST_CHECK( tag.has_title());
  BOOST_CHECK( tag.has_track());
  BOOST_CHECK( tag.has_year());

  vector<COMM> comments;
  tag.get_comments(back_inserter(comments));
  BOOST_CHECK(1 == comments.size());

  vector<unsigned char> buf;
  comments[0].descriptionb(back_inserter(buf));
  BOOST_CHECK(buf.size());

  const vector<unsigned char> GOLD1{
    0xff, 0xfe, 0x41, 0x00, 0x6d, 0x00, 0x61, 0x00,
    0x7a, 0x00, 0x6f, 0x00, 0x6e, 0x00, 0x2e, 0x00,
    0x63, 0x00, 0x6f, 0x00, 0x6d, 0x00, 0x20, 0x00,
    0x53, 0x00, 0x6f, 0x00, 0x6e, 0x00, 0x67, 0x00,
    0x20, 0x00, 0x49, 0x00, 0x44, 0x00, 0x3a, 0x00,
    0x20, 0x00, 0x32, 0x00, 0x30, 0x00, 0x33, 0x00,
    0x35, 0x00, 0x35, 0x00, 0x38, 0x00, 0x32, 0x00,
    0x35, 0x00, 0x34, 0x00 };

  buf.erase(buf.begin(), buf.end());
  comments[0].textb(back_inserter(buf));
  BOOST_CHECK(buf == GOLD1);

  string s = comments[0].description<string>();
  BOOST_CHECK(s.empty());
  s = comments[0].text<string>();
  BOOST_CHECK(s == "Amazon.com Song ID: 203558254");

  // TCOM/Composer & TPE3/Conducter just have a BOM
  // TPE2/Band is "The Pogues"
  // TCOP: "2004 Warner Music UK Ltd." (UCS2, LE BOM)
  // TPOS frame (Part of a set): "1" (ISO-8859-1)
  // APIC frame
  // PRIV frame: Owner/ID: "amazon.com" (XML Doc)

} // End test_id3v2_3_tag.

/**
 * \brief Test against two more ID3v2.3 files
 *
 *
 * Test data for the first file (Opium Garden):
 *
 * \code

   000000 49 44 33 03 00 00 00 00 0f 76 54 45 4e 43 00 00  >ID3......vTENC..<
   000010 00 1b 40 00 01 ff fe 57 00 69 00 6e 00 61 00 6d  >..@....W.i.n.a.m<
   000020 00 70 00 20 00 35 00 2e 00 35 00 35 00 32 00 54  >.p. .5...5.5.2.T<
   000030 52 43 4b 00 00 00 05 00 00 01 ff fe 31 00 43 4f  >RCK.........1.CO<
   000040 4d 4d 00 00 00 44 00 00 01 00 00 00 ff fe 00 00  >MM...D..........<
   000050 ff fe 52 00 69 00 70 00 70 00 65 00 64 00 20 00  >..R.i.p.p.e.d. .<
   000060 62 00 79 00 20 00 57 00 69 00 6e 00 61 00 6d 00  >b.y. .W.i.n.a.m.<
   000070 70 00 20 00 6f 00 6e 00 20 00 50 00 69 00 6d 00  >p. .o.n. .P.i.m.<
   000080 70 00 65 00 72 00 6e 00 65 00 6c 00 54 50 55 42  >p.e.r.n.e.l.TPUB<
   000090 00 00 00 19 00 00 01 ff fe 4f 00 70 00 69 00 75  >.........O.p.i.u<
   0000a0 00 6d 00 20 00 4d 00 75 00 73 00 69 00 63 00 54  >.m. .M.u.s.i.c.T<
   0000b0 50 4f 53 00 00 00 09 00 00 01 ff fe 31 00 2f 00  >POS.........1./.<
   0000c0 31 00 54 59 45 52 00 00 00 0b 00 00 01 ff fe 32  >1.TYER.........2<
   0000d0 00 30 00 30 00 33 00 54 43 4f 4e 00 00 00 27 00  >.0.0.3.TCON...'.<
   0000e0 00 01 ff fe 47 00 65 00 6e 00 65 00 72 00 61 00  >....G.e.n.e.r.a.<
   0000f0 6c 00 20 00 43 00 6c 00 75 00 62 00 20 00 44 00  >l. .C.l.u.b. .D.<
   000100 61 00 6e 00 63 00 65 00 54 41 4c 42 00 00 00 1d  >a.n.c.e.TALB....<
   000110 00 00 01 ff fe 4f 00 70 00 69 00 75 00 6d 00 20  >.....O.p.i.u.m. <
   000120 00 47 00 61 00 72 00 64 00 65 00 6e 00 73 00 54  >.G.a.r.d.e.n.s.T<
   000130 50 45 32 00 00 00 1b 00 00 01 ff fe 4f 00 70 00  >PE2.........O.p.<
   000140 69 00 75 00 6d 00 20 00 47 00 61 00 72 00 64 00  >i.u.m. .G.a.r.d.<
   000150 65 00 6e 00 54 50 45 31 00 00 00 1b 00 00 01 ff  >e.n.TPE1........<
   000160 fe 53 00 74 00 65 00 70 00 68 00 61 00 6e 00 20  >.S.t.e.p.h.a.n. <
   000170 00 4c 00 75 00 6b 00 65 00 55 46 49 44 00 00 00  >.L.u.k.e.UFID...<
   000180 5c 00 00 68 74 74 70 3a 2f 2f 77 77 77 2e 63 64  >\..http://www.cd<
   000190 64 62 2e 63 6f 6d 2f 69 64 33 2f 74 61 67 69 6e  >db.com/id3/tagin<
   0001a0 66 6f 31 2e 68 74 6d 6c 00 33 43 44 33 4e 33 39  >fo1.html.3CD3N39<
   0001b0 52 35 38 30 37 31 39 37 33 55 32 34 38 36 36 42  >R58071973U24866B<
   0001c0 32 39 43 36 31 42 39 36 41 39 33 33 41 42 43 32  >29C61B96A933ABC2<
   0001d0 36 39 45 36 45 31 45 46 42 44 42 33 44 50 32 54  >69E6E1EFBDB3DP2T<
   0001e0 49 54 32 00 00 00 25 00 00 01 ff fe 4f 00 70 00  >IT2...%.....O.p.<
   0001f0 69 00 75 00 6d 00 20 00 43 00 68 00 61 00 6e 00  >i.u.m. .C.h.a.n.<
   000200 74 00 20 00 49 00 6e 00 74 00 72 00 6f 00 00 00  >t. .I.n.t.r.o...<
   000210 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000800 ff fb 90 60 00 00 00 00 00 00 00 00 00 00 00 00  >...`............<

 \endcode
 *
 * Broken out:
 *
 \code

   000000 49 44 33                                         ID3
   000003          03 00                                   version 2, revision 3
   000005                00                                no unsync, no extended header, not experimental
   000006                   00 00 0f 76                    frame is 2038 bytes in size (track begins at 0x800)
   00000a                               54 45 4e 43        TENC frame (Encoded By)
   00000e                                           00 00  frame is 27 bytes in size
   000010 00 1b
   000012       40 00                                      discard frame if file is altered
   000014             01                                   Unicode encoding
   000014                ff fe                             Little Endian BOM
   000016                      57 00 69 00 6e 00 61 00 6d  "Winamp 5.552"
   000020 00 70 00 20 00 35 00 2e 00 35 00 35 00 32 00
   00002f                                              54  TRCK frame (Track)
   000030 52 43 4b
   000033          00 00 00 05                             frame is 5 bytes in size
   000037                      00 00                       no flags
   000039                            01                    Unicode encoding
   00003a                               ff fe              Little Endian BOM
   00003c                                     31 00        "1"
   00003e                                           43 4f  COMM frame (Comments)
   000040 4d 4d
   000042       00 00 00 44                                frame is 68 bytes in length
   000046                   00 00                          no flags
   000048                         01                       Unicode encoding
   000049                            00 00 00              000 (no language)
   00004c                                     ff fe        Little Endian BOM
   00004e                                           00 00  "" (no description)
   000050 ff fe                                            Little Endian BOM
   000052       52 00 69 00 70 00 70 00 65 00 64 00 20 00  "Ripped by Winamp on Pimpernel"
   000060 62 00 79 00 20 00 57 00 69 00 6e 00 61 00 6d 00
   000070 70 00 20 00 6f 00 6e 00 20 00 50 00 69 00 6d 00
   000080 70 00 65 00 72 00 6e 00 65 00 6c 00
   000080                                     54 50 55 42  TPUB frame (Publisher)
   000090 00 00 00 19                                      frame is 25 bytes in length
   000094             00 00                                no frame flags
   000096                   01                             Unicode encoding
   000097                      ff fe                       Little Endian BOM
   000099                            4f 00 70 00 69 00 75  "Opium Music"
   0000a0 00 6d 00 20 00 4d 00 75 00 73 00 69 00 63 00
   0000a0                                              54  TPOS frame (Part of a set)
   0000b0 50 4f 53
   0000b3          00 00 00 09                             frame is 9 bytes in length
   0000b7                      00 00                       no frame flags
   0000b9                            01                    Unicode encoding
   0000ba                               ff fe              Little Endian BOM
   0000bc                                     31 00 2f 00  "1/1"
   0000c0 31 00
   0000c2       54 59 45 52                                TYER
   0000c6                   00 00 00 0b                    frame is 11 bytes long
   0000ca                               00 00              no frame flags
   0000cc                                     01           Unicode encoding
   0000cd                                        ff fe     Little Endian BOM
   0000cf                                              32  "2003"
   0000d0 00 30 00 30 00 33 00
   0000d7                      54 43 4f 4e                 TCON
   0000db                                  00 00 00 27     frame is 39 bytes in length
   0000df                                              00  no frame flags
   0000e0 00
   0000e1    01                                            Unicode encoding
   0000e2       ff fe                                      Little Endian BOM
   0000e4             47 00 65 00 6e 00 65 00 72 00 61 00  "General Club Dance"
   0000f0 6c 00 20 00 43 00 6c 00 75 00 62 00 20 00 44 00
   000100 61 00 6e 00 63 00 65 00
   000108                         54 41 4c 42              TALB
   00010c                                     00 00 00 1d  frame is 29 bytes in length
   000110 00 00                                            no frame flags
   000112       01                                         Unicode encoding
   000113          ff fe                                   little Endian BOM
   000115                4f 00 70 00 69 00 75 00 6d 00 20  "Opium Gardens"
   000120 00 47 00 61 00 72 00 64 00 65 00 6e 00 73 00
   00012f                                              54  TPE2
   000130 50 45 32
   000133          00 00 00 1b                             frame is 27 bytes long
   000137                      00 00                       no frame flags
   000139                            01                    Unicode Encoding
   00013a                               ff fe              Little Endian BOM
   00013c                                     4f 00 70 00  "Opium Garden"
   000140 69 00 75 00 6d 00 20 00 47 00 61 00 72 00 64 00
   000150 65 00 6e 00
   000154             54 50 45 31                          TPE1
   000158                         00 00 00 1b              frame is 27 bytes long
   00015c                                     00 00        no frame flags
   00015e                                           01     Unicode encoding
   00015f                                              ff  Little Endian BOM
   000160 fe
   000161    53 00 74 00 65 00 70 00 68 00 61 00 6e 00 20  "Stephan Luke"
   000170 00 4c 00 75 00 6b 00 65 00
   000179                            55 46 49 44           UFID
   00017d                                        00 00 00  frame is 92  bytes long
   000180 5c
   000181    00 00                                         no frame flags
   000183          68 74 74 70 3a 2f 2f 77 77 77 2e 63 64  "http://www.cddb.com/id3/taginfo1.html"
   000190 64 62 2e 63 6f 6d 2f 69 64 33 2f 74 61 67 69 6e
   0001a0 66 6f 31 2e 68 74 6d 6c 00
   0001a9                            33 43 44 33 4e 33 39  3CD3N39R58071973U24866B29C61B96A933ABC269E6E1EFBDB3DP2
   0001b0 52 35 38 30 37 31 39 37 33 55 32 34 38 36 36 42
   0001c0 32 39 43 36 31 42 39 36 41 39 33 33 41 42 43 32
   0001d0 36 39 45 36 45 31 45 46 42 44 42 33 44 50 32
   0001df                                              54  TIT2
   0001e0 49 54 32
   0001e3          00 00 00 25                             frame is 37 bytes in size
   0001e7                      00 00                       no frame flags
   0001e9                            01                    Unicode encoding
   0001ea                               ff fe              Little Endian BOM
   0001ec                                     4f 00 70 00  "Opium Chant Intro"
   0001f0 69 00 75 00 6d 00 20 00 43 00 68 00 61 00 6e 00
   000200 74 00 20 00 49 00 6e 00 74 00 72 00 6f 00
   00020e                                           00 00  Padding
   000210 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
   *
   000800 ff fb 90 60 00 00 00 00 00 00 00 00 00 00 00 00  ...`............

   1. %00 00 0f 76 = b0000 0000 0000 0000 0000 1111 0111 0110 =>
    0000 0000  000 0000  000 1111  111 0110 =>
    00000000000000000 0111 1111 0110 =>
    0x07f6 = 2038 (=> track begins at 0x800)

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_id3v2_3_files )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE_01("/vagrant/test/data/opium.mp3");
  const fs::path TEST_FILE_02("/vagrant/test/data/u2-promenade.mp3");
  const fs::path TEST_FILE_03("/vagrant/test/data/forbidden.mp3");

  fs::ifstream ifs01(TEST_FILE_01, fs::ifstream::binary);
  id3v2_3_tag tag01(ifs01);

  BOOST_CHECK(!tag01.experimental());
  BOOST_CHECK(!tag01.has_extended_header());
  BOOST_CHECK(1522 == tag01.padding());

  BOOST_CHECK("Opium Gardens"      == tag01.album());
  BOOST_CHECK("Stephan Luke"       == tag01.artist());
  BOOST_CHECK("General Club Dance" == tag01.content_type());
  BOOST_CHECK("Winamp 5.552"       == tag01.encoded_by());
  BOOST_CHECK("Opium Chant Intro"  == tag01.title());
  BOOST_CHECK("1"                  == tag01.track());
  BOOST_CHECK("2003"               == tag01.year());

  BOOST_CHECK( tag01.has_album());
  BOOST_CHECK( tag01.has_artist());
  BOOST_CHECK( tag01.has_content_type());
  BOOST_CHECK( tag01.has_encoded_by());
  BOOST_CHECK(!tag01.has_languages());
  BOOST_CHECK( tag01.has_title());
  BOOST_CHECK( tag01.has_track());
  BOOST_CHECK( tag01.has_year());

  BOOST_CHECK( 1 == tag01.has_frame("UFID"));
  const id3v2_3_frame &rf = tag01.get_frame("UFID");
  
  const UFID *pf = dynamic_cast<const UFID*>(&rf);
  BOOST_CHECK(nullptr != pf);

  string s = pf->owner<string>();
  BOOST_CHECK("http://www.cddb.com/id3/taginfo1.html" == s);

  vector<COMM> comments;
  tag01.get_comments(back_inserter(comments));

  BOOST_CHECK(1 == comments.size());
  const COMM &C = comments.front();

  unsigned char lang[3];
  vector<unsigned char> desc, text;

  BOOST_CHECK(1 == C.unicode());

  C.lang(lang);
  BOOST_CHECK(0 == lang[0] && 0 == lang[1] && 0 == lang[2]);

  vector<unsigned char> GOLD0 = vector<unsigned char>{0xff, 0xfe};
  C.descriptionb(back_inserter(desc));
  BOOST_CHECK(desc == GOLD0);

  s = C.description<string>();
  BOOST_CHECK("" == s);

  C.textb(back_inserter(text));
  vector<unsigned char> GOLD1 = vector<unsigned char>{
    0xff, 0xfe, 0x52, 0x00, 0x69, 0x00, 0x70, 0x00,
    0x70, 0x00, 0x65, 0x00, 0x64, 0x00, 0x20, 0x00,
    0x62, 0x00, 0x79, 0x00, 0x20, 0x00, 0x57, 0x00,
    0x69, 0x00, 0x6e, 0x00, 0x61, 0x00, 0x6d, 0x00,
    0x70, 0x00, 0x20, 0x00, 0x6f, 0x00, 0x6e, 0x00,
    0x20, 0x00, 0x50, 0x00, 0x69, 0x00, 0x6d, 0x00,
    0x70, 0x00, 0x65, 0x00, 0x72, 0x00, 0x6e, 0x00,
    0x65, 0x00, 0x6c, 0x00
  };
  BOOST_CHECK(text == GOLD1);

  s = C.text<string>();
  BOOST_CHECK("Ripped by Winamp on Pimpernel" == s);

  fs::ifstream ifs02(TEST_FILE_02, fs::ifstream::binary);
  id3v2_3_tag tag02(ifs02);

  BOOST_CHECK("The Unforgettable Fire" == tag02.album());
  BOOST_CHECK("U2" == tag02.artist());
  BOOST_CHECK("Alternative" == tag02.content_type());
  BOOST_CHECK("Winamp 5.5" == tag02.encoded_by());
  BOOST_CHECK("Promenade" == tag02.title());

  BOOST_CHECK(1 == tag02.has_frame("TXXX"));
  const id3v2_3_frame &f = tag02.get_frame("TXXX");
  const TXXX *p = dynamic_cast<const TXXX*>(&f);
  BOOST_CHECK(nullptr != p);

  s = p->description<string>();
  BOOST_CHECK("GN/ExtData" == s);

  s = p->text<string>();
  BOOST_CHECK("GN/ExtData" == s);

  fs::ifstream ifs03(TEST_FILE_03, fs::ifstream::binary);
  id3v2_3_tag tag03(ifs03);

  BOOST_CHECK(!tag03.experimental());
  BOOST_CHECK(!tag03.has_extended_header());
  BOOST_CHECK(860 == tag03.padding());

  BOOST_CHECK("The Amazing"     == tag03.album());
  BOOST_CHECK("Nina Simone"     == tag03.artist());
  BOOST_CHECK("Forbidden Fruit" == tag03.title());
  BOOST_CHECK("3"               == tag03.track());

  BOOST_CHECK( tag03.has_album());
  BOOST_CHECK( tag03.has_artist());
  BOOST_CHECK( tag03.has_content_type());
  BOOST_CHECK(!tag03.has_encoded_by());
  BOOST_CHECK(!tag03.has_languages());
  BOOST_CHECK( tag03.has_title());
  BOOST_CHECK( tag03.has_track());
  BOOST_CHECK( tag03.has_year());

  BOOST_CHECK("(8)" == tag03.content_type());
  BOOST_CHECK("" == tag03.year());
}

/**
 * \brief Test against an interesting ID3v2.3 tag
 *
 *
 * The test data:
 *
 \code

   000000 49 44 33 03 00 00 00 00 04 51 54 50 45 31 00 00  >ID3......QTPE1..<
   000010 00 10 00 00 00 4e 69 6e 65 20 49 6e 63 68 20 4e  >.....Nine Inch N<
   000020 61 69 6c 73 54 49 54 32 00 00 00 0f 00 00 00 4f  >ailsTIT2.......O<
   000030 6e 6c 79 20 54 69 6d 65 2c 20 54 68 65 54 41 4c  >nly Time, TheTAL<
   000040 42 00 00 00 01 00 00 00 54 59 45 52 00 00 00 01  >B.......TYER....<
   000050 00 00 00 54 43 4f 4e 00 00 00 0e 00 00 00 28 37  >...TCON.......(7<
   000060 39 29 48 61 72 64 20 52 6f 63 6b 43 4f 4d 4d 00  >9)Hard RockCOMM.<
   000070 00 00 05 00 00 00 65 6e 67 00 54 52 43 4b 00 00  >......eng.TRCK..<
   000080 00 01 00 00 00 43 4f 4d 4d 00 00 00 14 00 00 00  >.....COMM.......<
   000090 00 00 00 4d 75 73 69 63 4d 61 74 63 68 5f 4d 6f  >...MusicMatch_Mo<
   0000a0 6f 64 00 43 4f 4d 4d 00 00 00 15 00 00 00 00 00  >od.COMM.........<
   0000b0 00 4d 75 73 69 63 4d 61 74 63 68 5f 54 65 6d 70  >.MusicMatch_Temp<
   0000c0 6f 00 43 4f 4d 4d 00 00 00 19 00 00 00 00 00 00  >o.COMM..........<
   0000d0 4d 75 73 69 63 4d 61 74 63 68 5f 53 69 74 75 61  >MusicMatch_Situa<
   0000e0 74 69 6f 6e 00 00 00 00 00 00 00 00 00 00 00 00  >tion............<
   0000f0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
   *
   000250 00 00 00 00 00 00 00 00 00 00 00 ff fa 92 04 cc  >................<

 \endcode
 *
 * broken out:
 *
 \code

   000000 49 44 33                                         ID3
   000003          03 00                                   ID3v2 version 3, revision 0
   000005                00                                no unsync, no extended header, not experimental
   000006                   00 00 04 51                    tag size is 593 bytes [1]
   00000a                               54 50 45 31        TPE1 frame (Lead performer(s)/Soloist(s))
   00000e                                           00 00  frame is 16 bytes in size
   000010 00 10
   000012       00 00                                      no flags
   000014             00                                   ISO-8859-1 encoding
   000015                4e 69 6e 65 20 49 6e 63 68 20 4e  "Nine Inch Nails"
   000020 61 69 6c 73
   000024             54 49 54 32                          TIT2 frame (Title/songname/content description)
   000028                         00 00 00 0f              frame is 15 bytes in size
   00002c                                     00 00        no flags
   00002e                                           00     ISO-8859-1 encoding
   00002f                                              4f  "Only Time, The"
   000030 6e 6c 79 20 54 69 6d 65 2c 20 54 68 65
   00003d                                        54 41 4c  TALB frame (Album/Movie/Show title)
   000040 42
   000041    00 00 00 01                                   frame is 1 byte in size
   000045                00 00                             no flags
   000047                      00                          ISO-8859-1 encoding
   000048                         54 59 45 52              TYER (Year)
   00004c                                     00 00 00 01  frame is 1 byte in size
   000050 00 00                                            no frame flags
   000052       00                                         ISO-8859-1 encoding
   000053          54 43 4f 4e                             TCON frame (Content Type)
   000057                      00 00 00 0e                 frame is 14 bytes in size
   00005b                                  00 00           no flags
   00005d                                        00        ISO-8859-1 encoding
   00005e                                           28 37  "(79)Hard Rock"
   000060 39 29 48 61 72 64 20 52 6f 63 6b
   00006b                                  43 4f 4d 4d 00  COMM frame (Comments)
   00006f                                              00  frame is 5 bytes in size
   000070 00 00 05
   000073          00 00                                   no flags
   000075                00                                ISO-8859-1 encoding
   000076                   65 6e 67                       "eng" (English language)
   000079                            00                    no description
   00007a                               54 52 43 4b        TRCK frame (Track number/Position in set)
   00007e                                           00 00  frame is 1 byte in size
   000080 00 01
   000082       00 00                                      no flags
   000084             00                                   ISO-8859-1 encoding (no data)
   000085                43 4f 4d 4d                       COMM frame (Comments)
   000089                            00 00 00 14           frame is 20 bytes in size
   00008d                                        00 00     no flags
   00008f                                              00  ISO-8859-1 encoding
   000090 00 00 00                                         000 (no language information)
   000093          4d 75 73 69 63 4d 61 74 63 68 5f 4d 6f  "MusicMatch_Mood" (description-- no data)
   0000a0 6f 64 00
   0000a3          43 4f 4d 4d                             COMM (Comment frame)
   0000a7                      00 00 00 15                 frame is 21 bytes in size
   0000ab                                  00 00           no flags
   0000ad                                        00        ISO-8859-1
   0000ae                                           00 00  000 (no language information)
   0000b0 00
   0000b1    4d 75 73 69 63 4d 61 74 63 68 5f 54 65 6d 70  "MusicMatch_Tempo" (description-- no data)
   0000c0 6f 00
   0000c2       43 4f 4d 4d                                COMM (Comment frame)
   0000c6                   00 00 00 19                    frame is 25 bytes in size
   0000ca                               00 00              no flags
   0000cc                                     00           ISO-8859-1
   0000cd                                        00 00 00  000 (no language information)
   0000d0 4d 75 73 69 63 4d 61 74 63 68 5f 53 69 74 75 61  "MusicMatch_Situation"
   0000e0 74 69 6f 6e 00
   0000e5                00 00 00 00 00 00 00 00 00 00 00  Padding (374 bytes)
   0000f0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
   *
   000250 00 00 00 00 00 00 00 00 00 00 00
   00025b                                  ff fa 92 04 cc  audio data

  1. 00 00 04 51 = b0000 0000 0000 0000 0000 0100 0101 0001 =>
     b 000 0000 000 0000 000 0100 101 0001 =>
     b 0000 0000 0000 0000 0010 0101 0001 =
     0x0000251 = 593

  \endcode
  *
  *
  */

BOOST_AUTO_TEST_CASE( test_funny_files )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE_01("/vagrant/test/data/nin-only-time.mp3");
  const fs::path TEST_FILE_02("/vagrant/test/data/waterfall.mp3");

  fs::ifstream ifs01(TEST_FILE_01, fs::ifstream::binary);
  id3v2_3_tag tag01(ifs01);

  BOOST_CHECK(!tag01.experimental());
  BOOST_CHECK(!tag01.has_extended_header());
  BOOST_CHECK(374 == tag01.padding());

  BOOST_CHECK("" == tag01.album());
  BOOST_CHECK("Nine Inch Nails" == tag01.artist());
  BOOST_CHECK("(79)Hard Rock" == tag01.content_type());
  BOOST_CHECK(0 == tag01.has_encoded_by());
  BOOST_CHECK(!tag01.has_languages());
  BOOST_CHECK("Only Time, The" == tag01.title());
  BOOST_CHECK(tag01.has_track());
  BOOST_CHECK("" == tag01.track());
  BOOST_CHECK(tag01.has_year());
  BOOST_CHECK("" == tag01.year());

  vector<COMM> C;

  const vector<unsigned char> GOLD1({{'M', 'u', 's', 'i', 'c', 'M', 'a', 't', 'c', 'h', '_', 'M', 'o', 'o', 'd'}});
  const vector<unsigned char> GOLD2({{'M', 'u', 's', 'i', 'c', 'M', 'a', 't', 'c', 'h', '_', 'T', 'e', 'm', 'p', 'o'}});
  const vector<unsigned char> GOLD3({{'M', 'u', 's', 'i', 'c', 'M', 'a', 't', 'c', 'h', '_', 'S', 'i', 't', 'u', 'a', 't', 'i', 'o', 'n'}});

  unsigned char lang[3];
  vector<unsigned char> dsc, txt;
  tag01.get_comments(back_inserter(C));

  BOOST_CHECK(4 == C.size());
  BOOST_CHECK(0 == C[0].unicode());
  C[0].lang(lang);
  BOOST_CHECK('e' == lang[0] && 'n' == lang[1] && 'g' == lang[2]);
  C[0].descriptionb(back_inserter(dsc));
  BOOST_CHECK(0 == dsc.size());
  C[0].textb(back_inserter(txt));
  BOOST_CHECK(0 == txt.size());

  dsc.resize(0);
  txt.resize(0);

  BOOST_CHECK(0 == C[1].unicode());
  C[1].lang(lang);
  BOOST_CHECK(0 == lang[0] && 0 == lang[1] && 0 == lang[2]);
  C[1].descriptionb(back_inserter(dsc));
  BOOST_CHECK(dsc == GOLD1);
  BOOST_CHECK("MusicMatch_Mood" == C[1].description<string>());
  C[1].textb(back_inserter(txt));
  BOOST_CHECK(0 == txt.size());

  dsc.resize(0);
  txt.resize(0);

  BOOST_CHECK(0 == C[2].unicode());
  C[2].lang(lang);
  BOOST_CHECK(0 == lang[0] && 0 == lang[1] && 0 == lang[2]);
  C[2].descriptionb(back_inserter(dsc));
  BOOST_CHECK(dsc == GOLD2);
  BOOST_CHECK("MusicMatch_Tempo" == C[2].description<string>());
  C[2].textb(back_inserter(txt));
  BOOST_CHECK(0 == txt.size());

  dsc.resize(0);
  txt.resize(0);

  BOOST_CHECK(0 == C[3].unicode());
  C[3].lang(lang);
  BOOST_CHECK(0 == lang[0] && 0 == lang[1] && 0 == lang[2]);
  C[3].descriptionb(back_inserter(dsc));
  BOOST_CHECK(dsc == GOLD3);
  BOOST_CHECK("MusicMatch_Situation" == C[3].description<string>());
  C[3].textb(back_inserter(txt));
  BOOST_CHECK(0 == txt.size());

  fs::ifstream ifs02(TEST_FILE_02, fs::ifstream::binary);
  id3v2_3_tag tag02(ifs02);

  BOOST_CHECK(!tag02.experimental());
  BOOST_CHECK(!tag02.has_extended_header());

  BOOST_CHECK(""          == tag02.album());
  BOOST_CHECK("Enya"      == tag02.artist());
  BOOST_CHECK("New Age"   == tag02.content_type());
  BOOST_CHECK(""          == tag02.encoded_by());
  BOOST_CHECK("Waterfall" == tag02.title());
  BOOST_CHECK(""          == tag02.track());
  BOOST_CHECK("1998"      == tag02.year());

  BOOST_CHECK(!tag02.has_languages());
  BOOST_CHECK(2 == tag02.has_artist());
  BOOST_CHECK(2 == tag02.has_content_type());
  BOOST_CHECK(2 == tag02.has_title());

}

/**
 * \brief Test a tag that contains text tags that are present, but empty
 *
 *
 * This comes from my "Rock the Joint" capture ;)
 *
 \code

  mgh@Crickhollow[2-0:...ode/projects/scribbu]: od -A x -t x1z test/data/rock-the-joint.id3v2.3.tag
  000000 49 44 33 03 00 00 00 00 0c 40 54 50 45 31 00 00  >ID3......@TPE1..<
  000010 00 05 00 00 01 fe ff 00 00 54 49 54 32 00 00 00  >.........TIT2...<
  000020 4b 00 00 01 fe ff 00 52 00 6f 00 63 00 6b 00 74  >K......R.o.c.k.t<
  000030 00 68 00 65 00 4a 00 6f 00 69 00 6e 00 74 00 6c  >.h.e.J.o.i.n.t.l<
  000040 00 69 00 76 00 65 00 66 00 72 00 6f 00 6d 00 4c  >.i.v.e.f.r.o.m.L<
  000050 00 69 00 6d 00 65 00 72 00 69 00 63 00 6b 00 49  >.i.m.e.r.i.c.k.I<
  000060 00 72 00 65 00 6c 00 61 00 6e 00 64 00 00 54 45  >.r.e.l.a.n.d..TE<
  000070 4e 43 00 00 00 35 00 00 01 fe ff 00 52 00 69 00  >NC...5......R.i.<
  000080 70 00 70 00 65 00 64 00 20 00 77 00 69 00 74 00  >p.p.e.d. .w.i.t.<
  000090 68 00 20 00 53 00 74 00 72 00 65 00 61 00 6d 00  >h. .S.t.r.e.a.m.<
  0000a0 72 00 69 00 70 00 70 00 65 00 72 00 00 54 41 4c  >r.i.p.p.e.r..TAL<
  0000b0 42 00 00 00 05 00 00 01 fe ff 00 00 54 52 43 4b  >B...........TRCK<
  0000c0 00 00 00 07 00 00 01 fe ff 00 32 00 00 54 59 45  >..........2..TYE<
  0000d0 52 00 00 00 05 00 00 01 fe ff 00 00 00 00 00 00  >R...............<
  0000e0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  *
  000640 00 00 00 00 00 00 00 00 00 00                    >..........<
  00064a

 \endcode
 *
 * broken out:
 *
 \code
  000000 49 44 33                                         ID3
  000003          03 00                                   ID3v2 version 3, revision 0
  000005                00                                no unsync, no extended header, not experimental
  000006                   00 00 0c 40                    tag size is 0x0640 = 1600 bytes [1]
  00000a                               54 50 45 31        TPE1 frame (Lead performer(s)/Soloist(s))
  00000e                                           00 00  frame is 5 bytes in size
  000010 00 05
  000012       00 00                                      no flags
  000014             01                                   Unicode encoding
  000015                fe ff                             Big Endian BOM
  000017                      00 00                       ""
  000019                            54 49 54 32           TIT2
  00001d                                        00 00 00
  000020 4b                                               frame is 0x4b = 75 bytes in size
  000021    00 00                                         no flags
  000023          01                                      Unicode encoding
  000024             fe ff                                Big Endian BOM
  000026                   00 52 00 6f 00 63 00 6b 00 74  "RocktheJointlivefromLimerickIreland"
  000030 00 68 00 65 00 4a 00 6f 00 69 00 6e 00 74 00 6c
  000040 00 69 00 76 00 65 00 66 00 72 00 6f 00 6d 00 4c
  000050 00 69 00 6d 00 65 00 72 00 69 00 63 00 6b 00 49
  000060 00 72 00 65 00 6c 00 61 00 6e 00 64 00 00
  00006e                                           54 45  Encoded By
  000070 4e 43
  000072       00 00 00 35                                frame is 0x35 = 53 bytes in size
  000076                   00 00                          no flags
  000078                         01                       Unicode encoding
  000079                            fe ff                 Big Endian BOM
  00007b                                  00 52 00 69 00  "Ripped with Streamripper"
  000080 70 00 70 00 65 00 64 00 20 00 77 00 69 00 74 00
  000090 68 00 20 00 53 00 74 00 72 00 65 00 61 00 6d 00
  0000a0 72 00 69 00 70 00 70 00 65 00 72 00 00
  0000ad                                        54 41 4c  TALB frame (Album/Movie/Show title)
  0000b0 42
  0000b1    00 00 00 05                                   frame is 5 bytes in size
  0000b5                00 00                             no flags
  0000b7                      01                          Unicode encoding
  0000b8                         fe ff                    Big Endian BOM
  0000ba                               00 00              ""
  0000ba                                     54 52 43 4b  TRCK frame (Track number/Position in set)
  0000c0 00 00 00 07                                      frame is 7 bytes in size
  0000c4             00 00                                no flags
  0000c6                   01                             Unicode encoding
  0000c7                      fe ff                       Big Endian BOM
  0000c9                            00 32 00 00           "2"
  0000cd                                        54 59 45  TYER frame (Year)
  0000d0 52
  0000d1    00 00 00 05                                   frame is 5 bytes in size
  0000d5                00 00                             no flags
  0000d7                      01                          Unicode encoding
  0000d8                         fe ff                    Big Endian BOM
  0000da                               00 00              ""
  0000dc                                     00 00 00 00  padding
  *
  000640 00 00 00 00 00 00 00 00 00 00                    ..........
  00064a

  1. 00 00 0c 40 = b0000 0000 0000 0000 0000 1100 0100 0000 =>
     0000 0000 000 0000 000 1100 100 0000 =>
     b00000 0000 0000 0000 0110 0100 0000 =
     0x0000640 = 1600 bytes

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_rock_the_joint )
{
  using scribbu::id3v2_3_tag;

  const fs::path TEST_FILE("/vagrant/test/data/rock-the-joint.id3v2.3.tag");

  fs::ifstream ifs(TEST_FILE, fs::ifstream::binary);
  id3v2_3_tag tag(ifs);

  BOOST_CHECK("" == tag.artist());
  BOOST_CHECK("RocktheJointlivefromLimerickIreland" == tag.title());
  BOOST_CHECK("Ripped with Streamripper" == tag.encoded_by());
  BOOST_CHECK("" == tag.album());
  BOOST_CHECK("" == tag.year());
  BOOST_CHECK(0x56e == tag.padding());

}

BOOST_AUTO_TEST_CASE( test_id3v2_2_frames )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/cerulean.mp3");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_3_tag tag(ifs);

  vector<POPM> popms;
  tag.get_popularimeters(back_inserter(popms));
  BOOST_CHECK(1 == popms.size());

  const POPM &p = popms.front();
  
  string text = p.email<string>();
  BOOST_CHECK("rating@winamp.com" == text);
  BOOST_CHECK(255 == p.rating());
  

}

/**
 * \brief Test unsynchronised data
 *
 * 
 \code

 000000 49 44 33 03 00 80 00 00 01 30 54 49 54 32 00 00  >ID3......0TIT2..<
 000010 00 35 00 00 01 fe ff 00 00 4d 00 79 00 20 00 62  >.5.......M.y. .b<
 000020 00 61 00 62 00 65 00 20 00 6a 00 75 00 73 00 74  >.a.b.e. .j.u.s.t<
 000030 00 20 00 63 00 61 00 72 00 65 00 73 00 20 00 66  >. .c.a.r.e.s. .f<
 000040 00 6f 00 72 00 20 00 6d 00 65 54 50 45 31 00 00  >.o.r. .m.eTPE1..<
 000050 00 19 00 00 01 fe ff 00 00 4e 00 69 00 6e 00 61  >.........N.i.n.a<
 000060 00 20 00 53 00 69 00 6d 00 6f 00 6e 00 65 54 41  >. .S.i.m.o.n.eTA<
 000070 4c 42 00 00 00 15 00 00 01 fe ff 00 00 31 00 30  >LB...........1.0<
 000080 00 30 00 25 00 20 00 4a 00 61 00 7a 00 7a 54 52  >.0.%. .J.a.z.zTR<
 000090 43 4b 00 00 00 07 00 00 01 fe ff 00 00 30 00 33  >CK...........0.3<
 0000a0 54 4c 45 4e 00 00 00 0f 40 00 01 fe ff 00 00 32  >TLEN....@......2<
 0000b0 00 31 00 36 00 30 00 30 00 30 ff fb 90 0c 00 0f  >.1.6.0.0.0......<
 0000c0 f0 c6 02 48 00 00 00 00 1c 00 49 10 00 00 00 03  >...H......I.....<
 0000d0 00 09 2c 00 00 00 00 5e 01 25 c0 00 00 00 ff ff  >..,....^.%......<
 0000e0 ff ff ff ff ff ff ff ff ff ff ff e5 ff ac fc a0  >................<
 0000f0 0c 1f 3e 7d 47 cb bf ff ff ff ff ff ff ff ff ff  >..>}G...........<
 000100 ff ff ff ff f2 e3 86 02 07 35 02 00 30 7d e5 c1  >.........5..0}..<
 000110 f7 4b bf ff ff ff ff ff ff ff ff ff ff ff ff ff  >.K..............<
 000120 ba 9b f9 52 22 4a d8 5c 06 c4 bb ff ff ff ff ff  >...R"J.\........<
 000130 ff ff ff ff ff ff ff ff d3 43 1d e5 9c 82 67 90  >.........C....g.<
 000140

 \endcode
 *
 * broken out:
 *
 \code

 000000 49 44 33                                         This is an ID3 tag
 000003          03 00                                   ID3v2.3
 000005                80                                unsynchronised
 000006                   00 00 01 30                    0xb0 = 176 bytes [1]
 00000a                               54 49 54 32        TIT2 frame
 00000e                                           00 00  0x35 = 53 bytes
 000010 00 35
 000012       00 00                                      no flags
 000014             01                                   UCS-2
 000015                fe ff 00                          Big Endian (w/sync)
 000018                         00 4d 00 79 00 20 00 62  My babe just cares
 000020 00 61 00 62 00 65 00 20 00 6a 00 75 00 73 00 74  for me
 000030 00 20 00 63 00 61 00 72 00 65 00 73 00 20 00 66
 000040 00 6f 00 72 00 20 00 6d 00 65 
 00004a                               54 50 45 31        TPE1 (artist)
 00004e                                           00 00  0x19 = 25 bytes
 000050 00 19 
 000052       00 00                                      no flags
 000054             01                                   UCS-2
 000055                fe ff 00                          Big Endian (w/sync)
 000058                         00 4e 00 69 00 6e 00 61  Nina Simone
 000060 00 20 00 53 00 69 00 6d 00 6f 00 6e 00 65
 00006e                                           54 41  TALB
 000070 4c 42
 000072       00 00 00 15                                0x15 = 21 bytes
 000076                   00 00                          no flags
 000078                         01                       UCS-2
 000079                            fe ff 00              Big Endian (w/sync)
 00007c                                     00 31 00 30  100% Jazz
 000080 00 30 00 25 00 20 00 4a 00 61 00 7a 00 7a
 00008e                                           54 52  TRCK
 000090 43 4b
 000092       00 00 00 07                                0x07 = 7 bytes
 000096                   00 00                          no flags
 000098                         01                       UCS-2
 000099                            fe ff 00              Big Endian (w/sync)
 00009c                                     00 30 00 33  "03"
 0000a0 54 4c 45 4e                                      TLEN
 0000a4             00 00 00 0f                          0x0f = 15 bytes
 0000a8                         40 00                    file alter preservation
 0000aa                               01                 UCS-2
 0000ab                                  fe ff 00        Big Endian (w/sync)
 0000ae                                           00 32  216000
 0000b0 00 31 00 36 00 30 00 30 00 30
 0000ba                               ff fb 90 0c 00 0f  track data...

 1. 0x0130 = b0000 0001 0011 0000 -> b 000 0001 011 0000 =
 b 00 0000 1011 0000 = 0xb0 = 176
 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_unsync )
{
  using namespace std;
  using namespace scribbu;

  // Lifted from the taglib test suite
  static const fs::path DATA1("/vagrant/test/data/unsynch.id3");
  // Bill LeFaive - Orlando.mp3
  static const fs::path DATA2("/vagrant/test/data/orlando.mp3");

  fs::ifstream ifs1(DATA1, fs::ifstream::binary);
  id3v2_3_tag tag1(ifs1);

  string text = tag1.title();
  BOOST_CHECK("My babe just cares for me" == text);
  text = tag1.artist();
  BOOST_CHECK("Nina Simone" == text);
  text = tag1.album();
  BOOST_CHECK("100% Jazz" == text);
  text = tag1.track();
  BOOST_CHECK("03" == text);

  fs::ifstream ifs2(DATA2, fs::ifstream::binary);
  id3v2_3_tag tag2(ifs2);

  text = tag2.album();
  BOOST_CHECK("http://music.download.com" == text);
  text = tag2.title();
  BOOST_CHECK("Orlando" == text);
  text = tag2.artist();
  BOOST_CHECK("Bill LeFaive" == text);

}

/**
 *
 *
 \code

 000000 49 44 33 03 00 00 00 00 2c 34 41 50 49 43 00 00  >ID3.....,4APIC..<
 000010 10 5d 00 80 00 00 00 9b 78 9c ed 9d fb 5b 13 57  >.]......x....[.W<
 000020 1a c7 ed 7f b1 7f c2 fe b6 3f 6d bb dd c7 ee b3  >.........?m.....<
 000030 b5 d6 d6 f5 5e bb 5a ef ca 03 ab 16 15 05 5b 2a  >....^.Z.......[*<
 000040 a2 20 d8 2a 88 d8 05 5d 84 c8 45 41 6e 22 f7 20  >. .*...]..EAn". <
 000050 97 a0 5c 44 2e 51 90 7b 80 2a 01 14 14 94 5b 0a  >..\D.Q.{.*....[.<

 \endcode
 *
 \code

 000000 49 44 33                                         ID3: ID3v2 header
 000003          03 00                                   ID3v2.3(.0)
 000005                00                                no flags
 000006                   00 00 2c 34                    0x1634 = 5684 bytes [1]
 00000a                               41 50 49 43        APIC frame
 00000e                                           00 00  0x105d = 4189 bytes
 000010 10 5d
 000012       00 80                                      COMPRESSED
 000014             00 00 00 9b                          DECOMPRESSED SIZE [2]
 000018                         78 9c ed 9d fb 5b 13 57  >.]......x....[.W<
 000020 1a c7 ed 7f b1 7f c2 fe b6 3f 6d bb dd c7 ee b3  >.........?m.....<
 *
 001071    57 4f 41 52                                   WOAR
 001075                00 00 00 00                       0 bytes
 001079                            00 00                 no flags
 00107b                                  50 4f 50 4d     POPM
 00107f                                              00  6 bytes
 001080 00 00 06 
 001083          00 00                                   no flags
 001085                00                                nil e-mail
 001086                   00                             unknown rating
 001087                      00 00 00 00                 zero counter
 00108b                                  54 52 43 4b     TRCK
 00108f                                              00  >...........TRCK.<
 001090 00 00 01 00 00 00 54 43 4f 4e 00 00 00 0d 00 00  >......TCON......<
 0010a0 00 54 65 63 68 6e 6f 2d 44 61 6e 63 65 43 4f 4d  >.Techno-DanceCOM<
 0010b0 4d 00 00 00 05 00 00 00 65 6e 67 00 54 59 45 52  >M.......eng.TYER<
 0010c0 00 00 00 01 00 00 00 54 41 4c 42 00 00 00 0c 00  >.......TALB.....<
 0010d0 00 00 3c 55 6e 64 65 66 69 6e 65 64 3e 54 50 45  >..<Undefined>TPE<
 0010e0 31 00 00 00 05 00 00 00 4d 6f 62 79 54 49 54 32  >1.......MobyTIT2<
 0010f0 00 00 00 1f 00 00 00 42 72 61 76 65 68 65 61 72  >.......Bravehear<
 001100 74 20 54 68 65 6d 65 20 28 54 65 63 68 6e 6f 20  >t Theme (Techno <
 001110 72 65 6d 69 78 00 00 00 00 00 00 00 00 00 00 00  >remix...........<
 001120 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
 *
 001380 00 00 00 00 00 00 00 00                          >........<
 001388

 1. 0x2c34 = b0010 1100 0011 0100 -> b 010 1100  011 0100
    -> b 01 0110 0011 0100 = 0x1634 = 5684

 This is incorrect-- the tag is actually 0x1388 bytes in size; subtracting
 10 for the frame header gives 0x1373. Making this sync-safe:

 0x1373 = b0001 0011 0111 0011 -> b 010 0110 111 0011
 -> b 0010 0110 0111 0011 = 0x2673

 2. This is also incorrect; the actual uncompressed size is 86427 = 0x1519B

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_compressed )
{
  using namespace std;
  using namespace scribbu;

  // Lifted from the taglib test suite
  static const fs::path DATA1("/vagrant/test/data/compressed.mp3");

  fs::ifstream ifs1(DATA1, fs::ifstream::binary);
  id3v2_3_tag tag1(ifs1);
  BOOST_CHECK(1 == tag1.has_title());
  BOOST_CHECK("Braveheart Theme (Techno remix" == tag1.title());
}
