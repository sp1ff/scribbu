/**
 * \file mp3.cc
 *
 * Copyright (C) 2021-2024 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/mp3.hh>

#include "unit.hh"

#include <fstream>
#include <boost/test/unit_test.hpp>

namespace fs = std::filesystem;

BOOST_AUTO_TEST_CASE( test_mp3_smoke )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "sat.mp3");

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);

  mp3_audio_frame f(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_1 == f.version() );
  BOOST_CHECK( 128000 == f.bps() );
  BOOST_CHECK( 44100 == f.sample_rate_hz() );
  BOOST_CHECK( 418 == f.size() );

  mp3_audio_frame g(ifs);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_1 == g.version() );
  BOOST_CHECK( 128000 == g.bps() );
  BOOST_CHECK( 44100 == g.sample_rate_hz() );
  BOOST_CHECK( 418 == g.size() );

  size_t nframes = 2; // two frames so far
  while (ifs.good()) {
    try {
      mp3_audio_frame h(ifs);
      ++nframes;
    } catch (const std::exception&) {
      BOOST_REQUIRE(ifs.eof());
    }
  }

  BOOST_CHECK( nframes == 12379 );
  BOOST_CHECK( 323 == int( (1152. / 44100.) * nframes ) );

  ifs.clear();
  ifs.seekg(0, std::ios_base::beg);
  double x = get_mp3_duration(ifs);

  BOOST_CHECK_CLOSE(x, 323.36979591836734693877551020408163265306122448979591836734693877, 1.0e-16);

}

BOOST_AUTO_TEST_CASE( test_vbri_smoke )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "VBRI_16M.mp3");

  // 0000000 ff f3 b9 c0 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // 0000016 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // 0000032 00 00 00 00 56 42 52 49 00 01 06 f1 00 32 00 00  >....VBRI.....2..<
  // 0000048 0f 54 00 00 00 20 00 1f 00 01 00 02 00 01 01 f8  >.T... ..........<
  // 0000064 00 fc 00 90 00 6c 00 90 00 48 00 6c 00 48 00 6c  >.....l...H.l.H.l<
  // 0000080 00 6c 00 6c 00 48 00 6c 00 6c 00 48 00 6c 00 6c  >.l.l.H.l.l.H.l.l<
  // 0000096 00 6c 00 6c 00 6c 00 6c 00 48 00 6c 00 6c 00 6c  >.l.l.l.l.H.l.l.l<
  // 0000112 00 48 00 6c 00 48 00 6c 00 d8 00 b4 00 00 00 00  >.H.l.H.l........<
  // 0000128 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // *
  // 0000496 00 00 00 00 00 00 00 00 ff f2 79 c0 39 61 00 33  >..........y.9a.3<

  // ff f3 b9 c0
  // 1111 1111 1111 0011 1011 1001 1100 0000
  // AAAA AAAA AAAB BCCD EEEE FFGH IIJJ KLMM

  // B := 10 => MPEG version 2
  // C := 01 => Layer III
  // D := 1 => no CRC
  // E := 1011 => 112kbps
  // F := 10 => 16000 Hz
  // G := 0 => no padding
  // I := 11 => mono (=> side info is 9 bytes)

  // frame size:

  // floor(576 * (112000/(16000*8)) = 504

  // frame duration:

  // 576 / 16000 = 0.036 seconds

  // VBRI:
  // - version: 1
  // - delay: 0x06f1
  // - quality: 0x0032
  // - bytes in file: 0x00000f54
  // - frames: 0x00000020
  // - entries: 0x001f
  // - scale factor: 0x0001
  // - TOC entry size: 0x0002
  // - frame per entry: 0x0001
  // - TOC: [62,124)

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);

  mp3_audio_frame f(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_2 == f.version() );
  BOOST_CHECK( 112000 == f.bps() );
  BOOST_CHECK( 16000 == f.sample_rate_hz() );
  BOOST_CHECK( 504 == f.size() );

  // ff f2 79 c0
  // 1111 1111 1111 0010 0111 1001 1100 0000
  // AAAA AAAA AAAB BCCD EEEE FFGH IIJJ KLMM
  mp3_audio_frame g(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_2 == g.version() );
  BOOST_CHECK( 56000 == g.bps() );
  BOOST_CHECK( 16000 == g.sample_rate_hz() );

  ifs.seekg(0, ios_base::beg);
  double x = get_mp3_duration(ifs);
  BOOST_CHECK_CLOSE(x, 1.152, 1.0e-16);

}

BOOST_AUTO_TEST_CASE( test_lame_smoke )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "LAME_aots_3951.mp3");

  // 0000000 ff fb 94 44 00 00 00 00 00 00 00 00 00 00 00 00  >...D............<
  // 0000016 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // 0000032 00 00 00 00 49 6e 66 6f 00 00 00 0f 00 00 00 d2  >....Info........<
  // 0000048 00 01 3c 80 00 03 06 08 0a 0d 0f 12 14 17 19 1d  >..<.............<
  // 0000064 1f 22 24 27 29 2b 2e 30 33 36 39 3b 3e 40 43 45  >."$')+.0369;>@CE<
  // 0000080 47 4a 4c 50 52 55 57 5a 5c 5f 61 63 66 6a 6c 6e  >GJLPRUWZ\_acfjln<
  // 0000096 71 73 76 78 7b 7d 80 83 86 88 8a 8d 8f 92 94 97  >qsvx{}..........<
  // 0000112 99 9d 9f a2 a4 a7 a9 ab ae b0 b3 b6 b9 bb be c0  >................<
  // 0000128 c3 c5 c7 ca cc d0 d2 d5 d7 da dc df e1 e3 e6 ea  >................<
  // 0000144 ec ee f1 f3 f6 f8 fb fd 00 00 00 3a 4c 41 4d 45  >...........:LAME<
  // 0000160 33 2e 39 33 20 01 99 00 00 00 00 00 00 00 00 02  >3.93 ...........<
  // 0000176 80 24 05 40 8d 00 00 00 00 01 3c 80 59 ca 38 50  >.$.@......<.Y.8P<
  // 0000192 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // *
  // 0000384 ff fb 94 44 00 00 02 31 1c 4f 06 72 00 00 4e 29  >...D...1.O.r..N)<

  // ff fb 94 44
  // 1111 1111 1111 1011 1001 0100 0100 0100
  // AAAA AAAA AAAB BCCD EEEE FFGH IIJJ KLMM

  // B: 11 => MPEG 1
  // D: 1 => no CRC
  // E: 1001 => 128kbps
  // F: 01 => 48000Hz
  // G: 0 => no padding
  // I : 01 => stereo
  // 32 bytes of side info

  // frame size:

  // floor(1152 * (128000/(48000*8)) = 384

  // frame duration:

  // 1152 / 48000 = 0.024 seconds

  // 0xd2 = 210 = frames

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);

  mp3_audio_frame f(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_1 == f.version() );
  BOOST_CHECK( 128000 == f.bps() );
  BOOST_CHECK( 48000 == f.sample_rate_hz() );
  BOOST_CHECK( 384 == f.size() );

  mp3_audio_frame g(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_1 == g.version() );
  BOOST_CHECK( 128000 == g.bps() );
  BOOST_CHECK( 48000 == g.sample_rate_hz() );

  ifs.seekg(0, ios_base::beg);
  double x = get_mp3_duration(ifs);
  BOOST_CHECK_CLOSE(x, 5.04, 1.0e-16);

}

BOOST_AUTO_TEST_CASE( test_3931 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "3931.mp3");

  // 0000000 ff fb 94 44 00 00 00 00 00 00 00 00 00 00 00 00  >...D............<
  // 0000016 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // 0000032 00 00 00 00 49 6e 66 6f 00 00 00 0f 00 00 00 d2  >....Info........<
  // 0000048 00 01 3c 80 00 03 06 08 0a 0d 0f 12 14 17 19 1d  >..<.............<
  // 0000064 1f 22 24 27 29 2b 2e 30 33 36 39 3b 3e 40 43 45  >."$')+.0369;>@CE<
  // 0000080 47 4a 4c 50 52 55 57 5a 5c 5f 61 63 66 6a 6c 6e  >GJLPRUWZ\_acfjln<
  // 0000096 71 73 76 78 7b 7d 80 83 86 88 8a 8d 8f 92 94 97  >qsvx{}..........<
  // 0000112 99 9d 9f a2 a4 a7 a9 ab ae b0 b3 b6 b9 bb be c0  >................<
  // 0000128 c3 c5 c7 ca cc d0 d2 d5 d7 da dc df e1 e3 e6 ea  >................<
  // 0000144 ec ee f1 f3 f6 f8 fb fd 00 00 00 3a 4c 41 4d 45  >...........:LAME<
  // 0000160 33 2e 39 33 20 01 99 00 00 00 00 00 00 00 00 02  >3.93 ...........<
  // 0000176 80 24 05 40 8d 00 00 00 00 01 3c 80 59 ca 38 50  >.$.@......<.Y.8P<
  // 0000192 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // *
  // 0000384 ff fb 94 44 00 00 02 31 1c 4f 06 72 00 00 4e 29  >...D...1.O.r..N)<

  // ff fb 94 44
  // 1111 1111 1111 1011 1001 0100 0100 0100
  // AAAA AAAA AAAB BCCD EEEE FFGH IIJJ KLMM

  // B: 11 => MPEG version 1
  // D: 1 => no CRC
  // E: 1001 => 128kbps
  // F: 01 => sampling rate 48000Hz
  // G: 0 => not padded

  // MP3:
  // frame size: floor(1152 * 128 * 1000 / 48000 / 8) = 384 bytes
  // frame duration: 1152 / 48000 = 0.024 seconds

  // Xing header:
  // flags: 0x0f => frames, bytes & toc all present
  // 0xd2 = 210 = frames
  // 0x013c80 = 81024 = bytes
  // <toc>
  // LAME 3.93

  // CBR encoding so duration = 0.024 * 210 = 5.04

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);

  mp3_audio_frame f(ifs, true);
  BOOST_CHECK( mp3_audio_frame::mpeg_version::mpeg_audio_version_1 == f.version() );
  BOOST_CHECK( 128000 == f.bps() );
  BOOST_CHECK( 48000 == f.sample_rate_hz() );
  BOOST_CHECK( 384 == f.size() );

  ifs.seekg(0, ios_base::beg);
  double x = get_mp3_duration(ifs);
  BOOST_CHECK_CLOSE(x, 5.04, 1.0e-16);

}

BOOST_AUTO_TEST_CASE( test_zoe )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "zoe.mp3");

  // 000000 49 44 33                                         this is an ID3 tag
  // 000003          04 00                                   ID3 version 2.4.0
  // 000005                00                                no flags
  // 000006                   00 00 03 2a                    tag is 0x1aa = 426
  //                                                         bytes in size
  // 00000a                               54 53 53 45        TSSE frame
  // 00000e                                           00 00  frame is 0x0f =
  // 000010 00 0f                                            15 bytes in size
  // 000012       00 00                                      no flags
  // 000014             03                                   UTF-8 encoded
  // 000014                4c 61 76 66 35 39 2e 31 36 2e 31  "Lavf59.16.100"
  // 000020 30 30 00
  // 000023          50 43 4e 54                             PCNT frame
  // 000027                      00 00 00 01                 frame is one byte
  // 00002b                                  00 00           no flags
  // 00002d                                        01        count is one
  // 00002e                                           54 50  TPE1 frame
  // 000030 45 31
  // 000032       00 00 00 0c                                frame is 12 bytes
  // 000036                   00 00                          no flags
  // 000038                         03                       UTF-8
  // 000038                            27 5a 6f c3 ab 20 4d  "'Zoë Moon'"
  // 000040 6f 6f 6e 27
  // 000044             54 49 54 32                          TIT2 frame
  // 000048                         00 00 00 08              8 bytes
  // 00004c                                     00 00        no flags
  // 00004e                                           03     UTF-8
  // 00004f                                              46  "Falling"
  // 000050 61 6c 6c 69 6e 67
  // 000056                   54 59 45 52                    TYER frame
  // 00005a                               00 00 00 05        5 bytes
  // 00005e                                           00 00  no flags
  // 000060 03                                               UTF-8
  // 000061    32 30 31 39                                   "2019"
  // 000065                54 41 4c 42                       TALB frame
  // 000069                            00 00 00 05           5 bytes
  // 00006d                                        00 00     no flags
  // 00006f                                              03  UTF-8
  // 000070 45 64 65 6e                                      "Eden"
  // 000074             00 00 00 00 00 00 00 00 00 00 00 00  padding
  // 000080 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >................<
  // *

  // frame sync is eleven on bits:
  // 1111 1111 111* ****
  // ff 1110 * or ff 1111 *
  // ff e* or ff f*

  // 000110 00 00 00 00 fd fe 00 00 00 00 4c 61 76 63 35 39  >..........Lavc59<

  //                    👆
  // so we have a sync here
  // 000120 2e 31 38 00 00 00 00 00 00 00 00 00 00 00 00 24  >.18............$<
  // 000130 03 2c 00 00 00 00 00 57 b7 30 c3 34 8c b9 ff fb  >.,.....W.0.4....<
  // 000140 14 64 00 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.d.....i....... <
  // 000150 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 000160 00 04 4c 41 4d 45 33 2e 31 30 30 55 55 55 55 55  >..LAME3.100UUUUU<
  // 000170 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 000180 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 000190 55 55 55 55 55 55 55 55 55 55 55 55 55 55 ff fb  >UUUUUUUUUUUUUU..<
  // 0001a0 14 64 1e 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.d.....i....... <
  // 0001b0 00 00 01 00
  // tag ends here!
  // 0001b4             00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 0001c0 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 0001d0 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 0001e0 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 0001f0 55 55 55 55 55 55 55 55 55 55 55 55 55 55
  //                                                  sync!
  // 0001fe                                           ff fb  >UUUUUUUUUUUUUU..<
  // 000200 14 64 3c 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.d<....i....... <
  // 000210 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 000220 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 000230 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 000240 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 000250 55 55 55 55 55 55 55 55 55 55 55 55 55 55 ff fb  >UUUUUUUUUUUUUU..<
  // 000260 14 64 5a 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.dZ....i....... <
  // 000270 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 000280 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 000290 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 0002a0 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 0002b0 55 55 55 55 55 55 55 55 55 55 55 55 55 55 ff fb  >UUUUUUUUUUUUUU..<
  // 0002c0 14 64 78 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.dx....i....... <
  // 0002d0 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 0002e0 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 0002f0 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 000300 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 000310 55 55 55 55 55 55 55 55 55 55 55 55 55 55 ff fb  >UUUUUUUUUUUUUU..<
  // 000320 14 64 96 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.d.....i....... <
  // 000330 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 000340 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 000350 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 4c  >UUUUUUUUUUUUUUUL<
  // 000360 41 4d 45 33 2e 31 30 30 55 55 55 55 55 55 55 55  >AME3.100UUUUUUUU<
  // 000370 55 55 55 55 55 55 55 55 55 55 55 55 55 55 ff fb  >UUUUUUUUUUUUUU..<
  // 000380 14 64 b4 0f f0 00 00 69 00 00 00 08 00 00 0d 20  >.d.....i....... <
  // 000390 00 00 01 00 00 01 a4 00 00 00 20 00 00 34 80 00  >.......... ..4..<
  // 0003a0 00 04 55 55 55 55 55 55 55 55 55 55 55 55 55 55  >..UUUUUUUUUUUUUU<
  // 0003b0 55 55 55 55 55 55 55 55 55 55 55 55 55 55 55 a4  >UUUUUUUUUUUUUUU.<
  // 0003c0 6d 32 22 52 04 a9 94 d4 90 f6 a8 43 a9 da 28 c0  >m2"R.......C..(.<
  // 0003d0 41 84 59 72 e5 90 1b f6 cb dc 89 2b 0f 7c ff fb  >A.Yr.......+.|..<

  std::ifstream ifs(TEST_DATA, std::ifstream::binary);
  double x = get_mp3_duration(ifs);
  BOOST_CHECK_CLOSE(x, 268.44, 1.0e-16);
}
