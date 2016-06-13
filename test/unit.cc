#include <boost/filesystem.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>

namespace fs   = boost::filesystem;
namespace test = boost::unit_test;

test::test_suite*
init_unit_test_suite(int   argc,
                     char *argv[])
{
  scribbu::static_initialize();
  return 0;
}

/**
 * \brief Exercise basic file processing
 *
 * Walk a test file that I've dissected by hand & test the basic interface.
 *
 *
 \code
 od -t x1z -N 128 Pogues\,\ The\ -\ Lorca\'s\ Novena.mp3
 0000000 49 44 33 03 00 00 00 1b 52 57 54 49 54 32 00 00  >ID3.....RWTIT2..<
 0000020 00 0f 00 00 00 4c 6f 72 63 61 27 73 20 4e 6f 76  >.....Lorca's Nov<
 0000040 65 6e 61 54 50 45 31 00 00 00 0b 00 00 00 54 68  >enaTPE1.......Th<
 0000060 65 20 50 6f 67 75 65 73 54 41 4c 42 00 00 00 25  >e PoguesTALB...%<
 0000100 00 00 00 48 65 6c 6c 27 73 20 44 69 74 63 68 20  >...Hell's Ditch <
 0000120 5b 45 78 70 61 6e 64 65 64 5d 20 28 55 53 20 56  >[Expanded] (US V<
 0000140 65 72 73 69 6f 6e 29 54 43 4f 4e 00 00 00 04 00  >ersion)TCON.....<
 0000160 00 00 50 6f 70 54 43 4f 4d 00 00 00 03 00 00 01  >..PopTCOM.......<
 0000200
 \endcode
 *
 * ID3v2 header:
 *
 \code
 0000000 49 44 33 => ID3 tag
                  03 00 => version 2.3.0
                        00 => no unsync, no compression
                           00 1b 52 57 ==
                           b0000 0000 0001 1011 0101 0010 0101 0111 =>
                           b 000 0000  001 1011  101 0010  101 0111 =>
                           b0000 0000 0110 1110 1001 0101 0111 ==
                           0x006e957 = 452,951
 \endcode
 *
 * So we know this ID3v2 tag:
 *
 * - uses version 2.3 of the spec
 * - does not use unsychronization nor compression
 * - is 452,951 bytes in size
 *
 *
 */

// TODO:
// BOOST_AUTO_TEST_CASE( test_file_processing )
// {
//   using namespace scribbu;

//   const fs::path TEST_FILE("/vagrant/test/data/Pogues, The - Lorca's Novena.mp3");

//   const std::size_t DIGEST_SIZE = track_data::DIGEST_SIZE;

//   // 48ff9cadea7d842e9059db25159d2daa
//   const unsigned char TEST_DIGEST[DIGEST_SIZE] = {
//     0x48, 0xff, 0x9c, 0xad, 0xea, 0x7d, 0x84, 0x2e,
//     0x90, 0x59, 0xdb, 0x25, 0x15, 0x9d, 0x2d, 0xaa
//   };

//   std::unique_ptr<std::istream> pis;
//   file_info            fi;
//   std::tie(pis, fi) = open_file(TEST_FILE);
//   BOOST_CHECK(pis && *pis);
//   BOOST_CHECK(fs::path("/vagrant/test/data") == fi.parent());
//   BOOST_CHECK(fs::path("Pogues, The - Lorca's Novena.mp3") == fi.filename());
//   BOOST_CHECK(9878797UL == fi.size());

//   std::unique_ptr<id3v2_tag> pid3v2 = process_id3v2(*pis);
//   BOOST_CHECK(pis && *pis);
//   BOOST_CHECK(452961UL == pis->tellg());
//   BOOST_REQUIRE(pid3v2);
//   BOOST_CHECK(3 == pid3v2->version());
//   BOOST_CHECK(0 == pid3v2->revision());
//   BOOST_CHECK(452951 == pid3v2->size());

//   track_data tdata(*pis);
//   BOOST_CHECK(*pis);
//   BOOST_CHECK(9878669 == pis->tellg());
//   unsigned char md5[DIGEST_SIZE];
//   tdata.get_md5(md5);
//   BOOST_CHECK(std::equal(md5, md5 + DIGEST_SIZE, TEST_DIGEST));

//   std::unique_ptr<scribbu::id3v1_tag> pid3v1 = process_id3v1(*pis);
//   BOOST_CHECK(pid3v1);

// }
