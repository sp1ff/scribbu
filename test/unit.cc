#include "unit.hh"

#include <scribbu/scribbu.hh>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/test/unit_test.hpp>
#include <boost/filesystem/fstream.hpp>

#include <openssl/evp.h>

namespace fs   = boost::filesystem;
namespace po   = boost::program_options;
namespace test = boost::unit_test;

namespace {

  static fs::path source_directory_;

  /**
   * \brief Initialize the source directory
   *
   *
   * \param pth [in, opt] scribbu test source directory (i.e. .../scribbu/test"
   *
   *
   * This method will initialize the source directory using the following
   * order:
   *
   * 1. \a pth
   *
   * 2. the $srcdir environment variable
   *
   * 3. if $HOSTNAME == vagrant, set it to /vagrant/test
   *
   * 4. the present working directory
   *
   *
   * This routine is trying to guess the location of the test source code, and
   * by extension the data.
   *
   *
   */
  
  void
  initialize_source_directory(const fs::path &pth)
  {
    if (!pth.empty()) {
      source_directory_ = pth;
      return;
    }

    char *p = getenv("srcdir");
    if (p) {
      source_directory_ = fs::path(p);
      return;
    }

    p = getenv("HOSTNAME");
    if (p && 0 == strcmp(p, "vagrant")) {
      source_directory_ = fs::path("/vagrant");
      return;
    }

    source_directory_ = fs::current_path();
    
  }
  
} // End un-named namespace.

fs::path
get_source_directory()
{
  return source_directory_;
}

fs::path
get_data_directory()
{
  return get_source_directory() / "data";
}

test::test_suite*
init_unit_test_suite(int   argc,
                     char *argv[])
{
  scribbu::static_initialize();

  po::options_description opts("Extra options");
  opts.add_options()
    ("srcdir,s", po::value<fs::path>(), "scribbu test source directory");

  po::parsed_options parsed = po::command_line_parser(argc, argv).options(opts).run();

  po::variables_map vm;
  po::store(parsed, vm);

  fs::path srcdir;
  if (vm.count("srcdir")) {
    srcdir = vm["srcdir"].as<fs::path>();
  }

  initialize_source_directory(srcdir);

  return 0;
}

void
compute_md5(const boost::filesystem::path &pth, unsigned char md5[])
{
  using scribbu::openssl_error;

  const std::size_t BUFSIZE = 4 * 1024 * 1024; // Four megabytes

  static unsigned char BUF[BUFSIZE];

  EVP_MD_CTX *mdctx = EVP_MD_CTX_create();
  if (! mdctx) {
    throw new openssl_error();
  }

  if (! EVP_DigestInit_ex(mdctx, EVP_md5(), 0)) {
    EVP_MD_CTX_destroy(mdctx);
    throw new openssl_error();
  }

  fs::ifstream ifs(pth, fs::ifstream::binary);

  for (std::streamsize nleft = fs::file_size(pth); nleft > 0; ) {

    std::streamsize nbytes = BUFSIZE > nleft ? nleft : BUFSIZE;

    ifs.read((char*)BUF, nbytes);
    if (! EVP_DigestUpdate(mdctx, BUF, nbytes)) {
      EVP_MD_CTX_destroy(mdctx);
      throw new openssl_error();
    }
    
    nleft -= nbytes;

  }

  unsigned int md_len;
  EVP_DigestFinal_ex(mdctx, md5, &md_len);
  
  EVP_MD_CTX_destroy(mdctx);
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

BOOST_AUTO_TEST_CASE( test_file_processing )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE(get_data_directory() / "lorca.mp3");

  const size_t DIGEST_SIZE = track_data::DIGEST_SIZE;

  // 48ff9cadea7d842e9059db25159d2daa
  const unsigned char TEST_DIGEST[DIGEST_SIZE] = {
    0x48, 0xff, 0x9c, 0xad, 0xea, 0x7d, 0x84, 0x2e,
    0x90, 0x59, 0xdb, 0x25, 0x15, 0x9d, 0x2d, 0xaa
  };

  unique_ptr<istream> pis;
  file_info            fi;
  tie(pis, fi) = open_file(TEST_FILE);
  BOOST_CHECK(pis && *pis);
  string s = fi.parent().string();
  BOOST_CHECK("/data" == s.substr(s.length() - 5));
  BOOST_CHECK(fs::path("lorca.mp3") == fi.filename());
  BOOST_CHECK(9878797UL == fi.size());

  vector<unique_ptr<id3v2_tag>> v2tags;
  read_all_id3v2(*pis, back_inserter(v2tags));
  BOOST_CHECK(1 == v2tags.size());

  unique_ptr<id3v2_tag> &pid3v2 = v2tags.front();
  BOOST_CHECK(pis && *pis);
  BOOST_CHECK(452961UL == pis->tellg());
  BOOST_REQUIRE(pid3v2);
  BOOST_CHECK(3 == pid3v2->version());
  BOOST_CHECK(0 == pid3v2->revision());
  BOOST_CHECK(452951 == pid3v2->size());

  track_data tdata(*pis);
  BOOST_CHECK(*pis);
  BOOST_CHECK(9878669 == pis->tellg());
  unsigned char md5[DIGEST_SIZE];
  tdata.get_md5(md5);
  BOOST_CHECK(equal(md5, md5 + DIGEST_SIZE, TEST_DIGEST));

  unique_ptr<scribbu::id3v1_tag> pid3v1 = process_id3v1(*pis);
  BOOST_CHECK(pid3v1);

}
