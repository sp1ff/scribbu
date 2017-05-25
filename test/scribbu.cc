#include <scribbu/scribbu.hh>

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/ostream.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/csv-pprinter.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_track_data )
{
  const fs::path TEST_DATA("/vagrant/test/data/cerulean.mp3");

  using namespace std;
  using namespace scribbu;

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  unique_ptr<scribbu::id3v2_tag> pid3v2 = maybe_read_id3v2(ifs); // ID3v2 tags...
  track_data td(ifs);                                            // the track itself...
  unique_ptr<id3v1_tag> pid3v1 = process_id3v1(ifs);             // & the ID3v1 tag.

  stringstream stm1;
  stm1 << *pid3v2 << td;
  if (pid3v1) {
    stm1 << *pid3v1;
  }

  static const string GOLD1(R"(ID3v2.3(.0) Tag:
295607 bytes, synchronised
flags: 0x00
The Ocean Blue - Questions Of Travel (LP Version)
Cerulean (US Release) (track 6/12), 2005
Content-type Alternative Rock
PCNT: rating@winamp.com
rating: 255
counter: 00000000
frame PRIV (8207 bytes)
TIT2: Questions Of Travel (LP Version)
TPE1: The Ocean Blue
TALB: Cerulean (US Release)
TCON: Alternative Rock
TPE3: 
TRCK: 6/12
TYER: 2005
TPE2: The Ocean Blue
COMM (<no description>):
tags=90s,sub-genres=shoegazer
TCOP: 2005 Warner Bros. Records Inc. Manufactured & Marketed by Warner Strategic Marketing.
TPOS: 1/1
frame APIC (286673 bytes)
82 bytes of padding
7760096 bytes of track data:
MD5: 32e73c84659bcf729333d7dd2efca044
ID3v1.1: The Ocean Blue - Questions Of Travel (LP Versio
Cerulean (US Release) (track 6), 2005
tags=90s,sub-genres=shoegaze
unknown genre 255
)");

  string text = stm1.str();
  BOOST_MESSAGE( text );
  BOOST_CHECK( GOLD1 == text );

  static const string GOLD2("3,0,295607,0x00,0,The Ocean Blue,Questions Of Travel (LP Version),Cerulean (US Release),Alternative Rock,,2005,,0,,1,\"tags=90s,sub-genres=shoegazer\",,,,7760096,32e73c84659bcf729333d7dd2efca044,");

  stringstream stm2;
  stm2 << print_as_csv(4, encoding::ASCII, boost::none) << *pid3v2 << td;

  text = stm2.str();
  BOOST_MESSAGE( text );
  BOOST_CHECK( text == GOLD2 );

}

BOOST_AUTO_TEST_CASE( test_track_data2 )
{
  const fs::path TEST_DATA("/vagrant/test/data/searchresults.dat");

  using namespace std;
  using namespace scribbu;

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  ios_base::iostate state = ifs.rdstate();
  unique_ptr<scribbu::id3v2_tag> pid3v2 = maybe_read_id3v2(ifs); // ID3v2 tags...
  track_data td(ifs);                                            // the track itself...
  unique_ptr<id3v1_tag> pid3v1 = process_id3v1(ifs);             // & the ID3v1 tag.

  stringstream stm1;
  if (pid3v2) {
    stm1 << *pid3v2;
  }
  stm1 << td;
  if (pid3v1) {
    stm1 << *pid3v1;
  }

  string text = stm1.str();
  BOOST_MESSAGE( text );

  static const string GOLD1(R"(8 bytes of track data:
MD5: 69c1753bd5f81501d95132d08af04464
)");

  BOOST_CHECK(text == GOLD1);

}
