#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>

#include <iostream>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_track_data )
{
  const fs::path TEST_DATA("/vagrant/test/data/Cerulean - Questions Of Travel (LP Version).mp3");

  using namespace std;

  using scribbu::compact_track_data_formatter;
  using scribbu::csv_track_data_formatter;
  using scribbu::standard_track_data_formatter;

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  std::unique_ptr<scribbu::id3v2_tag> pid3v2 = scribbu::maybe_read_id3v2(ifs); // ID3v2 tags...
  scribbu::track_data td(ifs);                                                 // the track itself...

  std::stringstream stm_compact;
  stm_compact << compact_track_data_formatter(scribbu::file_size_units::megabytes) << td;

  std::string s = stm_compact.str();

  const char * const GOLD_COMPACT =
    "Track:32e73c84659bcf729333d7dd2efca044(7.40M)";

  BOOST_CHECK(GOLD_COMPACT == s);

  std::stringstream stm_csv;
  stm_csv << csv_track_data_formatter(scribbu::file_size_units::megabytes) << td;

  s = stm_csv.str();

  const char * const GOLD_CSV =
    "32e73c84659bcf729333d7dd2efca044,7.40";

  BOOST_CHECK(GOLD_CSV == s);

  std::stringstream stm_std;
  stm_std << standard_track_data_formatter(scribbu::file_size_units::megabytes) << td;

  s = stm_std.str();

  const char * const GOLD_STD =
    "Track Data:\n"
    "     MD5: 32e73c84659bcf729333d7dd2efca044\n"
    "    size: 7.40M\n";

  BOOST_CHECK(GOLD_STD == s);

}
