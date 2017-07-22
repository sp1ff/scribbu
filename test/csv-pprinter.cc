#include <scribbu/csv-pprinter.hh>

#include "unit.hh"

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v1 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V1(get_data_directory() / "id3v1.tag");

  const string GOLDEN("1,0,The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Ve,1990,Amazon.com Song ID: 20355825,255,");

  fs::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v22 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.2.tag");

  const string GOLDEN("2,0,2192,0x00,0,Murley Braid Quartet,Sheep Walking,Mnemosyne's March (Demo),(8),iTunes v6.0.4,,,0,,2, 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78, 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000,,,");
  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, encoding::UTF_8) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v23 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_FILE_1(get_data_directory() / "id3v2.3.tag");
  const string GOLDEN_1("3,0,452951,0x00,0,The Pogues,Lorca's Novena,Hell's Di"
                        "tch [Expanded] (US Version),Pop,,1990,,0,,1,Amazon.co"
                        "m Song ID: 203558254,,,,");

  const fs::path TEST_FILE_2(get_data_directory() / "life.mp3");
  const string GOLDEN_2("3,0,1397,0x00,0,Frank Sinatra,That's Life,The Very Go"
                        "od Years,Vocal,,,,2,*,5,,Pretty slow,Evening,1966,,,");

  fs::ifstream ifs_1(TEST_FILE_1, fs::ifstream::binary);
  id3v2_3_tag tag_1(ifs_1);

  stringstream stm1;
  stm1 << print_as_csv(4, encoding::ASCII, boost::none) << tag_1;

  string text = stm1.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  stringstream stm2;
  stm2 << print_as_csv() << tag_1;

  text = stm2.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_1 == text);

  fs::ifstream ifs_2(TEST_FILE_2, fs::ifstream::binary);
  id3v2_3_tag tag_2(ifs_2);

  stringstream stm3;
  stm3 << print_as_csv(6, encoding::ASCII, encoding::UTF_8) << tag_2;

  text = stm3.str();
  BOOST_TEST_MESSAGE(text);
  BOOST_CHECK(GOLDEN_2 == text);


}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v24 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA(get_data_directory() / "id3v2.4.tag");

  const string GOLDEN("4,0,1091,0x00,0,Joao Gilberto,Acapulco,Ela E Carioca,,,,,0,,0,,,,,");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, boost::none) << tag;

  string text = stm.str();
  BOOST_TEST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}
