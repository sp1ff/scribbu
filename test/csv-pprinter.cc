#include <scribbu/csv-pprinter.hh>

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

  const fs::path TEST_DATA_V1("/vagrant/test/data/id3v1.tag");

  const string GOLDEN("1,0,The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Ve,1990,Amazon.com Song ID: 20355825,255,");

  fs::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII) << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v22 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.2.tag");

  const string GOLDEN("2,0,2192,0x00,0,Murley Braid Quartet,Sheep Walking,Mnemosyne's March (Demo),(8),iTunes v6.0.4,,,0,,2, 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78, 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000,,,");
  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, encoding::UTF_8) << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v23 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.3.tag");

  const string GOLDEN("3,0,452951,0x00,0,The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Version),Pop,,1990,,0,,1,Amazon.com Song ID: 203558254,,,,");

  fs::ifstream ifs_3(TEST_DATA, fs::ifstream::binary);
  id3v2_3_tag tag(ifs_3);

  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, boost::none) << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_csv_pprinting_id3v24 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.4.tag");

  const string GOLDEN("4,0,1091,0x00,0,Joao Gilberto,Acapulco,Ela E Carioca,,,,,0,,0,,,,,");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << print_as_csv(4, encoding::ASCII, boost::none) << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}
