#include <scribbu/pprinter.hh>

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_pprinting_id3v1 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA_V1("/vagrant/test/data/id3v1.tag");

  const string GOLDEN(R"(ID3v1.1: The Pogues - Lorca's Novena
Hell's Ditch [Expanded] (US Ve (track 5), 1990
Amazon.com Song ID: 20355825
unknown genre 255
)");

  fs::ifstream ifs(TEST_DATA_V1);
  id3v1_tag tag(ifs);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_2 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.2.tag");

  const string GOLDEN(R"(ID3v2.2(.0) Tag:
2192 bytes, synchronised
flags: 0x00
Murley Braid Quartet - Sheep Walking
Mnemosyne's March (Demo), 2006
Content-type (8)
Encoded by iTunes v6.0.4
TT2: Sheep Walking
TP1: Murley Braid Quartet
TCM: Mike Murley
TAL: Mnemosyne's March (Demo)
TYE: 2006
TCO: (8)
TEN: iTunes v6.0.4
COM (iTunNORM):
 000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78
COM (iTunSMPB):
 00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
1802 bytes of padding
)");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v2_2_tag tag(ifs);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_3 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.3.tag");

  const string GOLDEN(R"(ID3v2.3(.0) Tag:
452951 bytes, synchronised
flags: 0x00
The Pogues - Lorca's Novena
Hell's Ditch [Expanded] (US Version) (track 5), 1990
Content-type Pop
TIT2: Lorca's Novena
TPE1: The Pogues
TALB: Hell's Ditch [Expanded] (US Version)
TCON: Pop
TCOM: 
TPE3: 
TRCK: 5
TYER: 1990
TPE2: The Pogues
COMM (<no description>):
Amazon.com Song ID: 203558254
TCOP: 2004 Warner Music UK Ltd.
TPOS: 1
frame APIC (115554 bytes)
frame PRIV (1122 bytes)
335921 bytes of padding
)");

  fs::ifstream ifs_3(TEST_DATA, fs::ifstream::binary);
  id3v2_3_tag tag(ifs_3);

  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);

  const fs::path TEST_DATA2("/vagrant/test/data/cerulean.mp3");

  fs::ifstream ifs2(TEST_DATA2, fs::fstream::binary);
  id3v2_3_tag tag2(ifs2);

  stringstream stm2;
  stm2 << tag2;
  text = stm2.str();
  BOOST_MESSAGE(text);

}

BOOST_AUTO_TEST_CASE( test_pprinting_id3v2_4 )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v2.4.tag");

  const string GOLDEN(R"(ID3v2.4(.0) Tag:
1091 bytes, synchronised
flags: 0x00
Joao Gilberto - Acapulco
Ela E Carioca, <no year>
TPE1: Joao Gilberto
TIT2: Acapulco
TALB: Ela E Carioca
1024 bytes of padding
)");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v2_4_tag tag(ifs);
  stringstream stm;
  stm << tag;

  string text = stm.str();
  BOOST_MESSAGE( text );

  BOOST_CHECK(GOLDEN == text);
}

// TODO: Test track_data
