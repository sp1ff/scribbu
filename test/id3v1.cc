/**
 * \page id3v1_unit_tests
 *
 *
 * Corpus:
 *
 * - "A"/test_id3v1_a/id3v1.2.3.tag-- ID3v1.1 tag from 'Pogues, The - Lorca's Novena.mp3'
 * - "B"/test_id3v1_b/id3v1.2.4.tag-- ID3v1 tag from 'Joao Gilberto - Acapulco.mp3'
 * - "C"/test_id3v1_c/id3v1-ext.tag-- IDv1 Extended tag from 'Mike Murley - Sleepwalking.mp3'
 * - "D"/test_jing_jing_1/红颜旧.mp3-- no ID3v1 tag at all
 * - "E"/test_elliot_goldenthal/elliot-goldenthal.id3v1.tag-- ID3v1.1
 *
 *
 */

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/framesv2.hh>

#include <iostream>
#include <memory>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_id3v1_a )
{
  using namespace scribbu;

  using std::back_inserter;

  using scribbu::compact_id3v1_formatter;
  using scribbu::csv_id3v1_formatter;
  using scribbu::standard_id3v1_formatter;

  const fs::path TEST_DATA("/vagrant/test/data/id3v1.2.3.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v1_tag tag1(ifs);

  BOOST_CHECK(!tag1.enhanced());
  BOOST_CHECK(!tag1.extended());
  BOOST_CHECK(tag1.v1_1());

  BOOST_CHECK(0xff == tag1.genre());

  std::vector<unsigned char> album;
  tag1.album(back_inserter(album));
  const std::vector<unsigned char> ALBUM = {{
      'H', 'e', 'l', 'l', '\'', 's', ' ', 'D', 'i', 't', 'c', 'h', ' ', '[',
      'E', 'x', 'p', 'a', 'n', 'd', 'e', 'd', ']', ' ', '(', 'U', 'S', ' ',
      'V', 'e'
    }};
  BOOST_CHECK(album == ALBUM);

  const std::string SALBUM("Hell\'s Ditch [Expanded] (US Ve");
  std::string salbum = tag1.album(id3v1_encoding::automatic);
  BOOST_CHECK(salbum == SALBUM);

  std::vector<unsigned char> artist;
  tag1.artist(back_inserter(artist));
  const std::vector<unsigned char> ARTIST = {{
      'T', 'h', 'e', ' ', 'P', 'o', 'g', 'u', 'e', 's', 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0
    }};
  BOOST_CHECK(artist == ARTIST);

  std::vector<unsigned char> comment;
  tag1.comment(back_inserter(comment));
  const std::vector<unsigned char> COMMENT = {{
      'A', 'm', 'a', 'z', 'o', 'n', '.', 'c', 'o', 'm', ' ', 'S', 'o', 'n',
      'g', ' ', 'I', 'D', ':', ' ', '2', '0', '3', '5', '5', '8', '2', '5',
    }};
  BOOST_CHECK(comment == COMMENT);

  std::vector<unsigned char> genre2;
  tag1.genre(back_inserter(genre2));
  BOOST_CHECK(genre2.empty());

  bool valid;
  unsigned char speed;
  std::tie(valid, speed) = tag1.speed();
  BOOST_CHECK(!valid);

  std::vector<unsigned char> start_time;
  tag1.genre(back_inserter(start_time));
  BOOST_CHECK(start_time.empty());
  std::vector<unsigned char> end_time;
  tag1.genre(back_inserter(end_time));
  BOOST_CHECK(end_time.empty());

  std::vector<unsigned char> title;
  tag1.title(back_inserter(title));
  const std::vector<unsigned char> TITLE = {{
      'L', 'o', 'r', 'c', 'a', '\'', 's', ' ', 'N', 'o', 'v', 'e', 'n', 'a',
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0
    }};
  BOOST_CHECK(title == TITLE);

  unsigned char track_number;
  std::tie(valid, track_number) = tag1.track_number();
  BOOST_CHECK(valid);
  BOOST_CHECK(0x05 == track_number);

  unsigned char year[4];
  tag1.year(year);
  BOOST_CHECK('1' == year[0] && '9' == year[1] &&
              '9' == year[2] && '0' == year[3]);

  std::stringstream compact_stm;
  compact_stm << compact_id3v1_formatter(id3v1_encoding::automatic,
                                         id3v1_genre_expansion::expand,
                                         ',') << tag1;
  std::string s = compact_stm.str();

  static const char * const GOLD_COMPACT =
    "ID3v1.1:The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Ve,1990,genr"
    "e:255,track:5,Amazon.com Song ID: 20355825";

  BOOST_CHECK(GOLD_COMPACT == s);

  std::stringstream csv_stm;
  csv_stm << csv_id3v1_formatter(id3v1_encoding::automatic,
                                 id3v1_genre_expansion::expand) << tag1;
  s = csv_stm.str();

  static const char * const GOLD_CSV =
    "1,0,The Pogues,Lorca's Novena,Hell's Ditch [Expanded] (US Ve,1990,5,255,,,,Amazon.com Song ID: 20355825";

  BOOST_CHECK(GOLD_CSV == s);

  std::stringstream std_stm;
  std_stm << standard_id3v1_formatter(id3v1_encoding::automatic,
                                      id3v1_genre_expansion::expand,
                                      4) << tag1;
  s = std_stm.str();

  static const char * const GOLD_STD =
    "ID3v1.1:\n"
"       title: Lorca's Novena\n"
"      artist: The Pogues\n"
"       album: Hell's Ditch [Expanded] (US Ve\n"
"        year: 1990\n"
"       genre: 255\n"
"       track: 5\n"
"       speed: N/A\n"
"       start: N/A\n"
"         end: N/A\n"
"     comment: Amazon.com Song ID: 20355825\n";

  BOOST_CHECK(GOLD_STD == s);

}

BOOST_AUTO_TEST_CASE( test_id3v1_b )
{
  using namespace scribbu;

  using std::back_inserter;

  using scribbu::compact_id3v1_formatter;
  using scribbu::csv_id3v1_formatter;
  using scribbu::standard_id3v1_formatter;

  const fs::path TEST_DATA("/vagrant/test/data/id3v1.2.4.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v1_tag tag1(ifs);

  BOOST_CHECK(!tag1.enhanced());
  BOOST_CHECK(!tag1.extended());
  BOOST_CHECK(!tag1.v1_1());

  BOOST_CHECK(0x0c == tag1.genre());

  std::vector<unsigned char> album;
  tag1.album(back_inserter(album));
  const std::vector<unsigned char> ALBUM = {{
      'E', 'L', 'A', ' ', 'E', ' ', 'C', 'A', 'R', 'I', 'O', 'C', 'A', 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }};
  BOOST_CHECK(album == ALBUM);

  std::vector<unsigned char> artist;
  tag1.artist(back_inserter(artist));
  const std::vector<unsigned char> ARTIST = {{
      'J', 'o', 'a', 'o', ' ', 'G', 'i', 'l', 'b', 'e', 'r', 't', 'o', 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0
    }};
  BOOST_CHECK(artist == ARTIST);

  std::vector<unsigned char> comment;
  tag1.comment(back_inserter(comment));
  const std::vector<unsigned char> COMMENT = {{
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }};
  BOOST_CHECK(comment == COMMENT);

  std::vector<unsigned char> genre2;
  tag1.genre(back_inserter(genre2));
  BOOST_CHECK(genre2.empty());

  bool valid;
  unsigned char speed;
  std::tie(valid, speed) = tag1.speed();
  BOOST_CHECK(!valid);

  std::vector<unsigned char> start_time;
  tag1.genre(back_inserter(start_time));
  BOOST_CHECK(start_time.empty());
  std::vector<unsigned char> end_time;
  tag1.genre(back_inserter(end_time));
  BOOST_CHECK(end_time.empty());

  std::vector<unsigned char> title;
  tag1.title(back_inserter(title));
  const std::vector<unsigned char> TITLE = {{
      'A', 'C', 'A', 'P', 'U', 'L', 'C', 'O', 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }};
  BOOST_CHECK(title == TITLE);

  unsigned char track_number;
  std::tie(valid, track_number) = tag1.track_number();
  BOOST_CHECK(!valid);

  unsigned char year[4];
  tag1.year(year);
  BOOST_CHECK(0 == year[0] && 0 == year[1] &&
              0 == year[2] && 0 == year[3]);

  std::stringstream compact_stm;
  compact_stm << compact_id3v1_formatter(id3v1_encoding::automatic,
                                         id3v1_genre_expansion::expand,
                                         ',') << tag1;
  std::string s = compact_stm.str();

  static const char * const GOLD_COMPACT =
    "ID3v1:Joao Gilberto,ACAPULCO,ELA E CARIOCA,,Other,";

  BOOST_CHECK(GOLD_COMPACT == s);

  std::stringstream csv_stm;
  csv_stm << csv_id3v1_formatter(id3v1_encoding::automatic,
                                 id3v1_genre_expansion::expand,
                                 ',') << tag1;
  s = csv_stm.str();

  static const char * const GOLD_CSV =
    "0,0,Joao Gilberto,ACAPULCO,ELA E CARIOCA,,,Other,,,,";

  BOOST_CHECK(GOLD_CSV == s);

  std::stringstream std_stm;
  std_stm << standard_id3v1_formatter(id3v1_encoding::automatic,
                                      id3v1_genre_expansion::expand,
                                      4) << tag1;
  s = std_stm.str();

  static const char * const GOLD_STD =
    "ID3v1:\n"
"       title: ACAPULCO\n"
"      artist: Joao Gilberto\n"
"       album: ELA E CARIOCA\n"
"        year: \n"
"       genre: Other\n"
"       track: N/A\n"
"       speed: N/A\n"
"       start: N/A\n"
"         end: N/A\n"
"     comment: \n";

  BOOST_CHECK(GOLD_STD == s);

}

BOOST_AUTO_TEST_CASE( test_id3v1_c )
{
  using namespace scribbu;

  using std::back_inserter;

  using scribbu::compact_id3v1_formatter;
  using scribbu::csv_id3v1_formatter;
  using scribbu::standard_id3v1_formatter;

  const fs::path TEST_DATA("/vagrant/test/data/id3v1-ext.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v1_tag tag1(ifs);

  BOOST_CHECK(tag1.enhanced());
  BOOST_CHECK(tag1.extended());
  BOOST_CHECK(!tag1.v1_1());

  BOOST_CHECK(0x08 == tag1.genre());

  std::vector<unsigned char> album;
  tag1.album(back_inserter(album));
  const std::vector<unsigned char> ALBUM = {{
      'B', 'o', 'o', 't', 'l', 'e', 'g', ' ', '-', ' ', 'S', 't', 'o', 'c',
      'k', 'h', 'o', 'l', 'm', ' ', 'S', 'w', 'e', 'd', 'e', 'n', ' ', '1',
      '9', '6', '0', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' '
    }};
  BOOST_CHECK(album == ALBUM);

  std::vector<unsigned char> artist;
  tag1.artist(back_inserter(artist));
  const std::vector<unsigned char> ARTIST = {{
      'J', 'o', 'h', 'n', ' ', 'C', 'o', 'l', 't', 'r', 'a', 'n', 'e', ' ',
      '&', ' ', 'M', 'i', 'l', 'e', 's', ' ', 'D', 'a', 'v', 'i', 's', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' '
    }};
  BOOST_CHECK(artist == ARTIST);

  std::vector<unsigned char> comment;
  tag1.comment(back_inserter(comment));
  const std::vector<unsigned char> COMMENT = {{
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' '
    }};
  BOOST_CHECK(comment == COMMENT);

  std::vector<unsigned char> genre2;
  tag1.genre(back_inserter(genre2));
  const std::vector<unsigned char> GENRE2 = {{
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' '
    }};
  BOOST_CHECK(GENRE2 == genre2);

  bool valid;
  unsigned char speed;
  std::tie(valid, speed) = tag1.speed();
  BOOST_CHECK(valid);
  BOOST_CHECK(0 == speed);

  std::vector<unsigned char> start_time;
  tag1.start_time(back_inserter(start_time));
  const std::vector<unsigned char> TIME = {{
      ' ', ' ', ' ', ' ', ' ', ' '
    }};
  BOOST_CHECK(start_time == TIME);
  std::vector<unsigned char> end_time;
  tag1.end_time(back_inserter(end_time));
  BOOST_CHECK(end_time == TIME);

  std::vector<unsigned char> title;
  tag1.title(back_inserter(title));
  const std::vector<unsigned char> TITLE = {{
      'O', 'n', ' ', 'G', 'r', 'e', 'e', 'n', ' ', 'D', 'o', 'l', 'p', 'h', 'i',
      'n', ' ', 'S', 't', 'r', 'e', 'e', 't', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '
    }};
  BOOST_CHECK(title == TITLE);

  unsigned char track_number;
  std::tie(valid, track_number) = tag1.track_number();
  BOOST_CHECK(!valid);

  unsigned char year[4];
  tag1.year(year);
  BOOST_CHECK('1' == year[0] && '9' == year[1] &&
              '6' == year[2] && '0' == year[3]);

  std::stringstream compact_stm;
  compact_stm << compact_id3v1_formatter(id3v1_encoding::automatic,
                                         id3v1_genre_expansion::expand,
                                         ',') << tag1;
  std::string s = compact_stm.str();

  static const char * const GOLD_COMPACT =
    "ID3v1(enhanced):John Coltrane & Miles Davis,On Green Dolphin Street,Bootleg - Stockholm Sweden 1960,1960,Jazz,speed:0,:,";

  BOOST_CHECK(GOLD_COMPACT == s);

  std::stringstream csv_stm;
  csv_stm << csv_id3v1_formatter(id3v1_encoding::automatic,
                                 id3v1_genre_expansion::expand,
                                 ',') << tag1;
  s = csv_stm.str();

  static const char * const GOLD_CSV =
    "0,1,John Coltrane & Miles Davis,On Green Dolphin Street,Bootleg - Stockholm Sweden 1960,1960,,Jazz,0,,,";

  BOOST_CHECK(GOLD_CSV == s);

  std::stringstream std_stm;
  std_stm << standard_id3v1_formatter(id3v1_encoding::automatic,
                                      id3v1_genre_expansion::expand,
                                      4) << tag1;
  s = std_stm.str();

  static const char * const GOLD_STD =
    "ID3v1(enhanced):\n"
"       title: On Green Dolphin Street\n"
"      artist: John Coltrane & Miles Davis\n"
"       album: Bootleg - Stockholm Sweden 1960\n"
"        year: 1960\n"
"       genre: Jazz\n"
"       track: N/A\n"
"       speed: 0\n"
"       start: \n"
"         end: \n"
"     comment: \n";

  BOOST_CHECK(GOLD_STD == s);

}

BOOST_AUTO_TEST_CASE( test_jing_jing_1 )
{
  using namespace scribbu;

  using scribbu::compact_id3v1_formatter;
  using scribbu::csv_id3v1_formatter;
  using scribbu::standard_id3v1_formatter;

  const fs::path TEST_DATA("/vagrant/test/data/红颜旧.mp3");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v1_info I1 = ends_in_id3v1(ifs);
  BOOST_CHECK(scribbu::id3_v1_tag_type::none == I1.type_);

}

/**
 * \brief Exercise an ID3v1 tag, converting it to UTF-8
 *
 \code

  mgh@Crickhollow[2-0:...ode/projects/scribbu]: !od
  od -A x -t x1z test/data/elliot-goldenthal.id3v1.tag
  000000 54 41 47 45 61 73 74 65 72 20 52 65 62 65 6c 6c  >TAGEaster Rebell<
  000010 69 6f 6e 20 28 50 65 72 66 6f 72 6d 65 64 20 62  >ion (Performed b<
  000020 79 53 69 6e 65 61 64 20 4f 27 43 6f 6e 6e 6f 72  >ySinead O'Connor<
  000030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 4d  >...............M<
  000040 69 63 68 61 65 6c 20 43 6f 6c 6c 69 6e 73 00 00  >ichael Collins..<
  000050 00 00 00 00 00 00 00 00 00 00 00 00 00 31 39 39  >.............199<
  000060 36 52 69 70 70 65 64 20 62 79 20 57 69 6e 61 6d  >6Ripped by Winam<
  000070 70 20 6f 6e 20 50 69 6d 70 65 72 6e 65 00 01 ff  >p on Pimperne...<
  000080

 \endcode
 *
 *
 */

BOOST_AUTO_TEST_CASE( test_elliot_goldenthal )
{
  using namespace std;
  using namespace scribbu;

  using scribbu::compact_id3v1_formatter;
  using scribbu::csv_id3v1_formatter;
  using scribbu::standard_id3v1_formatter;

  const fs::path TEST_DATA("/vagrant/test/data/elliot-goldenthal.id3v1.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v1_info info = ends_in_id3v1(ifs);
  BOOST_CHECK( id3_v1_tag_type::v_1 == info.type_ );

  unique_ptr<id3v1_tag> ptag = process_id3v1(ifs);

  vector<unsigned char> title;
  ptag->title(back_inserter(title));

  vector<unsigned char> artist;
  ptag->artist(back_inserter(artist));

  scribbu::detail::iconv_guard guard("UTF-8", "ISO-8859-1");

  unique_ptr<unsigned char[]> ptitle( new unsigned char[title.size()] );
  copy(title.begin(), title.end(), ptitle.get());

  unique_ptr<unsigned char[]> partist( new unsigned char[artist.size()] );
  copy(artist.begin(), artist.end(), partist.get());

  string text = scribbu::detail::to_utf8(guard, ptitle.get(), title.size());
  BOOST_CHECK("Easter Rebellion (Performed by" == text);

  text = scribbu::detail::to_utf8(guard, partist.get(), artist.size());
  BOOST_CHECK("Sinead O'Connor" == text);

  std::stringstream compact_stm;
  compact_stm << compact_id3v1_formatter(id3v1_encoding::automatic,
                                         id3v1_genre_expansion::expand,
                                         ',') << *ptag;
  std::string s = compact_stm.str();

  static const char * const GOLD_COMPACT =
    "ID3v1.1:Sinead O'Connor,Easter Rebellion (Performed by,Michael Collins,1996,genre:255,track:1,Ripped by Winamp on Pimperne";

  BOOST_CHECK(GOLD_COMPACT == s);

  std::stringstream csv_stm;
  csv_stm << csv_id3v1_formatter(id3v1_encoding::automatic,
                                 id3v1_genre_expansion::expand,
                                 ',') << *ptag;
  s = csv_stm.str();

  static const char * const GOLD_CSV =
    "1,0,Sinead O'Connor,Easter Rebellion (Performed by,Michael Collins,1996,1,255,,,,Ripped by Winamp on Pimperne";

  BOOST_CHECK(GOLD_CSV == s);

  std::stringstream std_stm;
  std_stm << standard_id3v1_formatter(id3v1_encoding::automatic,
                                      id3v1_genre_expansion::expand,
                                      4) << *ptag;
  s = std_stm.str();

  static const char * const GOLD_STD =
    "ID3v1.1:\n"
"       title: Easter Rebellion (Performed by\n"
"      artist: Sinead O'Connor\n"
"       album: Michael Collins\n"
"        year: 1996\n"
"       genre: 255\n"
"       track: 1\n"
"       speed: N/A\n"
"       start: N/A\n"
"         end: N/A\n"
"     comment: Ripped by Winamp on Pimperne\n";

  BOOST_CHECK(GOLD_STD == s);

}
