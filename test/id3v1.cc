#include <scribbu/id3v1.hh>

#include <iostream>
#include <memory>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/framesv2.hh>

namespace fs = boost::filesystem;

// Test cases:

// 1. "A"/test_id3v1_a/id3v1.2.3.tag-- ID3v1.1 tag from 'Pogues, The - Lorca's Novena.mp3'
// 2. "B"/test_id3v1_b/id3v1.2.4.tag-- ID3v1 tag from 'Joao Gilberto - Acapulco.mp3'
// 3. "C"/test_id3v1_c/id3v1-ext.tag-- IDv1 Extended tag from 'Mike Murley - Sleepwalking.mp3'

BOOST_AUTO_TEST_CASE( test_id3v1_a )
{
  using namespace std;
  using namespace scribbu;

  const fs::path TEST_DATA("/vagrant/test/data/id3v1.2.3.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);

  id3v1_tag tag(ifs);

  BOOST_CHECK(!tag.enhanced());
  BOOST_CHECK(!tag.extended());
  BOOST_CHECK(tag.v1_1());

  BOOST_CHECK(0xff == tag.genre());

  vector<unsigned char> album;
  tag.album(back_inserter(album));
  const vector<unsigned char> ALBUM = {{
      'H', 'e', 'l', 'l', '\'', 's', ' ', 'D', 'i', 't', 'c', 'h', ' ', '[',
      'E', 'x', 'p', 'a', 'n', 'd', 'e', 'd', ']', ' ', '(', 'U', 'S', ' ',
      'V', 'e'
    }};
  BOOST_CHECK(album == ALBUM);

  string s = tag.album<string>(encoding::ASCII, encoding::UTF_8,
                                on_no_encoding::fail);
  BOOST_CHECK(s == string{"Hell's Ditch [Expanded] (US Ve"});

  vector<unsigned char> artist;
  tag.artist(back_inserter(artist));
  const vector<unsigned char> ARTIST = {{
      'T', 'h', 'e', ' ', 'P', 'o', 'g', 'u', 'e', 's', 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }};
  BOOST_CHECK(artist == ARTIST);

  s = tag.artist<string>();
  BOOST_CHECK(s == string{"The Pogues"});

  vector<unsigned char> comment;
  tag.comment(back_inserter(comment));
  const vector<unsigned char> COMMENT = {{
      'A', 'm', 'a', 'z', 'o', 'n', '.', 'c', 'o', 'm', ' ', 'S', 'o', 'n',
      'g', ' ', 'I', 'D', ':', ' ', '2', '0', '3', '5', '5', '8', '2', '5',
    }};
  BOOST_CHECK(comment == COMMENT);

  s = tag.comment<string>(encoding::UTF_8, encoding::ASCII);
  BOOST_CHECK(s == string{"Amazon.com Song ID: 20355825"});

  vector<unsigned char> genre2;
  tag.enh_genre(back_inserter(genre2));
  BOOST_CHECK(genre2.empty());

  s = tag.enh_genre<string>(encoding::ASCII);
  BOOST_CHECK(s.empty());

  bool valid;
  unsigned char speed;
  tie(valid, speed) = tag.speed();
  BOOST_CHECK(!valid);

  vector<unsigned char> title;
  tag.title(back_inserter(title));
  const vector<unsigned char> TITLE = {{
      'L', 'o', 'r', 'c', 'a', '\'', 's', ' ', 'N', 'o', 'v', 'e', 'n', 'a',
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    }};
  BOOST_CHECK(title == TITLE);

  s = tag.title<string>(encoding::ISO_8859_1, encoding::ISO_8859_1);
  BOOST_CHECK(s == string{"Lorca's Novena"});

  unsigned char track_number;
  tie(valid, track_number) = tag.track_number();
  BOOST_CHECK(valid);
  BOOST_CHECK(0x05 == track_number);

  unsigned char year[4];
  tag.year(year);
  BOOST_CHECK('1' == year[0] && '9' == year[1] &&
              '9' == year[2] && '0' == year[3]);

  s = tag.year<string>();
  BOOST_CHECK(s == string{"1990"});
}

BOOST_AUTO_TEST_CASE( test_id3v1_b )
{
  using namespace scribbu;

  using std::back_inserter;

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
  tag1.enh_genre(back_inserter(genre2));
  BOOST_CHECK(genre2.empty());

  bool valid;
  unsigned char speed;
  std::tie(valid, speed) = tag1.speed();
  BOOST_CHECK(!valid);

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
}

BOOST_AUTO_TEST_CASE( test_id3v1_c )
{
  using namespace scribbu;

  using std::back_inserter;

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
  tag1.enh_genre(back_inserter(genre2));
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
}

BOOST_AUTO_TEST_CASE( test_jing_jing_1 )
{
  using namespace scribbu;

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

  const fs::path TEST_DATA("/vagrant/test/data/elliot-goldenthal.id3v1.tag");

  fs::ifstream ifs(TEST_DATA, fs::ifstream::binary);
  id3v1_info info = ends_in_id3v1(ifs);
  BOOST_CHECK( id3_v1_tag_type::v_1 == info.type_ );

  unique_ptr<id3v1_tag> ptag = process_id3v1(ifs);

  vector<unsigned char> title;
  ptag->title(back_inserter(title));

  string text;

  text = convert_encoding<string>(&(title[0]), title.size(), encoding::UTF_8,
                                  encoding::ISO_8859_1);
  BOOST_CHECK("Easter Rebellion (Performed by" == text);

  text = ptag->title<string>();
  BOOST_CHECK("Easter Rebellion (Performed by" == text);

  vector<unsigned char> artist;
  ptag->artist(back_inserter(artist));

  text = convert_encoding<string>(&(artist[0]), artist.size(), encoding::UTF_8,
                                  encoding::ISO_8859_1);
  BOOST_CHECK("Sinead O'Connor" == text);

  text = ptag->artist<string>();
  BOOST_CHECK("Sinead O'Connor" == text);
}
