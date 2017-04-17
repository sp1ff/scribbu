#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/id3v2-utils.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_maybe_read_id3 )
{
  using scribbu::id3v2_tag;

  const fs::path TEST_FILE("/vagrant/test/data/lorca.mp3");

  fs::ifstream ifs(TEST_FILE);
  std::unique_ptr<id3v2_tag> ptag = scribbu::maybe_read_id3v2(ifs);
  BOOST_CHECK((bool)ptag);
  BOOST_CHECK(3 == ptag->version());
  BOOST_CHECK(0 == ptag->revision());
  BOOST_CHECK(452951 == ptag->size());
  BOOST_CHECK(0 == ptag->flags());
  BOOST_CHECK(!ptag->unsynchronised());

  BOOST_CHECK("Hell's Ditch [Expanded] (US Version)" == ptag->album());
  BOOST_CHECK("The Pogues" == ptag->artist());
  BOOST_CHECK("Pop" == ptag->content_type());
  BOOST_CHECK("Lorca's Novena" == ptag->title());

}

BOOST_AUTO_TEST_CASE( test_template_text )
{
  const fs::path TEST_FILE("/vagrant/test/data/lorca.mp3");

  using scribbu::template_processor;

  template_processor P1("%(title:output=utf-8&compress) - %A( \\(%(album)\\))?%E");
  std::string S = P1(TEST_FILE);
  BOOST_CHECK("Lorca's Novena - Pogues, The (Hell's Ditch [Expanded] (US Version)).mp3" == S);

}
