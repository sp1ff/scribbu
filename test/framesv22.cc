#include <scribbu/framesv22.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_unknown_id3v2_2_frame )
{
  using scribbu::frame_id3;
  using scribbu::unknown_id3v2_2_frame;

  const unsigned char DATA[] = {
    0xba, 0xbe, 0xba, 0xbe
  };

  unknown_id3v2_2_frame F("XXX", DATA, DATA + sizeof(DATA));

  BOOST_CHECK( frame_id3("XXX") == F.id() );

  unsigned char x[4];
  BOOST_CHECK( x + 4 == F.data(x) );
  BOOST_CHECK( 0 == memcmp(DATA, x, sizeof(DATA)) );

} // End test_id3v2_2_frame.

// TODO: test class id3v2_2_text_frame, once I get some test data
// TODO: test class TXX, once I get some test data

BOOST_AUTO_TEST_CASE( test_com )
{
  using scribbu::comments;
  using scribbu::frame_id3;
  using scribbu::COM;

  const unsigned char DATA[] = {
    00,                               // Unicode
    'e', 'n', 'g',                    // Language
    'f', 'o', 'o', 0,                 // short description
    'b', 'a', 'r'
  };

  COM C(DATA, DATA + sizeof(DATA));

  BOOST_CHECK( frame_id3("COM") == C.id() );

  BOOST_CHECK( 0 == C.unicode() );

  unsigned char buf[3];
  BOOST_CHECK( buf + 3 == C.lang(buf) );
  BOOST_CHECK( 'e' == buf[0] && 'n' == buf[1] && 'g' == buf[2] );

  BOOST_CHECK( buf + 3 == C.descriptionb(buf) );
  BOOST_CHECK( 'f' == buf[0] && 'o' == buf[1] && 'o' == buf[2] );

  BOOST_CHECK( buf + 3 == C.textb(buf) );
  BOOST_CHECK( 'b' == buf[0] && 'a' == buf[1] && 'r' == buf[2] );

} // End test_com.

// TODO: Unit test class POP, once I get some data...
