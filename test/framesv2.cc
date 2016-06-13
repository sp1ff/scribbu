#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>
#include <scribbu/scribbu.hh>
#include <scribbu/framesv2.hh>

BOOST_AUTO_TEST_CASE( test_frame_id3 )
{
  using scribbu::frame_id3;

  const unsigned char ID[3] = {'T', 'I', 'T'};
  frame_id3 fid1('T', 'I', 'T'), fid2(ID), fid3("TIT");

  BOOST_CHECK( std::string("TIT") == fid1.as_string() );
  BOOST_CHECK( std::string("TIT") == fid2.as_string() );
  BOOST_CHECK( std::string("TIT") == fid3.as_string() );

  unsigned char id[3];
  BOOST_CHECK( id + 3 == fid1.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );
  BOOST_CHECK( id + 3 == fid2.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );
  BOOST_CHECK( id + 3 == fid3.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] );

  BOOST_CHECK( fid1 == fid2 );
  BOOST_CHECK( fid2 == fid3 );

  std::stringstream stm;
  stm << fid1;
  BOOST_CHECK( std::string("TIT") == stm.str() );

  BOOST_CHECK( ! fid1.experimental() );

} // End test_frame_id3.

BOOST_AUTO_TEST_CASE( test_frame_id4 )
{
  using scribbu::frame_id4;

  const unsigned char ID[4] = {'T', 'I', 'T', '2'};
  frame_id4 fid1('T', 'I', 'T', '2'), fid2(ID),
    fid3("TIT2");

  BOOST_CHECK( std::string("TIT2") == fid1.as_string() );
  BOOST_CHECK( std::string("TIT2") == fid2.as_string() );
  BOOST_CHECK( std::string("TIT2") == fid3.as_string() );

  unsigned char id[4];
  BOOST_CHECK( id + 4 == fid1.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );
  BOOST_CHECK( id + 4 == fid2.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );
  BOOST_CHECK( id + 4 == fid3.copy(id) );
  BOOST_CHECK( 'T' == id[0] && 'I' == id[1] && 'T' == id[2] && '2' == id[3] );

  BOOST_CHECK( fid1 == fid2 );
  BOOST_CHECK( fid2 == fid3 );

  std::stringstream stm;
  stm << fid1;
  BOOST_CHECK( std::string("TIT2") == stm.str() );

  BOOST_CHECK( ! fid1.experimental() );

} // End test_frame_id4.
