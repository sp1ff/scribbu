#include <scribbu/framesv24.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_encr_2_4 )
{
  using scribbu::encryption_method;
  using scribbu::id3v2_3_plus_frame;
  using scribbu::ENCR_2_4;

  const unsigned char DATA[] = {
    'm', 'e', 0,
    1,
    1, 2, 3
  };

  ENCR_2_4 encr(DATA, DATA + sizeof(DATA),
                id3v2_3_plus_frame::tag_alter_preservation::preserve,
                id3v2_3_plus_frame::file_alter_preservation::preserve,
                id3v2_3_plus_frame::read_only::set,
                boost::none,
                boost::none,
                false,
                false,
                boost::none);

  unsigned char buf[3];
  BOOST_CHECK( buf + 2 == encr.emailb(buf) );
  BOOST_CHECK( 'm' == buf[0] && 'e' == buf[1] );
  BOOST_CHECK( 1 == encr.method_symbol() );
  BOOST_CHECK( buf + 3 == encr.datab(buf) );
  BOOST_CHECK( 1 == buf[0] && 2 == buf[1] && 3 == buf[2] );

} // End test_encr_2_4.

// TODO: Unit test class id3v2_4_text_frame
// TODO: Unit test class TXXX_2_4, once I get some data
// TODO: Unit test class COMM_2_4, once I get some data
// TODO: Unit test class POPM_2_4, once I get some data
