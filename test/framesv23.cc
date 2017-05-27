#include <scribbu/framesv23.hh>

#include <boost/filesystem/fstream.hpp>
#include <boost/test/unit_test.hpp>

#include <scribbu/scribbu.hh>

BOOST_AUTO_TEST_CASE( test_ufid )
{
  using scribbu::unique_file_id;
  using scribbu::id3v2_3_plus_frame;
  using scribbu::UFID;

  const unsigned char DATA[] = {
    's', 'p', '1', 'f', 'f', 0,
    1, 2, 3
  };

  UFID ufid(DATA, DATA + sizeof(DATA),
            id3v2_3_plus_frame::tag_alter_preservation::preserve,
            id3v2_3_plus_frame::file_alter_preservation::preserve,
            id3v2_3_plus_frame::read_only::set,
            boost::none,
            boost::none,
            boost::none);

  BOOST_CHECK( "UFID" == ufid.id() );

  unsigned char buf[5];
  BOOST_CHECK( buf + 5 == ufid.ownerb(buf) );
  BOOST_CHECK( 's' == buf[0] && 'p' == buf[1] &&
               '1' == buf[2] && 'f' == buf[3] &&
               'f' == buf[4] );
  BOOST_CHECK( buf + 3 == ufid.idb(buf) );
  BOOST_CHECK( 1 == buf[0] && 2 == buf[1] && 3 == buf[2] );

} // End test_ufid.
