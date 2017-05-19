#include <scribbu/ostream.hh>

#include <sstream>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE( test_ienc )
{
  using namespace std;
  using namespace scribbu;

  stringstream os;
  os << ienc(encoding::CP1252, on_no_encoding::fail);

  ienc x = ienc::retrieve(os);
  BOOST_CHECK(encoding::CP1252 == x.get_encoding());
  BOOST_CHECK(on_no_encoding::fail == x.get_on_no_encoding());

  stringstream is;
  is >> ienc(encoding::ISO_8859_1, on_no_encoding::fail);

  ienc y = ienc::retrieve(is);
  BOOST_CHECK(encoding::ISO_8859_1 == y.get_encoding());
  BOOST_CHECK(on_no_encoding::fail == y.get_on_no_encoding());
}
