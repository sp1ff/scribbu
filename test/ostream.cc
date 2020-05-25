/**
 * \file ostream.cc
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

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
