/**
 * \file json-pprinter.cc
 *
 * Copyright (C) 2025 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/json-pprinter.hh>

#include "unit.hh"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_CASE(test_json_pprinter_escape)
{
  using namespace scribbu;

  BOOST_CHECK("a" == json_pprinter::escape("a"));
  BOOST_TEST_MESSAGE(json_pprinter::escape("a	b"));
  BOOST_CHECK( "a\\tb" == json_pprinter::escape("a	b"));
  BOOST_CHECK( "the \\\"internet\\\"" == json_pprinter::escape("the \"internet\""));
}
