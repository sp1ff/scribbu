/**
 * \file unit.hh
 *
 * Copyright (C) 2015-2021 Michael Herstine <sp1ff@pobox.com>
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

#ifndef UNIT_HH_INCLUDED
#define UNIT_HH_INCLUDED

#include <boost/filesystem.hpp>

/// Retrieve the test source directory (i.e. the autoconf $srcdir)
boost::filesystem::path
get_source_directory();

/// Retrieve the test data directory (i.e $srcdir/test)
boost::filesystem::path
get_data_directory();

/// Compute the MD5 checksum of the given file; \a md5 must be at least
/// sixteen bytes in size
void
compute_md5(const boost::filesystem::path &pth, unsigned char md5[]);

#endif // not UNIT_HH_INCLUDED
