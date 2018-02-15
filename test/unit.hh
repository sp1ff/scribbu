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
