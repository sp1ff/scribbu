#ifndef UNIT_HH_INCLUDED
#define UNIT_HH_INCLUDED

#include <boost/filesystem.hpp>

/// Retrieve the test source directory ($srcdir)
boost::filesystem::path
get_source_directory();

/// Retrieve the test data directory
boost::filesystem::path
get_data_directory();

#endif // not UNIT_HH_INCLUDED
