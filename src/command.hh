#ifndef COMMAND_HH_INCLUDED
#define COMMAND_HH_INCLUDED 1

#include <iostream>

enum class command {
  /// Dump ID3 tag contents
  dump,
  /// Find files using scribbu conditions
  find,
  /// Generate a report on ID3 tags in a set of files
  report,
  /// Rename files based on their ID3 tags
  rename,
  /// Run a scribbu program
  run,
  /// Split a file into ID3 tags & track data
  split
};

std::ostream& operator<<(std::ostream &os, command x);

std::istream& operator>>(std::istream &is, command &x);

#endif // not COMMAND_HH_INCLUDED
