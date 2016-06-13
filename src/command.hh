#ifndef COMMAND_HH_INCLUDED
#define COMMAND_HH_INCLUDED 1

#include <iostream>

enum class command {
  report,
  rename,
  split
};

std::ostream& operator<<(std::ostream &os, command x);

std::istream& operator>>(std::istream &is, command &x);

#endif // not COMMAND_HH_INCLUDED
