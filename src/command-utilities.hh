#ifndef COMMAND_UTILITIES_HH_INCLUDED
#define COMMAND_UTILITIES_HH_INCLUDED 1

#include <iostream>
#include <boost/program_options.hpp>

const int EXIT_INCORRECT_USAGE = 2;

enum class help_level {
  none, regular, verbose
};

typedef std::function<int(const std::vector<std::string>&, help_level)> handler_type;

void print_usage(std::ostream &os,
                 const boost::program_options::options_description &opts,
                 const std::string &usage);

#endif // not COMMAND_UTILITIES_HH_INCLUDED
