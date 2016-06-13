#include "command-utilities.hh"

namespace po = boost::program_options;

void
print_usage(std::ostream &os,
            const po::options_description &opts,
            const std::string &usage)
{
  using namespace std;
  os << usage << opts << endl;
}
