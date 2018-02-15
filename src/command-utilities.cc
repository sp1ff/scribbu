#include "command-utilities.hh"

#include <sstream>
#include <unordered_map>

#include <boost/algorithm/string/replace.hpp>

namespace po = boost::program_options;

namespace {

  typedef std::unordered_map<std::string, handler_type> handler_map;

  handler_map& get_handler_map()
  {
    static handler_map the_map;
    return the_map;
  }
  
}

register_command::register_command(const std::string &s, handler_type f)
{
  handler_map &M = get_handler_map();
  if (M.count(s)) {
    throw std::out_of_range(s);
  }
  M[s] = f;
}

bool
has_sub_command(const std::string &s)
{
  return 0 != get_handler_map().count(s);
}

handler_type get_sub_command(const std::string &s)
{
  return get_handler_map().at(s);
}

void
print_usage(std::ostream                  &os,
            const po::options_description &opts,
            const std::string             &usage)
{
  using namespace std;

  // Workaround for https://svn.boost.org/trac10/ticket/4644; marked as fixed
  // some time ago, but still happening...
  stringstream stm4644;
  stm4644 << opts;
  string str4644 = stm4644.str();
  boost::algorithm::replace_all(str4644, "--h ", "-h  ");
  os << usage << str4644  << std::endl;
}
