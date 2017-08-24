#include "command-utilities.hh"

#include <unordered_map>

namespace po = boost::program_options;

namespace {

  typedef std::unordered_map<std::string, handler_type> handler_map;

  handler_map& get_handler_map()
  {
    static handler_map the_map;
    return the_map;
  }
  
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

register_command::register_command(const std::string &s, handler_type f)
{
  handler_map &M = get_handler_map();
  if (M.count(s)) {
    throw std::out_of_range(s);
  }
  M[s] = f;
}

void
print_usage(std::ostream                  &os,
            const po::options_description &opts,
            const std::string             &usage)
{
  os << usage << opts << std::endl;
}
