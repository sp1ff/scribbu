#ifndef COMMAND_UTILITIES_HH_INCLUDED
#define COMMAND_UTILITIES_HH_INCLUDED 1
/**
 * \brief Assorted sub-command-related utilities
 *
 *
 * TODO: Write me!
 *
 *
 */

#include <iostream>

#include <boost/filesystem.hpp>
#include <boost/optional/optional.hpp>
#include <boost/program_options.hpp>


const int EXIT_INCORRECT_USAGE = 2;


enum class help_level {
  none, regular, verbose
};

typedef 
std::function<int(const std::vector<std::string> 		 &opts, 
		  help_level                     		  help,          
		  const boost::optional<boost::filesystem::path> &cfg)> 
handler_type;

handler_type get_sub_command(const std::string &s);

struct register_command 
{
  register_command(const std::string &s, handler_type f);
};

void 
print_usage(std::ostream                                      &os,
	    const boost::program_options::options_description &opts,
	    const std::string                                 &usage);

#endif // not COMMAND_UTILITIES_HH_INCLUDED
