#include "config.h"

#include <iostream>

#include <boost/lexical_cast.hpp>

#include "command-utilities.hh"

#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {

  const std::string USAGE(R"(scribbu -- tag your music

scribbu is tool for managing your MP3 collection.

Usage:
    scribbu [OPTION...] SUB-COMMAND

where SUB-COMMAND is one of:

    - split: split a file into ID3v2 tag(s), track data, and ID3v1 tag
    - rename: rename a file based on it's tag(s)
    - report: generate a report on the contents of one or more directories
    - dump: dump the tags for one or more files or directories

For sub-command help, run 'scribbu SUB-COMMAND --help'
)");
  
  const fs::path DEFCFG("~/.scribbu");
}

int
main(int argc, char * argv[])
{
  using namespace std;

  using boost::lexical_cast;
  using boost::optional;

  scribbu::static_initialize();

  int status = EXIT_SUCCESS;

  // Global options, or options that apply to all sub-commands, should
  // be defined here. Each sub-command will define & parse its own options
  // separately.
  po::options_description gopts("Global options");
  gopts.add_options()
    ("config,c", po::value<fs::path>()->default_value(DEFCFG),
     "path (absolute or relative) to the config file")
    ("help,h", "print the " PACKAGE " usage message & exit with status zero")
    ("version,v", "print the " PACKAGE " version & exit with status zero");

  po::options_description xgopts("Developer-only global options");
  xgopts.add_options()
    ("man", "print the " PACKAGE " usage message including developer-only"
     " options & exit with status zero");

  po::options_description hidden;
  hidden.add_options()
    ("sub-command", po::value<vector<string>>());

  po::options_description global; // for error reporting, below.
  global.add(gopts).add(xgopts);

  try {

    po::options_description allopts;
    allopts.add(gopts).add(xgopts).add(hidden);

    // In order to have boost accept later positional parameters, it
    // seems we need to stuff them all one option (as opposed to
    // making position 1 the command and all the rest something else).
    po::positional_options_description popts;
    popts.add("sub-command", -1);

    po::parsed_options parsed = po::command_line_parser(argc, argv).
      options(allopts).
      positional(popts).
      allow_unregistered().
      run();

    po::variables_map vm;
    po::store(parsed, vm);

    // At this point, any global options we've defined ('--help',
    // '--version' &c) have been parsed out and placed in 'vm',
    // regardless of where they appear. So if, for instance, the user
    // typed something like 'scribbu list --help', (i.e. they're
    // seeking help on the list command specifically), the '--help'
    // has already been removed from 'opts' (see which below).

    // Furthermore, different global options interact with
    // sub-commands in different ways:

    // 	 - version can only be given with no sub-command
    // 	 - config can only be given *with* a sub-command
    // 	 - help can be given either way, but it's behaviour will differ

    // 'vm["sub-command"]' will only contain positional options, not
    // flags. To get everything, we need to do this:
    vector<string> opts = po::collect_unrecognized(parsed.options, po::include_positional);

    if (vm.count("version")) {
      if (opts.size()) {
	throw po::error("unrecognized argument: --version");
      } else {
	cout << PACKAGE_STRING << std::endl;
      }
    } else if (!vm["config"].defaulted() && !opts.size()) {
      throw po::error("configuration specified, but nothing asked");
    } else {
      
      // If we're here, we going to do *something*, albeit as little
      // as printing help; how much help was requested (if any)?
      help_level help = help_level::none;
      if (vm.count("man")) {
        help = help_level::verbose;
      } else if (vm.count("help")) {
        help = help_level::regular;
      }

      // Was a config file given?
      optional<fs::path> cfg;
      fs::path pth = vm["config"].as<fs::path>();
      if (fs::exists(pth)) {
	cfg = pth;
      } else if (!vm["config"].defaulted()) {
	// If it was explicitly given, and it doesn't exist, that's an error.
	throw po::error(pth.native() + " does not exist");
      } 

      if (opts.size()) {

        // 'opts' is non-empty, so there's at least one element--
        // attempt to resolve the first element to a command...
        handler_type f = get_sub_command(opts.front());
        // pop it...
        opts.erase(opts.begin());
        // and dispatch.
        status = f(opts, help, cfg);

      } else {

	// No sub-command has been given, so we're just printing help.
        if (help_level::verbose == help) {
          print_usage(cout, global, USAGE);
        } else {
          print_usage(cout, gopts, USAGE);
	}

      } // End if on 'opts' size.
    } // End if on '--version' &c.
  } catch (const po::error &ex) {
    cerr << ex.what() << endl; 
    print_usage(cerr, gopts, USAGE);
    status = EXIT_INCORRECT_USAGE;
  } catch (const std::exception &ex) {
    cerr << ex.what() << endl;
    status = EXIT_FAILURE;
  }

  scribbu::static_cleanup();

  return status;

} // End main.
