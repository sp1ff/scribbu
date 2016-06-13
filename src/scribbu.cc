#include "config.h"
#include "command.hh"
#include "command-utilities.hh"
#include "rename.hh"
#include "report.hh"
#include "split.hh"
#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>
#include <scribbu/scribbu.hh>

namespace po = boost::program_options;

namespace {

  void
  print_version(std::ostream &os)
  {
    using namespace std;
    os << PACKAGE_STRING << endl;
  }

  std::map<command, handler_type> COMMANDS = {
    { command::rename, handler_type(handle_rename) },
    { command::report, handler_type(handle_report) },
    { command::split,  handler_type(handle_split)  }
  };

  const std::string USAGE("scribbu -- tag your music\n\n");

}

int
main(int argc, char * argv[])
{
  using namespace std;

  using boost::lexical_cast;

  scribbu::static_initialize();

  int status = EXIT_SUCCESS;

  po::options_description gopts("Global options");
  gopts.add_options()
    ("help,h", "print the " PACKAGE " usage message & exit with status zero")
    ("version,v", "print the " PACKAGE " version & exit with status zero");

  po::options_description xgopts("Developer-only global options");
  xgopts.add_options()
    ("man", "print the " PACKAGE " usage message including developer-only"
     " options & exit with status zero");

  po::options_description hidden;
  hidden.add_options()
    ("sub-command", po::value<vector<string>>());

  po::options_description global;
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

    // We can't just check for '--hep' (and so forth), because the
    // user could have typed something like 'scribbu list --help', in
    // which case they're seeking help on the list command
    // specifically.

    // --version is unambiguous
    if (vm.count("version")) {
      print_version(cout);
    } else {

      help_level help = help_level::none;
      if (vm.count("help")) {
        help = help_level::regular;
      } else if (vm.count("man")) {
        help = help_level::verbose;
      }

      // 'vm["sub-command"]' will only contain positional options, not
      // flags. To get everything, we need to do this:
      vector<string> opts =
        po::collect_unrecognized(parsed.options, po::include_positional);

      if (opts.size()) {

        // 'opts' is non-empty, so there's at least one element--
        // convert that to a 'command'...
        command cmd = lexical_cast<command>(opts.front());
        // pop it...
        opts.erase(opts.begin());
        // and use the 'command' as an index into a global lookup
        // table of handlers.
        status = COMMANDS[cmd](opts, help);

      } else {

        // If help has been requested, give it & bail.
        if (help_level::regular == help) {
          print_usage(cout, gopts, USAGE);
        } else if (help_level::verbose == help) {
          print_usage(cout, global, USAGE);
        } else {
          throw po::error("no command specified");
        }

      }
    }
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
