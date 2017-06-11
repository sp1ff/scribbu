#include "config.h"

#include <signal.h>
#include <sys/resource.h>

#include <iostream>

#include <boost/lexical_cast.hpp>

#include <libguile.h>

#include "command-utilities.hh"

#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {

  void*
  register_functions (void* data)
  {
    return NULL;
  }

  void* evaluate_expression(void *p) {
    using namespace std;
    string s = *reinterpret_cast<string*>(p);
    cout << "evaluate_expression: " << s << endl;
    return (void*)11;
  }

  void* evaluate_file(void *p) {
    using namespace std;
    fs::path pth = *reinterpret_cast<fs::path*>(p);
    cout << "evaluate_file: " << pth << endl;
    return (void*)12;
  }

  // TODO: Update scribbu.cc USAGE messagevvvvvvvvvvvv
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

  /////////////////////////////////////////////////////////////////////////////
  //                        scribbu signal handling                          //
  /////////////////////////////////////////////////////////////////////////////

  volatile sig_atomic_t fatal_error_in_progress = 0;

  // Largely lifted from the example in the glibc manual, cf.
  // https://www.gnu.org/software/libc/manual/html_mono/libc.html#Termination-in-Handler
  void sig_handler(int signum)
  {
    // Since a handler can be established for more than one kind of signal, 
    // it might still get invoked recursively by delivery of some other kind
    // of signal. Use a static variable to keep track of that
    if (fatal_error_in_progress) {
      raise(signum);
    }

    fatal_error_in_progress = 1;

    if (SIGSEGV == signum) {
      write(2, "SEGV: if this did not produce a core file, type 'ulimit -c unl"
            "imited' and re-run this command to generate one.\n", 111);
    }
    else {
      write(2, "Unknown signal-- this is a bug that should be reported at http"
            "s://github.com/sp1ff/scribbu\n", 91);
    }

    // Now reraise the signal.  We reactivate the signal’s default handling,
    // which is to terminate the process.  We could just call exit or abort,
    // but reraising the signal sets the return status from the process
    // correctly.
    signal(signum, SIG_DFL);
    raise(signum);
  }

}

int
main(int argc, char * argv[])
{
  using namespace std;

  using boost::lexical_cast;
  using boost::optional;

  // Setup an alternate stack, so our signal handler will still work in the
  // case of stack overflow (which will cause a SIGSEGV)
  static char stack[SIGSTKSZ];
  stack_t ss = { 0 };
  ss.ss_size = SIGSTKSZ;
  ss.ss_sp = stack;
  sigaltstack(&ss, 0);

# ifdef SCRIBBU_ALWAYS_DUMP_CORE
  // In general, one needs to run ulimit (e.g. 'ulimit -c unlimited') 
  // *before* running the program that's SEGV'ing (or whatever). However,
  // during development, it can be handy to have the program dump core
  // regardless.
  struct rlimit lim;
  lim.rlim_cur = lim.rlim_max = RLIM_INFINITY;
  if (0 != setrlimit(RLIMIT_CORE, &lim)) {
      cerr << "WARNING: setrlimit failed; cores may not be dumped in " <<
        "the event of signals. errno reports " << errno << endl;
  }
# endif

  // Finally, setup the signal handler.
  struct sigaction sigact = { 0 }, oldact = { 0 };
  sigact.sa_handler = sig_handler;
  sigact.sa_flags = SA_ONSTACK;
  if (0 != sigaction (SIGSEGV, &sigact, &oldact)) {
    cerr << "Failed to set signal handler." << endl;
    return 3;
  }

  scribbu::static_initialize();

  int status = EXIT_SUCCESS;

  // Global options, or options that apply to all sub-commands, should
  // be defined here. Each sub-command will define & parse its own options
  // separately.
  po::options_description gopts("Global options");
  gopts.add_options()
    ("config,c", po::value<fs::path>()->default_value(DEFCFG),
     "path (absolute or relative) to the config file")
    ("expression,e", po::value<string>(), "Expression to evaluate")
    ("file,f", po::value<string>(), "Scheme file to evaluate")
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
        cout << PACKAGE_STRING << endl;
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

        // TODO: if there's an unknoown option or command given, like scribbu -x,
        // or scribbu foo, we'll end up here. Need to handle this.

        // 'opts' is non-empty, so there's at least one element--
        // attempt to resolve the first element to a command...
        handler_type f = get_sub_command(opts.front());
        // pop it...
        opts.erase(opts.begin());
        // and dispatch.
        status = f(opts, help, cfg);

      } else {

        if (help_level::verbose == help) {
          print_usage(cout, gopts, USAGE);
        }
        else if (help_level::regular == help) {
          print_usage(cout, global, USAGE);
        }
        else {

          // No sub-command has been given, so we're going to be evaluating
          // some Scheme.
          if (vm.count("expression")) {
            string exp = vm["expression"].as<string>();
            void *p = scm_with_guile(evaluate_expression, &exp);
            status = p != 0;
          } 
          else if (vm.count("file")) {
            fs::path pth(vm["file"].as<string>());
            void *p = scm_with_guile(evaluate_file, &pth);
            status = p != 0;
          }
          else {
            scm_with_guile (&register_functions, NULL);
            scm_shell(argc, argv);
          }

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
