#include "config.h"

#include <signal.h>
#include <sys/resource.h>

#include <iostream>

#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>

#include <libguile.h>

#include "command-utilities.hh"

#include <scribbu/scribbu.hh>
#include <scribbu/scheme.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {

  /**
   * \brief Evaluate a Scheme expression
   *
   * 
   * \param p [in] Address of a std::string containing the Scheme expression
   * to be evaluated
   *
   * \return This method always returns nil (on which more below)
   *
   *
   * This function will unpack the std::string & invoke scm_c_eval_string. That
   * method will return a SCM containing the result of the expression, which this
   * function ignores.
   *
   * It also accepts the default Guile error handling; cf. `scm_c_catch' to change
   * that (https://www.gnu.org/software/guile/manual/html_node/Catching-Exceptions.html#Catching-Exceptions)
   *
   *
   */

  void* evaluate_expression(void *p) {
    initialize_guile(0);
    std::string s = *reinterpret_cast<std::string*>(p);
    scm_c_eval_string(s.c_str());
    return 0;
  }

  /**
   * \brief Evaluate a Scheme file
   *
   * 
   * \param p [in] Address of an fs::path containing the Scheme file to be
   * evaluated
   *
   * \return This method always returns nil (on which more below)
   *
   *
   * This function will unpack the fs::path, read the file & invoke
   * scm_c_eval_string. That method will return a SCM containing the result of
   * the expression, which this function ignores.
   *
   * It also accepts the default Guile error handling; cf. `scm_c_catch' to change
   * that (https://www.gnu.org/software/guile/manual/html_node/Catching-Exceptions.html#Catching-Exceptions)
   *
   *
   */

  void* evaluate_file(void *p) {
    initialize_guile(0);
    fs::path pth = *reinterpret_cast<fs::path*>(p);

    boost::uintmax_t cb = fs::file_size(pth);
    std::unique_ptr<char[]> pb(new char[cb + 1]);

    fs::ifstream ifs(pth);
    ifs.read(pb.get(), cb);
    pb[cb] = 0;

    scm_c_eval_string(pb.get());
    return 0;
  }

  const std::string USAGE(R"(scribbu -- the extensible tool for tagging your music collection

Usage:
    scribbu [OPTION...] [SUB-COMMAND]

For detailed help, say `scribbu --help'. To see the scribbu manual, say `info scribbu'.
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
     "path (absolute or relative) to the config file from which additional"
     " options will be read")
    ("expression,e", po::value<string>(), "Scheme expression to evaluate")
    ("file,f", po::value<string>(), "Scheme file to evaluate")
    ("h", "print the " PACKAGE " usage message & exit with status zero")
    ("help", "show more detailed help")
    ("version,v", "print the " PACKAGE " version & exit with status zero");

  po::options_description hidden;
  hidden.add_options()
    ("sub-command", po::value<vector<string>>());

  try {

    po::options_description allopts;
    allopts.add(gopts).add(hidden);

    // In order to have boost accept later positional parameters, it
    // seems we need to stuff them all one option (as opposed to
    // making position 1 the command and all the rest something else).
    po::positional_options_description popts;
    popts.add("sub-command", -1);

    po::parsed_options parsed = po::command_line_parser(argc, argv).
      options(allopts).
      // Workaround for
      // https://stackoverflow.com/questions/3621181/short-options-only-in-boostprogram-options
      style(po::command_line_style::default_style |
            po::command_line_style::allow_long_disguise).
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
      if (vm.count("help")) {
        help = help_level::verbose;
      } else if (vm.count("h")) {
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

      if (opts.size() && has_sub_command(opts.front())) {

        // 'opts' is non-empty, so there's at least one element--
        // attempt to resolve the first element to a command...
        handler_type f = get_sub_command(opts.front());
        // pop it...
        opts.erase(opts.begin());
        // and dispatch.
        status = f(opts, help, cfg);

      } else {

        if (help_level::verbose == help) {
          // TODO(sp1ff): Might be nice to support other man page readeres
          // (woman, e.g.)
          execlp("man", "man", "scribbu", (char *)NULL);
          throw runtime_error("failed to exec"); // TODO(sp1ff): grab errno
        }
        else if (help_level::regular == help) {
          print_usage(cout, gopts, USAGE);
        }
        else {

          // No sub-command has been given, so we're going to be evaluating
          // some Scheme. Will it be an expression...
          if (vm.count("expression")) {
            // it is:
            string exp = vm["expression"].as<string>();
            status = (intptr_t) scm_with_guile(evaluate_expression, &exp);
          } 
          else if (vm.count("file")) { // Will it be a file?
            // It is:
            fs::path pth(vm["file"].as<string>());
            void *p = scm_with_guile(evaluate_file, &pth);
            status = p != 0;
          }
          else {
            // Otherwise, startup the interprester.
            scm_with_guile(&initialize_guile, NULL);
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
