/**
 * \file scribbu.cc
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#include "config.h"
#include "command-utilities.hh"

#include <scribbu/scribbu.hh>
#include <scribbu/scheme.hh>

#include <libguile.h>
#include <signal.h>
#include <sys/resource.h>

#include <iostream>

#include <fstream>
#include <boost/lexical_cast.hpp>
#include <boost/program_options.hpp>


namespace fs = std::filesystem;
namespace po = boost::program_options;


namespace {

  const std::string USAGE(R"( -- the extensible tool for tagging your music collection

Usage:

    scribbu [GLOBAL-OPTION...] [SUB-COMMAND] [OPTION...] [ARGUMENT...]
    scribbu [GLOBAL-OPTION...] ([-s] FILE|-c EXPR|--) [ARGUMENT...]

The first form invokes one of the scribbu sub-commands (type `scribbu --help'
for a list). The second invokes the Guile Scheme interpreter embedded within
scribbu.

GLOBAL-OPTION may be:

    -h                    display this help message and exit with status zero
    --help                display the scribbu man page
    -v, --version         display this package's version information and exit
                          with status zero
    -L DIRECTORY          add DIRECTORY to the front of the Guile module load
                          path
    -C DIRECTORY          like -L, but for compiled files
    -x EXTENSION          add EXTENSION to the front of the load extensions
    -l FILE               load source code from FILE
    -e FUNCTION           after reading script, apply FUNCTION to
                          command line arguments
    -ds                   do -s script at this point
    --debug               start with the "debugging" VM engine
    --no-debug            start with the normal VM engine (backtraces but
                          no breakpoints); default is --debug for interactive
                          use, but not for `-s' and `-c'.
    --auto-compile        compile source files automatically
    --fresh-auto-compile  invalidate auto-compilation cache
    --no-auto-compile     disable automatic source file compilation;
                          default is to enable auto-compilation of source
                          files.
    --listen[=P]          listen on a local port or a path for REPL clients;
                          if P is not given, the default is local port 37146
    -q                    inhibit loading of user init file
    --use-srfi=LS         load SRFI modules for the SRFIs in LS,
                          which is a list of numbers like "2,13,14"
    \                     read arguments from following script lines

The following switches stop argument processing, and pass all remaining
arguments as the value of `(command-line)'. If FILE begins with `-' the
-s switch is mandatory.

     [-s] FILE      load source code from FILE, and exit
     -c EXPR        evalute expression EXPR, and exit
     --             stop scanning arguments; run interactively

For detailed help, say `scribbu --help'. To see the scribbu manual, say `info scribbu'.
)");

  /**
   * \brief Parse options until we encounter something we don't understand; copy
   * Guile options & return any scribbu-specific options; consume argc & argv
   *
   *
   * \param argc [in,out] address of argc
   *
   * \param argv [in,out] address of argv
   *
   * \param gargc [out] address of an integer variable counting Guile argumetns;
   * on return it will contain the numbr of arguments added to \a gargv
   *
   * \param gargv [out] address of an array of const char *-- Guile options
   * will be copied from argv to here
   *
   * \return a bool, help_level pair; the bool indicates whether version
   * information was requested
   *
   *
   * This function will consume up to the first un-recognized option or
   * argument. The caller can determine whether all arguments were consumed by
   * inspecting *argc on return. Encountering -s, -c or -- will cause all
   * remaining options to beconsumed & copied over to \a gargv.
   *
   *
   */

  std::tuple<bool, help_level, boost::optional<verbose_flavor>>
  handle_options(int *argc, char ***argv,
                 int *gargc, char **gargv)
  {
    using boost::optional;

    int n = 0;
    bool version = false;
    bool copy_rest = false;
    help_level help = help_level::none;
    optional<verbose_flavor> flav = boost::none;

    while (*argc > 0) {

      const char *cmd = (*argv)[0];

      if (!strcmp("-h", cmd)) {

        help = help_level::regular; // short help
        (*argv)++; (*argc)--;       // consume `argv'

      } else if (!strcmp("--help", cmd) || !strcmp("--man", cmd)) {

        help = help_level::verbose; // verbose help
        flav = verbose_flavor::man;
        (*argv)++; (*argc)--;       // consume `argv'

      } else if (!strcmp("--info", cmd)) {

        help = help_level::verbose; // verbose help
        flav = verbose_flavor::info;
        (*argv)++; (*argc)--;       // consume `argv'

      } else if (!strcmp("-v", cmd) || !strcmp("--version", cmd)) {

        version = true;       // version requested
        (*argv)++; (*argc)--; // consume `argv'

      } else if (!strcmp("-L", cmd) ||
                 !strcmp("-C", cmd) ||
                 !strcmp("-x", cmd) ||
                 !strcmp("-l", cmd) ||
                 !strcmp("-e", cmd)) {

        // all (short) Guile options accepting a second argument
        gargv[n++] = (*argv)[0];          // copy to `gargv'
        (*gargc)++; (*argv)++; (*argc)--; // consume `argv'

        // There *should* be an option value waiting for us
        if (0 == *argc) {
          throw po::required_option(cmd);
        } else {
          gargv[n++] = (*argv)[0];          // copy to `gargv'
          (*gargc)++; (*argv)++; (*argc)--; // consume `argv'
        }

      } else if (!strncmp("--language", cmd, 10)      ||
                 !strncmp("--listen", cmd, 8)         ||
                 !strncmp("--use-srfi", cmd, 10)      ||
                 !strcmp("-ds", cmd)                  ||
                 !strcmp("--debug", cmd)              ||
                 !strcmp("--no-debug", cmd)           ||
                 !strcmp("--auto-compile", cmd)       ||
                 !strcmp("--fresh-auto-compile", cmd) ||
                 !strcmp("--no-auto-compile", cmd)    ||
                 !strcmp("-q", cmd)                   ||
                 !strcmp("\\", cmd)) {

        // Guile options that either take no values, or that are
        // long options that take their values in a single argument
        // (i.e. "--foo=bar")
        gargv[n++] = (*argv)[0];          // copy to `gargv'
        (*gargc)++; (*argv)++; (*argc)--; // consume `argv'

      } else if (!strcmp("-c", cmd) || !strcmp("-s", cmd)) {

        gargv[n++] = (*argv)[0];          // copy to `gargv'
        (*gargc)++; (*argv)++; (*argc)--; // consume `argv'

        // There *should* be an option value waiting for us:
        if (*argc == 0) {
          throw po::required_option(cmd);
        } else {
          copy_rest = true;
          break;
        }

      } else if (!strcmp("--", cmd)) {

        // Everything else goes to `gargv'
        copy_rest = true;
        break;

      } else {

        // If we're here, we've encountered something we don't understand-- bail
        break;

      }

    } // End while on *argc.

    if (copy_rest) {

      while (*argc) {
        gargv[n++] = (*argv)[0];          // copy to `gargv'
        (*gargc)++; (*argv)++; (*argc)--; // consume `argv'
      }

    }

    return std::make_tuple(version, help, flav);
  }

  void
  print_usage(std::ostream      &os,
              const std::string &usage,
              const std::string  pname)
  {
    using namespace std;

    os << pname << usage << "\n" << std::endl;

    vector<string> sub_cmd_names;
    get_sub_command_names(back_inserter(sub_cmd_names));

    os << "scribbu sub-commands (run `scribbu <sub-command> -h' for a " <<
      "usage message):\n\n    ";
    ostream_iterator<string> out(cout, "\n    ");
    copy(sub_cmd_names.begin(), sub_cmd_names.end(), out);
    os << endl;
  }

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

/// The Big Kahuna
int
main(int argc, char * argv[])
{
  using namespace std;

  using boost::lexical_cast;
  using boost::optional;

  // Setup an alternate stack, so our signal handler will still work in the
  // case of stack overflow (which will cause a SIGSEGV)
  stack_t ss = { 0 };
  ss.ss_size = SIGSTKSZ;
  ss.ss_sp = malloc(SIGSTKSZ);
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

  // Next, setup the signal handler...
  struct sigaction sigact = { 0 }, oldact = { 0 };
  sigact.sa_handler = sig_handler;
  sigact.sa_flags = SA_ONSTACK;
  if (0 != sigaction (SIGSEGV, &sigact, &oldact)) {
    cerr << "Failed to set signal handler." << endl;
    return 3;
  }

  //  carry out scribbu library initialization...
  scribbu::static_initialize();

  // and, finally, parse the command line:

  //////////////////////////////////////////////////////////////////////////
  //                            command line parsing                      //
  //////////////////////////////////////////////////////////////////////////

  // This task is complicated by the multiple ways in which scribbu can be
  // invoked:

  //     1. scribbu (-h|--help|--version) : just display the requested
  //        information & exit
  //     2. scribbu [GLOBAL-OPTION...] SUBCMD [OPTION...] [ARGUMENT...]: parse
  //        any global options, then dispatch to the sub-command for further
  //        option parsing-- do _not_ parse any options or arguments for
  //        the sub-command.
  //     3. scribbu [(GLOBAL-OPTION|GUILE-OPTION)...] [-s FILE|-e EXPR|--]
  //        [ARGUMENT...]: parse the global options and the Guile options;
  //        handle the global options & pass all the Guile options to
  //        `scm_shell' for processing there

  // Constraints:
  //
  //     1. I can't just pass on all unknown options to Guile; if I do that,
  //     someone typing, say, `scribbu --hepl' will get the Guile usage message,
  //     which will be very confusing to them.
  //
  //     2. Whatever option parsing framework I use, I need to _stop_ parsing at
  //     the first non-global: suppose a sub-command also defines an option with
  //     the same name as a global? There's no way to do that in a general way.

  // boost::program_options really isn't suited for this. For instance, it's
  // inconvenient to distinguish between `-h' (for which I want to directly
  // print a short usage message on stdout) and `--help' (for which I want to
  // display the man page). You can allow unrecognized parameters, but you can't
  // get it to stop parsing on an un-recognized token, so it _will_ parse
  // sub-command options by the same name as global options. In casting about, I
  // found some Stack Overflow answers that involved using undocumented
  // features, but it seemed to me like hammering a square peg into a round
  // hole.

  // I thought about getopt-long, but then perused the Git source
  // (https://github.com/git/git/blob/master/git.c) and just doing it "by hand"
  // didn't look so bad...

  int status = EXIT_SUCCESS;

  string program_name(argv[0]);

  try {

    typedef char *argv_t;
    unique_ptr<argv_t[]> gargv(new argv_t[argc + 1]);

    int gargc = 1;
    gargv[0] = argv[0];

    argv++; argc--;

    bool version;
    help_level help;
    optional<verbose_flavor> flav;
    std::tie(version, help, flav) = handle_options(&argc, &argv, &gargc, &(gargv[1]));

    // At this point, we have a few possibilities. Our our caller could be
    // asking for help/version information; version "wins":
    if (version) {

      cout << PACKAGE_STRING << endl;

    } else if (help_level::verbose == help) {

      if (verbose_flavor::man == flav) {
        show_man_page("scribbu");
      } else {
        show_info_node("scribbu");
      }

    }
    else if (help_level::regular == help) {

      print_usage(cout, USAGE, program_name);

    } else if (argc > 0 && has_sub_command(argv[0])) {

      // If there's something remaining un-processed on the command line, it
      // could be a sub-command (presumably with more options & arguments behind
      // it.
      handler_type f = get_sub_command(argv[0]);
      argv++; argc--;
      status = f(argc, argv);

    } else {

      // Otherwise, _if_ there is still something waiting for us on `argv',
      // it must be a file to be handed to the Scheme interpreter, or it's
      // just garbage. Impossible to tell from here, of course, but I'm going
      // to say that if the argument begins with a '-', it is a mis-typed
      // option.
      if (argc) {

        if ('-' == argv[0][0]) {

          throw po::unknown_option(argv[0]);

        } else {

          // Consume everything-- there may be options & arguments for the
          // script as well.
          while (argc) {
            gargv[gargc++] = argv[0]; // copy to `gargv'
            argv++; argc--;           // consume `argv'
          }

        }

      } // End if on `argc'.

      // Per C11 5.1.2.2.1 Program startup:

      //     "If they are declared, the parameters to the main function shall
      //     obey the following constraints: The value of argc shall be
      //     nonnegative. argv[argc] shall be a null pointer."
      gargv[gargc] = 0;

      init_guile initg = { DATADIR };
      scm_with_guile(&initialize_guile, &initg);
      scm_shell(gargc, gargv.get());

    } // End if on options.

  } catch (const po::error &ex) {

    cerr << ex.what() << endl;
    print_usage(cerr, USAGE, program_name);
    status = EXIT_INCORRECT_USAGE;

  } catch (const std::exception &ex) {

    cerr << ex.what() << endl;
    status = EXIT_FAILURE;

  }

  scribbu::static_cleanup();

  return status;

} // End main.
