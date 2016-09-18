#include "config.h"
#include "find.hh"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>

#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/program-support.hh>
#include <scribbu/program-parser.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE("scribbu find -- find .mp3 files according to tag contents\n"
"\n"
"Usage: scribbu find [option...] file-or-directory [file-or-directory...]\n"
"\n"
"Search a collection of files based on their ID3 tags.\n"
"\n"
"Options:\n"
"\n"
"  -e\n"
"  --expression: scribbu boolean expression; files for which this expression\n"
"                evaluates as true will be printed; either this option or -f\n"
"                must be given\n"
"  -f\n"
"  --file: path to a file containing a scribbu boolean expression; either this\n"
"          option or -e must be given\n"
"  -m\n"
"  --m3u: Specify a filename to which the results shall be written (in M3U format);\n"
"         by default the results are written to stdout\n"
"  -r\n"
"  --regex: Optional regular expression for filtering files; only files matching\n"
"           this regex will be tested\n"
"\n"
"Any directories specified as arguments will be searched recursively.\n");


///////////////////////////////////////////////////////////////////////////////
//                      classes private to this module                       //
///////////////////////////////////////////////////////////////////////////////

namespace {

  class matcher: public std::unary_function<void, fs::path>
  {
  public:
    matcher(const std::string &expression,
            const boost::regex &file_regex,
            const fs::path &m3u);
    void operator()(const fs::path &pth);

  private:
    boost::regex file_regex_;
    scribbu::program::condition *pC_;

  }; // End class matcher.

  matcher::matcher(const std::string &expression,
                   const boost::regex &file_regex,
                   const fs::path &m3u):
    file_regex_(file_regex)
  {
    scribbu::program::start_symbol S;
    if (0 != programparse(&S)) {
      // TODO: Throw custom exception
      throw std::runtime_error("Parse failure");
    }

    if (!S.fcondition_) {
      // TODO: Throw custom exception
      throw std::runtime_error("Expected a condition, not a program");
    }

    pC_ = S.pcondition_;

  }

  void matcher::operator()(const fs::path &pth)
  {
    using namespace std;
    using namespace boost;

    if (!file_regex_.empty() && !regex_match(pth.string(), file_regex_)) {
      return;
    }

    fs::ifstream ifs(pth, std::ios_base::binary);
    vector<unique_ptr<scribbu::id3v2_tag>> id3v2;
    scribbu::read_all_id3v2(ifs, back_inserter(id3v2));


    for (size_t i = 0, n = id3v2.size(); i < n; ++i) {

      // TODO: Write me!


    }
  }

} // End this xlation unit's un-named namespace.


///////////////////////////////////////////////////////////////////////////////
//                 free functions exported from this module                  //
///////////////////////////////////////////////////////////////////////////////

int
handle_find(const std::vector<std::string> &tokens,
            help_level                      help)
{
  using namespace std;

  int status = EXIT_SUCCESS;

  /////////////////////////////////////////////////////////////////////////////
  //                                                                         //
  //                       C O M M A N D   O P T I O N S                     //
  //                                                                         //
  // Let's divide the options in two ways:                                   //
  //                                                                         //
  // - public versus developer-only options                                  //
  // - options permissible only on the command line versus options           //
  //   permissible on the command line, configuration file, and the          //
  //   environment                                                           //
  //                                                                         //
  //                            public   private                             //
  //                          +--------+---------+                           //
  //                cli-only  | clopts | xclopts |                           //
  //                          +--------+---------+                           //
  //                cli, cfg, |  opts  |  xopts  |                           //
  //                & env     +--------+---------+                           //
  //                                                                         //
  /////////////////////////////////////////////////////////////////////////////

  po::options_description clopts("command-line only options");
  clopts.add_options()
    ("config,c", po::value<fs::path>()->default_value(fs::path("~/.scribbu")),
     "path (absolute or relative) to the config file")
    ("help,h", "display the " PACKAGE " usage message & exit with status "
     "zero");

  po::options_description xclopts("command-line only developer options");
  xclopts.add_options()
    ("man", "display the " PACKAGE " usage message including developer-"
     "only options & exit with status zero");

  po::options_description opts("general options");
  opts.add_options()
    ("m3u,m", po::value<fs::path>(), "If specified, names an output file (in"
     "M3U format) to which matches shall be written")
    ("regex,r", po::value<string>(), "If specified, defines a regular "
     "expression filtering files (i.e. only filenames matching the regular "
     "expression will be considered)")
    // Work around to https://svn.boost.org/trac/boost/ticket/8535
    ("arguments", po::value<vector<string>>()->required(), "one or more "
     "files or directories to be examined; if a directory is given, it "
     "will be searched recursively");

  po::options_description xopts("hidden options");

  po::options_description docopts;
  docopts.add(clopts).add(opts);

  po::options_description nocli;
  nocli.add(opts).add(xopts);

  po::options_description all;
  all.add(clopts).add(xclopts).add(opts).add(xopts);

  po::positional_options_description popts;
  popts.add("arguments", -1);

  try {

    if (help_level::regular == help) {
      print_usage(cout, docopts, USAGE);
    } else if (help_level::verbose == help) {
      print_usage(cout, all, USAGE);
    } else {

      po::variables_map vm;

      // Command line takes highest priority...
      po::parsed_options parsed = po::command_line_parser(tokens).
        options(all).
        positional(popts).
        run();

      po::store(parsed, vm);

      // followed by the configuration file...
      fs::path cfg = vm["config"].as<fs::path>();
      if (fs::exists(cfg)) {
        fs::ifstream ifs(cfg);
        parsed = po::parse_config_file(ifs, nocli);
        po::store(parsed, vm);
      } else if (!vm["config"].defaulted()) {
        throw po::validation_error(po::validation_error::invalid_option_value,
                                   cfg.string() + " does not exist");
      }

      // and finally any environment variables.
      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      po::notify(vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...

      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      vector<fs::path> arguments;
      for (auto s: vm["arguments"].as<vector<string>>()) {
        arguments.push_back(fs::path(s));
      }

      string expression;
      if (vm.count("expression")) {
        expression = vm["expression"].as<string>();
      } else if (vm.count("program")) {
        fs::path pth = vm["program"].as<string>();
        fs::ifstream ifs(pth);
        ifs.seekg(0, ios_base::end);
        streampos cb = ifs.tellg();
        ifs.seekg(0, ios_base::beg);
        unique_ptr<char []> pbuf(new char[cb]);
        ifs.read(pbuf.get(), cb);
        expression.assign(pbuf.get(), cb);
      } else {
        throw po::validation_error(po::validation_error::at_least_one_value_required,
                                   "At least one of -e or -f must be given");
      }

      boost::regex file_regex;
      if (vm.count("regex")) {
        file_regex = boost::regex(vm["regex"].as<string>());
      }

      fs::path m3u;
      if (vm.count("m3u")) {
        m3u = vm["m3u"].as<fs::path>();
      }

      matcher M(expression, file_regex, m3u);
      for (auto x: arguments) {
        for_each(fs::recursive_directory_iterator(x),
                      fs::recursive_directory_iterator(),
                      ref(M));
      }

    } // End if on help.

  } catch (const po::error &ex) {
    cerr << ex.what() << endl;
    print_usage(cerr, docopts, USAGE);
    status = EXIT_INCORRECT_USAGE;
  } catch (const exception &ex) {
    cerr << ex.what() << endl;
    status = EXIT_FAILURE;
  }

  return status;

} // End free function handle_find.
