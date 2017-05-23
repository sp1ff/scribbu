#include "config.h"

#include <iomanip>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>

#include "command-utilities.hh"

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/csv-pprinter.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;


const std::string USAGE(R"(scribbu report -- generate a report

scribbu report [option...] file-or-directory [file-or-directory...]

Generate a report on one or more files. The idea is to have scribbu generate
the data & export it to some other format more convenient for querying &
reporting.

Only CSV output is currently supported.)");


////////////////////////////////////////////////////////////////////////////////
struct reporter {

  virtual
  void
  make_entry(const scribbu::file_info &fi,
             const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
             const scribbu::track_data &info,
             const std::unique_ptr<scribbu::id3v1_tag> &pid3v1) = 0;

};

class csv_reporter: public reporter {

public:

  csv_reporter(const fs::path &output,
               std::size_t ncomm,
               scribbu::encoding v1enc);

  virtual void make_entry(const scribbu::file_info &fi,
                          const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
                          const scribbu::track_data &info,
                          const std::unique_ptr<scribbu::id3v1_tag> &pid3v1);

private:

  static const std::string COMMA;

  fs::ofstream ofs_;
  std::size_t ncomm_;
  scribbu::encoding v1enc_;

};

/*static*/ const std::string csv_reporter::COMMA(",");

csv_reporter::csv_reporter(const fs::path &output,
                           std::size_t ncomm,
                           scribbu::encoding v1enc):
  ofs_(output), ncomm_(ncomm), v1enc_(v1enc)
{
  using namespace std;
  using namespace scribbu;

  ofs_ << "ID3v2 version"             << COMMA <<
          "ID3v2 revision"            << COMMA <<
          "ID3v2 size(bytes)"         << COMMA <<
          "ID3v2 flags"               << COMMA <<
          "ID3v2 unsync"              << COMMA <<
          "ID3v2 Artist"              << COMMA <<
          "ID3v2 Title"               << COMMA <<
          "ID3v2 Album"               << COMMA <<
          "ID3v2 Content Type"        << COMMA <<
          "ID3v2 Encoded By"          << COMMA <<
          "ID3v2 Year"                << COMMA <<
          "ID3v2 Langauges"           << COMMA <<
          "# ID3v2 play count frames" << COMMA <<
          "Play Count"                << COMMA <<
          "# ID3v2 comment frames";

  for (std::size_t i = 0; i < ncomm_; ++i) {
    ofs_ << COMMA << "comment #" << i << " text";
  }

  ofs_ <<                         COMMA <<
          "size (bytes)"       << COMMA << 
          "MD5"                << COMMA <<
          "has ID3v1.1"        << COMMA <<
          "has ID3v1 extended" << COMMA <<
          "ID3v1 Artist"       << COMMA <<
          "ID3v1 Title"        << COMMA <<
          "ID3v1 Album"        << COMMA <<
          "ID3v1 Year"         << COMMA <<
          "ID3v1 Comment"      << COMMA <<
          "ID3v1 Genrre"       << COMMA << endl;

  ofs_ << print_as_csv(ncomm_, v1enc_);
}

/*virtual*/
void
csv_reporter::make_entry(const scribbu::file_info                  &fi,
                         const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
                         const scribbu::track_data                 &info,
                         const std::unique_ptr<scribbu::id3v1_tag> &pid3v1)
{
  using namespace std;
  using namespace scribbu;

  if (pid3v2) {
    ofs_ << *pid3v2;
  }
  else {
    ofs_ << ",,,,,,,,,,,,,,,";
    ostream_iterator<char> oi(ofs_);
    fill_n(oi, ncomm_, ',');
  }

  ofs_ << info;

  if (pid3v1) {
    ofs_ << *pid3v1;
  }
  else {
    ofs_ << ",,,,,,,,";
  }

  ofs_ << endl;
}


////////////////////////////////////////////////////////////////////////////////

class reporting_strategy: public std::unary_function<void, fs::path> {

public:

  void operator()(const fs::path &pth);

protected:

  virtual void process_file(const fs::path &pth) = 0;
  virtual void process_directory(const fs::path &pth) = 0;

};

void reporting_strategy::operator()(const fs::path &pth) {

  if (! fs::exists(pth)) {
    throw std::invalid_argument(pth.string() + " does not appear to exist.");
  }

  if (fs::is_directory(pth)) {
    process_directory(pth);
  } else {
    process_file(pth);
  }

}

class sequential_strategy: public reporting_strategy {

public:

  sequential_strategy(const std::shared_ptr<reporter> &pr):
    _pr(pr)
  { }

protected:

  virtual void process_file(const fs::path &pth);
  virtual void process_directory(const fs::path &pth);

private:

  static const boost::regex REGEX;

  std::shared_ptr<reporter> _pr;

};

/*static*/ const boost::regex sequential_strategy::REGEX(".*\\.mp3");

void sequential_strategy::process_file(const fs::path &pth) {

  // Open 'pth' & collect externally-observable information about the
  // file while we're at it...
  std::pair<std::unique_ptr<std::istream>, scribbu::file_info> pr = scribbu::open_file(pth);

  std::istream &is = * pr.first.get();

  // and use the open istream to read the...
  std::unique_ptr<scribbu::id3v2_tag> pid3v2 = scribbu::maybe_read_id3v2(is); // ID3v2 tags...
  scribbu::track_data ti(is);                                                 // the track itself...
  std::unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(is);    // and the ID3v1 tag.

  // That's it-- pass whatever data we have to the reporter.
  _pr->make_entry(pr.second, pid3v2, ti, pid3v1);

}

/// Recursively process a directory sequentially
void sequential_strategy::process_directory(const fs::path &pth) {

  std::for_each(fs::recursive_directory_iterator(pth),
                fs::recursive_directory_iterator(),
                [&](const fs::directory_entry &p) {
                  // the iterators dereference to a directory_entry
                  fs::path pth = p.path();
                  // Only process files ending in .mp3
                  if (fs::is_regular_file(pth) && boost::regex_match(pth.string(), REGEX)) {
                    process_file(pth);
                  }
                });

}


////////////////////////////////////////////////////////////////////////////////
//                             handler                                        //
////////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_report(const std::vector<std::string>  &tokens,
                help_level                       help,
                const boost::optional<fs::path> &cfg)
  {
    using namespace std;

    using scribbu::encoding;

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
    // None at this time...

    po::options_description xclopts("command-line only developer options");
    // None at this time...

    po::options_description opts("general options");
    opts.add_options()
      ("num-comments,c", po::value<size_t>()->default_value(6),
       "Number of comment tags to be printed (their # will always be reported")
      ("output,o", po::value<fs::path>()->required(), ".csv file to which the "
       "results will be written")
      ("v1-encoding,1", po::value<encoding>()->
       default_value(encoding::CP1252), "Encoding to be used for text "
       "in ID3v1 tags.");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
       "files or directories to be examined; if a directory is given, it "
       "will be searched recursively");

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
        if (cfg) {
          fs::ifstream ifs(cfg.get());
          parsed = po::parse_config_file(ifs, nocli);
          po::store(parsed, vm);
        }

        // and finally any environment variables.
        parsed = po::parse_environment(nocli, "SCRIBBU");
        po::store(parsed, vm);

        po::notify(vm);

        // That's it-- the list of files and/or directories to be processed
        // should be waiting for us in 'arguments'...

        // Work around to https://svn.boost.org/trac/boost/ticket/8535
        std::vector<fs::path> arguments;
        for (auto s: vm["arguments"].as<std::vector<string>>()) {
          arguments.push_back(fs::path(s));
        }

        // Multi-threaded:

        // - whip up a thread-safe reporter

        // - whip up a queue

        // - whip up a thread pool, giving it a reference to the queue & the reporter

        // - whip up a crawler, giving it a reference to that thread
        //   pool; the crawler will handle each file by pushing it onto
        //   the queue (from the main thread)-- the threads will pull
        //   items off the queue, process them, and send results to the
        //   reporter

        // - when the crawler has finished, it sets a flag & joins all
        //   the threads in the thread pool; when that flag is set, and
        //   a thread can pull no more items off the queue, it
        //   terminates

        // Single-threaded

        // - whip up a non-thread-safe reporter

        // - whip up a crawler, giving it a reference to the reporter;

        // - the crawler will handle each file by processing it &
        //   sending the results to the reporter

        // TOOD: Implement the multi-threaded option

        size_t ncomm = vm["num-comments"].as<size_t>();
        fs::path out = vm["output"].as<fs::path>();
        encoding v1enc = vm["v1-encoding"].as<encoding>();

        std::shared_ptr<reporter> pr(new csv_reporter(out, ncomm, v1enc));

        std::unique_ptr<reporting_strategy> ps(new sequential_strategy(pr));

        std::for_each(arguments.begin(), arguments.end(), std::ref(*ps));

      }

    } catch (const po::error &ex) {
      cerr << ex.what() << endl;
      print_usage(cerr, docopts, USAGE);
      status = EXIT_INCORRECT_USAGE;
    } catch (const std::exception &ex) {
      cerr << ex.what() << endl;
      status = EXIT_FAILURE;
    }

    return status;

  }

  register_command r("report", handle_report);

}

  
