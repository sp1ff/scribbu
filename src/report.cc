/**
 * \file report.cc
 *
 * Copyright (C) 2015-2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/csv-pprinter.hh>
#include <scribbu/tdf-pprinter.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>

#include <iomanip>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>


namespace fs = boost::filesystem;
namespace po = boost::program_options;


const std::string USAGE(R"(scribbu report -- generate a report on one or more
ID3-tagged files

scribbu report [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

Where OPTION is:

    -c, --num-comments=INT: maximum # of comments to report
    -o, --output=FILE:      output file name
    -t, --tsv-format:       select tab-separated values as the output format
    -1, --v1-encoding=ENC:  character encoding to be assumed for ID3v1 fields
    -a, --ascii-delimited:  used with TDF (tab-delimited format) to produce
                            ASCII-delimited text (ASCII 0x31 instead of TABs)

Generate a report on one or more ID3-tagged files. Only comma-separated value
or tab-separated value formats are supported, on the assumption that scribbu's
output will be exported to some other tool more convenient for querying &
reporting.

For detailed help, say `scribbu report --help'. To see the manual, say
`info "scribbu (report) "'.
)");

////////////////////////////////////////////////////////////////////////////////
//                            A thing that reports                            //
////////////////////////////////////////////////////////////////////////////////

struct reporter {
  virtual void
  make_entry(const scribbu::file_info                  &fi,
             const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
             const scribbu::track_data                 &info,
             const std::unique_ptr<scribbu::id3v1_tag> &pid3v1) = 0;
  virtual ~reporter()
  { }
};

////////////////////////////////////////////////////////////////////////////////
//                          reporting in CSV format                           //
////////////////////////////////////////////////////////////////////////////////

/// A thing that reports in CSV format
class csv_reporter: public reporter {

public:
  csv_reporter(const fs::path   &output,
               std::size_t       ncomm,
               scribbu::encoding v1enc,
               bool              no_dir);
  virtual void make_entry(const scribbu::file_info                  &fi,
                          const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
                          const scribbu::track_data                 &info,
                          const std::unique_ptr<scribbu::id3v1_tag> &pid3v1);

private:
  static const std::string COMMA;
  fs::ofstream      ofs_;
  std::size_t       ncomm_;
  scribbu::encoding v1enc_;
  bool              no_dir_;

};

/*static*/ const std::string csv_reporter::COMMA(",");

csv_reporter::csv_reporter(const fs::path   &output,
                           std::size_t       ncomm,
                           scribbu::encoding v1enc,
                           bool              no_dir):
  ofs_(output), ncomm_(ncomm), v1enc_(v1enc), no_dir_(no_dir)
{
  using namespace std;
  using namespace scribbu;

  ////////////////////////////////////////////////////////////////////////////////
  //                                 header row                                 //
  ////////////////////////////////////////////////////////////////////////////////

  if (!no_dir_) {
    ofs_ << "directory" << COMMA;
  }

  ofs_ << "file"                      << COMMA <<
          "file size(MB)"             << COMMA <<
          "ID3v2 version"             << COMMA <<
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
          "ID3v1 Genrre"       << endl;

  // Imbue our output stream with a manipulator that will cause all subsequently
  // inserted tags &c to be printed in CSV format.
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

  const double ONE_MEG = 1048576;

  if (!no_dir_) {
    ofs_ << fi.parent() << COMMA;
  }

  double mb = double(fi.size()) / ONE_MEG;

  ofs_ << fi.filename() << COMMA << setprecision(3) << fixed << mb << COMMA;

  if (pid3v2) {
    ofs_ << *pid3v2;
  }
  else {
    ofs_ << ",,,,,,,,,,,,,,";
    ostream_iterator<char> oi(ofs_);
    fill_n(oi, ncomm_, ',');
  }

  ofs_ << "," << info << ",";

  if (pid3v1) {
    ofs_ << *pid3v1;
  }
  else {
    ofs_ << ",,,,,,,";
  }

  ofs_ << endl;
}

////////////////////////////////////////////////////////////////////////////////
//                          reporting in TDF format                           //
////////////////////////////////////////////////////////////////////////////////

/// A thing that reports in TDF format
class tdf_reporter: public reporter {

public:
  tdf_reporter(const fs::path   &output,
               std::size_t       ncomm,
               scribbu::encoding v1enc,
               bool              no_dir,
               bool              ascii);
  virtual void make_entry(const scribbu::file_info                  &fi,
                          const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
                          const scribbu::track_data                 &info,
                          const std::unique_ptr<scribbu::id3v1_tag> &pid3v1);

private:
  fs::ofstream      ofs_;
  std::size_t       ncomm_;
  scribbu::encoding v1enc_;
  bool              no_dir_;
  char              sep_;

};

tdf_reporter::tdf_reporter(const fs::path   &output,
                           std::size_t       ncomm,
                           scribbu::encoding v1enc,
                           bool              no_dir,
                           bool              ascii):
  ofs_(output), ncomm_(ncomm), v1enc_(v1enc), no_dir_(no_dir),
  sep_(ascii ? 0x1f : '\t')
{
  using namespace std;
  using namespace scribbu;

  ////////////////////////////////////////////////////////////////////////////////
  //                                 header row                                 //
  ////////////////////////////////////////////////////////////////////////////////

  if (!no_dir_) {
    ofs_ << "directory" << sep_;
  }

  ofs_ << "file"                      << sep_ <<
          "file size(MB)"             << sep_ <<
          "ID3v2 version"             << sep_ <<
          "ID3v2 revision"            << sep_ <<
          "ID3v2 size(bytes)"         << sep_ <<
          "ID3v2 flags"               << sep_ <<
          "ID3v2 unsync"              << sep_ <<
          "ID3v2 Artist"              << sep_ <<
          "ID3v2 Title"               << sep_ <<
          "ID3v2 Album"               << sep_ <<
          "ID3v2 Content Type"        << sep_ <<
          "ID3v2 Encoded By"          << sep_ <<
          "ID3v2 Year"                << sep_ <<
          "ID3v2 Langauges"           << sep_ <<
          "# ID3v2 play count frames" << sep_ <<
          "Play Count"                << sep_ <<
          "# ID3v2 comment frames";

  for (std::size_t i = 0; i < ncomm_; ++i) {
    ofs_ << sep_ << "comment #" << i << " text";
  }

  ofs_ <<                         sep_ <<
          "size (bytes)"       << sep_ <<
          "MD5"                << sep_ <<
          "has ID3v1.1"        << sep_ <<
          "has ID3v1 extended" << sep_ <<
          "ID3v1 Artist"       << sep_ <<
          "ID3v1 Title"        << sep_ <<
          "ID3v1 Album"        << sep_ <<
          "ID3v1 Year"         << sep_ <<
          "ID3v1 Comment"      << sep_ <<
          "ID3v1 Genre"        << endl;

  // Imbue our output stream with a manipulator that will cause all subsequently
  // inserted tags &c to be printed in CSV format.
  ofs_ << print_as_tdf(ncomm_, v1enc_, boost::none, ascii);
}

/*virtual*/
void
tdf_reporter::make_entry(const scribbu::file_info                  &fi,
                         const std::unique_ptr<scribbu::id3v2_tag> &pid3v2,
                         const scribbu::track_data                 &info,
                         const std::unique_ptr<scribbu::id3v1_tag> &pid3v1)
{
  using namespace std;
  using namespace scribbu;

  const double ONE_MEG = 1048576;

  if (!no_dir_) {
    ofs_ << fi.parent() << sep_;
  }

  double mb = double(fi.size()) / ONE_MEG;

  ofs_ << fi.filename() << sep_ << setprecision(3) << fixed << mb << sep_;

  if (pid3v2) {
    ofs_ << *pid3v2;
  }
  else {
    ostream_iterator<char> oi(ofs_);
    fill_n(oi, 14 + ncomm_, sep_);
  }

  ofs_ << sep_ << info << sep_;

  if (pid3v1) {
    ofs_ << *pid3v1;
  }
  else {
    ostream_iterator<char> oi(ofs_);
    fill_n(oi, 7, sep_);
  }

  ofs_ << endl;
}

////////////////////////////////////////////////////////////////////////////////

/// A thing that knows how to report on a directory tree full of
/// ID3v2-tagged files
class reporting_strategy: public std::unary_function<void, fs::path> {

public:
  /// return the # of files successfully processed & the # of failures; "failure"
  /// refers to the failure to process an existing file; if a given path just
  /// doesn't exist, we throw
  std::pair<std::size_t, std::size_t> operator()(const fs::path &pth);
  virtual ~reporting_strategy()
  { }

protected:
  /// Report on a single file; if done successfully, return true. If there is
  /// a failure in processing the file (invalid tag, bad MP3 frame, &c) return
  /// false. Else, throw.
  virtual bool process_file(const fs::path &pth) = 0;
  /// Report on a directory tree; return the number of files successfully
  /// processed & the number of files that could not be processed due to
  /// invalid data. If \a pth doesn't exist, throw.
  virtual std::pair<std::size_t, std::size_t>
  process_directory(const fs::path &pth) = 0;

};

std::pair<std::size_t, std::size_t>
reporting_strategy::operator()(const fs::path &pth) {

  using namespace std;

  if (! fs::exists(pth)) {
    throw invalid_argument(pth.string() + " does not appear to exist.");
  }

  size_t num_succeeded = 0, num_failed = 0;

  if (fs::is_directory(pth)) {
    size_t s, f;
    tie(s, f) = process_directory(pth);
    num_succeeded += s;
    num_failed += f;
  } else {
    if (process_file(pth)) {
      ++num_succeeded;
    }
    else {
      ++num_failed;
    }
  }

  return make_pair(num_succeeded, num_failed);

}

////////////////////////////////////////////////////////////////////////////////
/// A thing that reports on a directory tree full of ID3v2-tagged
/// files sequentually
////////////////////////////////////////////////////////////////////////////////

class sequential_strategy: public reporting_strategy {

public:
  sequential_strategy(const std::shared_ptr<reporter> &pr): _pr(pr)
  { }

protected:
  virtual bool process_file(const fs::path &pth);
  virtual std::pair<std::size_t, std::size_t>
  process_directory(const fs::path &pth);

private:
  static const boost::regex REGEX;
  std::shared_ptr<reporter> _pr;

};

/*static*/ const boost::regex sequential_strategy::REGEX(".*\\.mp3");

bool
sequential_strategy::process_file(const fs::path &pth) {

  using namespace std;

  try {
    // Open 'pth' & collect externally-observable information about the
    // file while we're at it...
    ifstream is;
    scribbu::file_info fi;
    tie(is, fi) = scribbu::open_file(pth);

    // and use the open istream to read the...
    unique_ptr<scribbu::id3v2_tag> pid3v2 = scribbu::maybe_read_id3v2(is); // ID3v2 tags...
    scribbu::track_data ti(is);                                            // the track itself...
    unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(is);    // and the ID3v1 tag.

    // That's it-- pass whatever data we have to the reporter.
    _pr->make_entry(fi, pid3v2, ti, pid3v1);
  }
  catch (const exception &ex) {
    cerr << pth << ": " << ex.what() << endl;
    return false;
  }

    return true;
}

/// Recursively process a directory sequentially
std::pair<std::size_t, std::size_t>
sequential_strategy::process_directory(const fs::path &pth) {

  std::size_t num_ok = 0, num_err = 0;
  std::for_each(fs::recursive_directory_iterator(pth),
                fs::recursive_directory_iterator(),
                [&](const fs::directory_entry &p) {
                  // the iterators dereference to a directory_entry
                  fs::path pth = p.path();
                  // Only process files ending in .mp3
                  if (fs::is_regular_file(pth) && boost::regex_match(pth.string(), REGEX)) {
                    if (process_file(pth)) {
                      num_ok++;
                    } else {
                      num_err++;
                    }
                  }
                });
  return std::make_pair(num_ok, num_err);

}


////////////////////////////////////////////////////////////////////////////////
//                             handler                                        //
////////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_report(int argc, char **argv)
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
    clopts.add_options()
      ("help,h", po::bool_switch(), "Display help & exit")
      ("info", po::bool_switch(), "Display help in Info format & exit");

    po::options_description xclopts("command-line only developer options");
    xclopts.add_options()
      ("man", po::bool_switch(), "Display the man page & exit");

    po::options_description opts("general options");
    opts.add_options()
      ("num-comments,c", po::value<size_t>()->default_value(6),
       "Number of comment tags to be printed (their # will always be reported")
      ("output,o", po::value<fs::path>()->required(), "output file to which the "
       "results will be written")
      ("tsv,t", po::bool_switch(), "select tab-separated values instead of "
       "comma-separated values")
      ("v1-encoding,1", po::value<encoding>()->
       default_value(encoding::CP1252), "Encoding to be used for text "
       "in ID3v1 tags.")
      ("no-directory,d", po::bool_switch(), "Do not output the directory column")
      ("ascii-delimited,a", po::bool_switch(), "Produce ASCII-delimited text "
       "instead of tab-delimited text (may be used only with --tsv)")
      ("no-summary,S", po::bool_switch(), "Do not print a summary of the "
       "number of files processed on completion.");

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

      vector<string> tokens;
      convert_tokens(argc, argv, back_inserter(tokens));

      po::variables_map vm;
      po::parsed_options parsed = po::command_line_parser(tokens).
        options(all).
        positional(popts).
        run();

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-report",
                        "(scribbu) Invoking scribbu report");

      po::store(parsed, vm);

      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      // Don't enforce required arguments until we know we're not just
      // displaying help.
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

      size_t   ncomm   = vm["num-comments"   ].as<size_t  >();
      fs::path out     = vm["output"         ].as<fs::path>();
      encoding v1enc   = vm["v1-encoding"    ].as<encoding>();
      bool     no_dir  = vm["no-directory"   ].as<bool    >();
      bool     tdf     = vm["tsv"            ].as<bool    >();
      bool     ascii   = vm["ascii-delimited"].as<bool    >();
      bool     no_summ = vm["no-summary"     ].as<bool    >();

      std::shared_ptr<reporter> pr(
        tdf ?
          (reporter*) new tdf_reporter(out, ncomm, v1enc, no_dir, ascii) :
          (reporter*) new csv_reporter(out, ncomm, v1enc, no_dir));

      std::unique_ptr<reporting_strategy> ps(new sequential_strategy(pr));

      size_t num_ok = 0, num_err = 0;
      for (auto pth: arguments) {
        size_t o, e;
        tie(o, e) = (*ps)(pth);
        num_ok += o;
        num_err += e;
      }

      status = (num_err != 0);

      if (!no_summ) {
        cout << "Processed " << num_ok << " files successfully; " <<
          num_err << " had errors." << endl;
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
