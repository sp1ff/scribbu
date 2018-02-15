#include "config.h"
#include "command-utilities.hh"

#include <scribbu/csv-pprinter.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/scribbu.hh>

#include <exception>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>


namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"(scribbu dump -- dump ID3 tags from one or more files
to stdout

USAGE:

    scribbu dump [OPTION] FILE-OR-DIRECTORY...

For detailed help, say `scribbu dump --help'. To see the manual, say
`info "scribbu (dump) "'.
)");


///////////////////////////////////////////////////////////////////////////////
//                      classes private to this module                       //
///////////////////////////////////////////////////////////////////////////////

namespace {

  /// Dump ID3v2 tags, track data, and/or the ID3v1 tag to stdout
  class dumper: public std::unary_function<void, fs::path>
  {
  public:
    /// The format in which we shall dump information to stdout
    enum class format {
      /// Comma-separated variable format
      csv,
      /// Multi-line output format
      standard
    };

    enum class dump_id3v1 { yes, no };
    enum class dump_track { yes, no };
    enum class dump_id3v2 { yes, no };

  public:
    /// Print in standard format
    dumper(const boost::regex &file_regex, dump_id3v2 d0, dump_track d1,
           dump_id3v1 d2, std::size_t indent, bool expand_genre,
           scribbu::encoding v1enc);
    /// Print in CSV format
    dumper(const boost::regex &file_regex, dump_id3v2 d0, dump_track d1,
           dump_id3v1 d2, scribbu::encoding v1enc, std::size_t ncomm);
    void operator()(const fs::path &pth);

  private:
    boost::regex file_regex_;
    format fmt_;
    bool dump_id3v2_;
    bool dump_track_;
    bool dump_id3v1_;

    // Standard settings
    std::size_t indent_;
    bool expand_genre_;

    // Compact settings
    std::size_t ncomm_;

    // Shared settings
    scribbu::encoding v1enc_;
  };

  dumper::dumper(const boost::regex &file_regex,
                 dump_id3v2 d0,
                 dump_track d1,
                 dump_id3v1 d2,
                 std::size_t indent,
                 bool expand_genre,
                 scribbu::encoding v1enc):
    file_regex_(file_regex),
    fmt_(format::standard),
    dump_id3v2_(d0 == dump_id3v2::yes),
    dump_track_(d1 == dump_track::yes),
    dump_id3v1_(d2 == dump_id3v1::yes),
    indent_(indent),
    expand_genre_(expand_genre),
    v1enc_(v1enc)
  {
    using namespace scribbu;

    if (!dump_id3v2_ && !dump_track_ && !dump_id3v1_) {
      dump_id3v2_ = dump_track_ = dump_id3v1_ = true;
    }

    std::cout << pretty_print(indent_, expand_genre_, v1enc_);
  }

  dumper::dumper(const boost::regex &file_regex,
                 dump_id3v2 d0,
                 dump_track d1,
                 dump_id3v1 d2,
                 scribbu::encoding v1enc,
                 std::size_t ncomm):
    file_regex_(file_regex),
    fmt_(format::csv),
    dump_id3v2_(d0 == dump_id3v2::yes),
    dump_track_(d1 == dump_track::yes),
    dump_id3v1_(d2 == dump_id3v1::yes),
    v1enc_(v1enc),
    ncomm_(ncomm)
  {
    using namespace std;
    using namespace scribbu;

    if (!dump_id3v2_ && !dump_track_ && !dump_id3v1_) {
      dump_id3v2_ = dump_track_ = dump_id3v1_ = true;
    }

    cout << print_as_csv(ncomm_, v1enc_);
  }

  void
  dumper::operator()(const fs::path &pth)
  {
    using namespace std;
    using namespace boost;
    using namespace scribbu;

    if (!fs::is_regular_file(pth)) {
      return;
    }

    if (!file_regex_.empty() && !regex_match(pth.string(), file_regex_)) {
      return;
    }

    try {

      fs::ifstream ifs(pth, ios_base::binary);

      vector<unique_ptr<scribbu::id3v2_tag>> id3v2;
      scribbu::read_all_id3v2(ifs, back_inserter(id3v2));
      scribbu::track_data td((istream&)ifs);
      unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(ifs);

      cout << pth << ":" << endl;

      if (dump_id3v2_) {
        for (ptrdiff_t i = 0, n = id3v2.size(); i < n; ++i) {
          cout << *(id3v2[i]);
        }
      }

      if (dump_track_) {
        cout << td;
      }

      if (dump_id3v1_ && pid3v1) {
        cout << *pid3v1;
      }

      cout << endl;

    }
    catch (const std::exception &ex) {
      cerr << "Failed to read " << pth << ": " << ex.what() << endl;
    }

  }

  std::istream&
  operator>>(std::istream &is, dumper::format &fmt)
  {
    static const boost::regex CSV("1|csv");
    static const boost::regex STANDARD("2|standard|std");

    std::string s;
    is >> s;

    if (boost::regex_match(s, CSV)) {
      fmt = dumper::format::csv;
    }
    else if (boost::regex_match(s, STANDARD)) {
      fmt = dumper::format::standard;
    }
    else {
      throw std::invalid_argument("Unknown format");
    }

    return is;

  }

  std::ostream&
  operator<<(std::ostream& os, const dumper::format &fmt)
  {
    switch (fmt) {
    case dumper::format::csv:
      os << "csv";
      break;
    case dumper::format::standard:
      os << "standard";
      break;
    default:
      throw std::logic_error("Unknown format");
    }
    return os;
  }


///////////////////////////////////////////////////////////////////////////////
//                       sub-command handler                                 //
///////////////////////////////////////////////////////////////////////////////

  int
  handler(const std::vector<std::string>  &tokens,
          help_level                       help,
          const boost::optional<fs::path> &cfg)
  {
    using namespace std;

    using scribbu::encoding;

    typedef dumper::format format;

    int status = EXIT_SUCCESS;

    ///////////////////////////////////////////////////////////////////////////
    //                                                                        /
    //                       C O M M A N D   O P T I O N S                    /
    //                                                                        /
    // Let's divide the options in two ways:                                  /
    //                                                                        /
    // - public versus developer-only options                                 /
    // - options permissible only on the command line versus options          /
    //   permissible on the command line, configuration file, and the         /
    //   environment                                                          /
    //                                                                        /
    //                            public   private                            /
    //                          +--------+---------+                          /
    //                cli-only  | clopts | xclopts |                          /
    //                          +--------+---------+                          /
    //                cli, cfg, |  opts  |  xopts  |                          /
    //                & env     +--------+---------+                          /
    //                                                                        /
    ///////////////////////////////////////////////////////////////////////////

    po::options_description clopts("command-line only options");
    // None at this time...

    po::options_description xclopts("command-line only developer options");
    // None at this time...

    po::options_description opts("general options");
    opts.add_options()
      ("id3v1-tags,1", po::bool_switch(), "Display only ID3v1 tags")
      ("track-data,D", po::bool_switch(), "Display only track data")
      ("id3v2-tags,2", po::bool_switch(), "Display only ID3v2 tags")
      ("indent,i", po::value<size_t>()->default_value(0), "number of "
       "spaces to indent")
      ("no-expand-genre,g", po::bool_switch(), "Don't attempt to epand the "
       "genre")
      ("expression,e", po::value<string>(), "If specified, defines a regular "
       "expression filtering files (i.e. only filenames matching the regular "
       "expression will be considered)")
      ("format,f", po::value<format>()-> default_value(format::standard), 
       "Output format")
      ("v1-encoding,c", po::value<encoding>()->default_value(encoding::CP1252),
       "Encoding uses to interpret text in ID3v1 tags.");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<vector<string>>()->required(), "one or more "
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
        // TODO(sp1ff): Might be nice to support other man page readeres
        // (woman, e.g.)
        execlp("man", "man", "scribbu-dump", (char *)NULL);
        throw runtime_error("failed to exec"); // TODO(sp1ff): grab errno
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
        vector<fs::path> arguments;
        for (auto s: vm["arguments"].as<vector<string>>()) {
          arguments.push_back(fs::path(s));
        }

        boost::regex file_regex;
        if (vm.count("expression")) {
          file_regex = boost::regex(vm["expression"].as<string>());
        }

        typedef dumper::dump_id3v2 dump_id3v2;
        typedef dumper::dump_track dump_track;
        typedef dumper::dump_id3v1 dump_id3v1;

        format fmt = vm["format"    ].as<format>();
        dump_id3v2 f0 = vm["id3v2-tags"].as<bool>() ? dump_id3v2::yes :
          dump_id3v2::no;
        dump_track f1 = vm["track-data"].as<bool>() ? dump_track::yes :
          dump_track::no;
        dump_id3v1 f2 = vm["id3v1-tags"].as<bool>() ? dump_id3v1::yes :
          dump_id3v1::no;

        unique_ptr<dumper> p;
        if (dumper::format::standard == fmt) {
          size_t   indent =   vm["indent"         ].as<size_t  >();
          bool     expand = ! vm["no-expand-genre"].as<bool    >();
          encoding v1enc  =   vm["v1-encoding"    ].as<encoding>();
          p.reset(new dumper(file_regex, f0, f1, f2,
                             indent, expand, v1enc));
        }
        else {
          encoding v1enc = vm["v1-encoding" ].as<encoding>();
          size_t   ncomm = vm["num-comments"].as<size_t  >();
          p.reset(new dumper(file_regex, f0, f1, f2,
                             v1enc, ncomm));
        }

        for (auto x: arguments) {
          if (fs::is_directory(x)) {
            for_each(fs::recursive_directory_iterator(x),
                     fs::recursive_directory_iterator(),
                     ref(*p));
          }
          else {
            (*p)(x);
          }
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

  } // End handler.

  register_command r("dump", handler);

}
