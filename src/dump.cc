#include "config.h"
#include "command-utilities.hh"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/regex.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {

  // TODO: Write a proper usage message for the dump sub-command
  const std::string USAGE(R"(scribbu dump -- dump ID3 tags from one or more files
)");

}

///////////////////////////////////////////////////////////////////////////////
//                      classes private to this module                       //
///////////////////////////////////////////////////////////////////////////////

#if 0
namespace {

  /// Dump ID3v2 tags, track data, and/or the ID3v1 tag to stdout
  class dumper: public std::unary_function<void, fs::path>
  {
  public:
    /// The format in which we shall dump information to stdout
    enum class format {
      /// Single-line output format
      compact,
      /// Comma-separated variable format
      csv,
      /// Multi-line output format
      standard
    };

  public:
    dumper(const boost::regex &file_regex, format fmt, unsigned char mask):
      file_regex_(file_regex),
      fmt_(fmt),
      dump_id3v2_(0 != (0x04 & mask)),
      dump_track_(0 != (0x02 & mask)),
      dump_id3v1_(0 != (0x01 & mask))
    {
      if (!dump_id3v2_ && !dump_track_ && !dump_id3v1_) {
        dump_id3v2_ = dump_track_ = dump_id3v1_ = true;
      }
    }
    void operator()(const fs::path &pth);

  private:
    boost::regex file_regex_;
    format fmt_;
    bool dump_id3v2_;
    bool dump_track_;
    bool dump_id3v1_;
  };

  void
  dumper::operator()(const fs::path &pth)
  {
    using namespace std;
    using namespace boost;
    using namespace scribbu;

    using scribbu::compact_id3v1_formatter;
    using scribbu::csv_id3v1_formatter;
    using scribbu::standard_id3v1_formatter;

    static const size_t DIGEST_SIZE = scribbu::track_data::DIGEST_SIZE;

    if (!file_regex_.empty() && !regex_match(pth.string(), file_regex_)) {
      return;
    }

    fs::ifstream ifs(pth, ios_base::binary);

    vector<unique_ptr<scribbu::id3v2_tag>> id3v2;
    scribbu::read_all_id3v2(ifs, back_inserter(id3v2));
    scribbu::track_data td((istream&)ifs);
    unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(ifs);

    switch (fmt_) {
    case format::compact:
      cout << compact_id3v1_formatter(id3v1_encoding::automatic,
                                      id3v1_genre_expansion::expand,
                                      ',');
      break;
    case format::csv:
      cout << csv_id3v1_formatter(id3v1_encoding::automatic,
                                  id3v1_genre_expansion::expand);
      break;
    case format::standard:
      cout << standard_id3v1_formatter(id3v1_encoding::automatic,
                                       id3v1_genre_expansion::expand,
                                       4);
      break;
    default:
      throw std::logic_error("Unknown format");
    }

    // TODO:
    // if (dump_id3v2_) {
    //   for (size_t i = 0, n = id3v2.size(); i < n; ++i) {
    //     cout << *( id3v2[i] ) << endl;
    //   }
    // }

    if (dump_track_) {

      unsigned char md5[DIGEST_SIZE];
      td.get_md5(md5);
      cout << "0x" << hex << td.size() << " bytes of track data (MD5: 0x" <<
        hex << setfill('0');
      for (size_t i = 0; i < DIGEST_SIZE; ++i) {
        cout << hex << setfill('0') << (int)md5[i];
      }
      cout << ")" << endl;

    }

    if (dump_id3v1_ && pid3v1) {
      cout << *pid3v1 << endl;
    }

  }

  std::istream&
  operator>>(std::istream &is, dumper::format &fmt)
  {
    static const boost::regex COMPACT("0|compact|cmp");
    static const boost::regex CSV("1|csv");
    static const boost::regex STANDARD("2|standard|std");

    std::string s;
    is >> s;
    if (boost::regex_match(s, COMPACT)) {
      fmt = dumper::format::compact;
    }
    else if (boost::regex_match(s, CSV)) {
      fmt = dumper::format::csv;
    }
    else if (boost::regex_match(s, STANDARD)) {
      fmt = dumper::format::standard;
    }
    else {
      // TODO: What exception to throw?
      throw std::runtime_error("Unknown format");
    }

    return is;

  }

} // End this xlation unit's un-named namespace.
#endif // 0


///////////////////////////////////////////////////////////////////////////////
//                       sub-command handler                                 //
///////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_dump(const std::vector<std::string>  &tokens,
              help_level                       help,
	      const boost::optional<fs::path> &cfg)
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

    po::options_description xclopts("command-line only developer options");

    po::options_description opts("general options");
    opts.add_options()
      ("id3v1-tags,1", po::bool_switch(), "Display only ID3v1 tags")
      ("id3v2-tags,2", po::bool_switch(), "Display only ID3v2 tags")
      ("track-data,D", po::bool_switch(), "Display only track data")
      ("regex,r", po::value<string>(), "If specified, defines a regular "
       "expression filtering files (i.e. only filenames matching the regular "
       "expression will be considered)")
      // ("format,f", po::value<dumper::format>(), "Output format")
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
        if (cfg) {
          fs::ifstream ifs(cfg.get());
          parsed = po::parse_config_file(ifs, nocli);
          po::store(parsed, vm);
        }

        // and finally any environment variables.
        parsed = po::parse_environment(nocli, "SCRIBBU");
        po::store(parsed, vm);

        po::notify(vm);

#       if 0
        // That's it-- the list of files and/or directories to be processed
        // should be waiting for us in 'arguments'...
        // Work around to https://svn.boost.org/trac/boost/ticket/8535
        vector<fs::path> arguments;
        for (auto s: vm["arguments"].as<vector<string>>()) {
          arguments.push_back(fs::path(s));
        }

        boost::regex file_regex;
        if (vm.count("regex")) {
          file_regex = boost::regex(vm["regex"].as<string>());
        }

        dumper::format fmt = dumper::format::standard;
        if (vm.count("format")) {
          fmt = vm["format"].as<dumper::format>();
        }

        bool dump_id3v2 = vm["id3v2-tags"].as<bool>();
        bool dump_track = vm["track-data"].as<bool>();
        bool dump_id3v1 = vm["id3v1-tags"].as<bool>();

        unsigned char mask = 0;
        if (dump_id3v2) {
          mask |= 4;
        }
        if (dump_track) {
          mask |= 2;
        }
        if (dump_id3v1) {
          mask |= 1;
        }

        if (!mask) {
          mask = 7;
        }

        dumper D(file_regex, fmt, mask);

        for (auto x: arguments) {

          if (fs::is_directory(x)) {
            for_each(fs::recursive_directory_iterator(x),
                     fs::recursive_directory_iterator(),
                     ref(D));
          }
          else {
            D(x);
          }
        }

#       endif // 0
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

  } // End handle_dump.

  register_command r("dump", handle_dump);

}
