#include "config.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/program_options.hpp>
#include <boost/regex.hpp>

#include "command-utilities.hh"

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/scribbu.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;


namespace {

  const std::string USAGE(R"(scribbu rename -- rename .mp3 files

scribbu rename [option...] file-or-directory [file-or-directory...]

Rename one or more files, and/or the files in one or more directories,
according to their ID3 tags.

The command will, by default, rename each file as:

    "<artist> - <title>.<extension>"

The caller can customize this, however, using the --template option. This
option accepts a string that mixes text with replacement parameters (such as
title, artist, album &c).

The replacement parameters begin with a % (escape % signs that do *not* start a
replacement with a backslash). Each parameter has a one-characer short form as
well as a long-form name. For example, artist can be represented as either 'A'
or"artist". The short form would be %A, the long %(artist).

If the long form is used, the action of the replacement paramter may,
optionally, be modified by giving options after a colon. The options take the
form: opt0&opt1&opt2&..., where opti is of the form name=value or just name. To
continue the example, if we wanted the artist to always be taken from the ID3v1
tag, and that field is encoded as ISO-8859-1, we could say:
%(artist:v1-only&v1-encoding=iso-8859-1).

Full list of replacement parameters & their options

Tag-based replacements:

replacement name, short form, long form(s)

    - album: L, album
    - artist: A, artist
    - content type: G, {content-type,genre}
    - encoded by: e, encoded-by
    - title: T, title
    - year: Y, year

Tag-based replacements take the following options:

    - source of the replacement text: prefer-v2, prefer-v1, v2-only, v1-only
    - character encoding when the ID3v1 tag is used: v1-encoding={auto,
      iso-8859-1,ascii,cp1252,utf-8,utf-16-be,utf-16,le,utf-32}
    - handling "The ...": the(suffix),the=suffix,the=prefix
    - capitalizing words: capitalization,capitalization={all-upper,all-lower}
    - whitespace: compress, ws="TEXT"
    - output character set: output=iso-8859-1,ascii,...

e.g. %(artist:prefer-v2&v1-encoding=cp1252&the=suffixcompress) applied to
a file whose ID3v2 tag had an artist frame of "The  Pogues" would produce
"Pogues, The".

In addition to the above, the year can be formatted as two digits or four
by giving "yy" or "yyyy" in the options for that replacement.

Filename-based replacement parameters:

    - basename: b, basename
    - extension: E, extension (incl. dot)

These take the same "The", capitalization & whitespace options as tag-based 
replacements.

Replacements based on the track data:

    - MD5 checksum: 5, md5
    - size: S, size (bytes)

Both replacements take the following options:

    - base: base={decimal,hex}
    - case for hexadecimal digits: hex-case={U,L}

)");

}


///////////////////////////////////////////////////////////////////////////////
//                          implementation                                   //
///////////////////////////////////////////////////////////////////////////////

namespace {

  class renamer {

  public:
    renamer(const std::string templat,
            const fs::path   &output,
            bool              dry_run,
            bool              rename,
            bool              verbose);

  public:

    void operator()(const fs::path &pth) const;

  private:

    void process_directory(const fs::path &pth) const;
    void process_file(const fs::path &pth) const;

  private:
    fs::path                    output_;
    scribbu::template_processor P_;
    bool                        dry_run_;
    bool                        rename_;
    bool                        verbose_;

  };

  renamer::renamer(const std::string templat,
                   const fs::path   &output,
                   bool              dry_run,
                   bool              rename,
                   bool              verbose):
    output_ (output                              ),
    P_      (scribbu::template_processor(templat)),
    dry_run_(dry_run                             ),
    rename_ (rename                              ),
    verbose_(verbose                             )
  { }

  void renamer::operator()(const fs::path &pth) const
  {
    if (! fs::exists(pth)) {
      throw std::invalid_argument(pth.string() + " does not appear to exist.");
    }

    if (fs::is_directory(pth)) {
      process_directory(pth);
    } else {
      process_file(pth);
    }

  }

  void renamer::process_file(const fs::path &pth) const
  {

    try {

      // Figure out the destination; if 'output' was given, all files will be
      // copied into that directory. Otherwise, they will be renamed in-place.
      fs::path out = output_.empty() ? pth.parent_path() : output_;
      out /= P_(pth);

      if (dry_run_ || verbose_) {
        std::cout << pth << " => " << out << std::endl;
      }
      if (! dry_run_ && pth != out) {
        fs::copy_file(pth, out);
        if (rename_) {
          fs::remove(pth);
        }
      }

    } catch (const scribbu::tbt_support::error &ex) {
      std::cerr << pth << ": " << ex.what() << std::endl;
    }

  }

  void renamer::process_directory(const fs::path &pth) const
  {
    const boost::regex REGEX(".*\\.mp3");

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

}


///////////////////////////////////////////////////////////////////////////////
//                             handler                                       //
///////////////////////////////////////////////////////////////////////////////

namespace {

  int
  handle_rename(const std::vector<std::string>  &tokens,
                help_level                       help,
                const boost::optional<fs::path> &cfg)
  {
    using namespace std;

    const std::string DEFAULT_TEMPLATE("%A - %T.mp3");
    const bool        DEFAULT_RENAME = false;
    const bool        DEFAULT_VERBOSE = false;

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
      ("dry-run,n", po::bool_switch(), "Dry-run; only print what would "
       "happen")
      ("output,o", po::value<fs::path>(), "If specified, copy the output files "
       "to this directory, rather than renaming in-place.")
      ("rename,r", po::bool_switch()->default_value(DEFAULT_RENAME),
       "Remove the source file (ignored if --dry-run is given)")
      ("template,t", po::value<std::string>()->default_value(DEFAULT_TEMPLATE),
       "Template by which to rename the files.")
      ("verbose,v", po::bool_switch()->default_value(DEFAULT_VERBOSE),
       "Produce verbose output.");

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

        bool dry_run = vm["dry-run"].as<bool>();
        fs::path output;
        if (vm.count("output")) {
          output = vm["output"].as<fs::path>();
        }
        std::string templat = vm["template"].as<std::string>();
        bool rename = vm["rename"].as<bool>();
        bool verbose = vm["verbose"].as<bool>();

        std::unique_ptr<renamer> pr(new renamer(templat, output, dry_run,
                                                rename, verbose));

        std::for_each(arguments.begin(), arguments.end(), std::ref(*pr));
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

  } // End free function handle_rename.

  register_command r("rename", handle_rename);

}
