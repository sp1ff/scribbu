/**
 * \file genre.cc
 *
 * Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>
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

#include <memory>

#include <scribbu/winamp-genres.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"usage(scribbu genre -- set the genre for one or more files

scribbu genre [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, set the genre for all tags in all files named on the command
line. If an argument is a file, operate on the tags in that file. If the
argument is a directory, operate recursively on all files containing ID3 tags
therein.

The genre can be specified in a few ways:

    scribbu genre -w N

will interpret N as one of the genres defined by Winamp, specified as an integer
between 0 & 191 (inclusive). Run `scribbu genre -W' to print a list of the
Winamp genres.

    scribbu genre -g GENRE

will attempt to map to map the string GENRE to one of the Winamp genres using
Damerau-Levenshtein distance, but disregarding case.  For instance `scribbu
genre -g rok' will be interpreted as Winamp genre 17: "Rock".

    scribbu genre -G GENRE

will accept GENRE uncritically as the TCON to be used for ID3v2 tags. ID3v1
tags, if present, will have their genre field mapped to one of the Winamp values
again by case-insensitive Damerau-Levenshtein distance (or just set to 255 if
that fails). To explicitly set the ID3v1 version when specifying genre in this
way, add the --v1 flag (e.g. `scribbu genre -G foo --v1 17').

The operation can also be scoped by tag:

        -1, --v1-only  operate on ID3v1 tags only
        -2, --v2-only  operate on ID3v2 tags only
  -t N, --tag-index=N  operate on ID3v2 tag N only (zero-based, may be given
                       more than once)

This brings up the question of what to do if there is no ID3v1 and/or no ID3v2
tag. By default, in the absence of a tag, nothing will be done (so if invoked,
for instance, on a file with neither an ID3v1 nor an ID3v2 tag, this sub-command
would do nothing). This behavior can be customized by the following flags:

         -c,--create-v2  create an ID3v2.3 tag & add a TCON frame to it for any
                         file that has an ID3v1 tag, but no ID3v2 tag
         -C,--create-v1  create an ID3v1 tag and set its genre field
                         appropriately for any file that has an ID3v2 tag, but
                         no ID3v1 tag
  -a,--always-create-v2  create an ID3v2.3 tag & add a TCON frame to it for any
                         file that does not have an ID3v2.3 tag, regardless of
                         the presence or absence of an ID3v1 tag; use this with
                         caution when operating on directories, or you may find
                         assorted non-musical files have had an ID3v2 tag
                         prepended to them
  -A,--always-create-v1  create an ID3v1 tag & set its genre field
                         appropriately for any file that does not have an ID3v1
                         tag, regardless of the presence or absence of an ID3v2
                         tag; use this with caution when operating on
                         directories, or you may find assorted, non-musical files
                         have had ID3v1 tags appended to them

Finally, serialization of new or updated tags can be modified with the following
options:

          -n,--dry-run  don't actually modify any files; just print what *would*
                        be done on stdout
    -u,--adjust-unsync  update the unsynchronisatoin flag as needed on write
                        (the default is to just never use it)
   -b,--create-backups  create a backup copy of each file before writing new or
                        updated tags (this is good for experimenting or gaining
                        confidence with the tool)

For detailed help, say `scribbu genre --help'. To see the manual, say
`info "scribbu (genre)"'.
)usage");

////////////////////////////////////////////////////////////////////////////////
//                      functor for processing tagsets                        //
////////////////////////////////////////////////////////////////////////////////

/**
 * \brief process_dirent_args-compliant functor for adjusting the genre
 *
 * \sa process_dirent_args
 * \sa tagset_processor
 *
 *
 * Since a lot of the logic of processing one or more "file-or-directory"
 * sub-command arguments, reading their tags & making adjustments is
 * boilerplate, I've factored it out into the process_dirent_args template free
 * function & the tagset_processor functor. The logic specific to the `genre'
 * sub-command resides here.
 *
 *
 */

class set_genre: public tagset_processor
{
public:
  // There are three ways to specify the genre; they are mutually
  // exlcusive & exactly one must be given:
  //
  //   1. winamp-genre (-w N): N is the Winamp genre 0-191
  //   2. genre (-g GENRE): match text GENRE to winamp genre
  //   3. Genre (-G GENRE): use text GENRE unconditionally
  //
  // Since methods 2 & 3 both specify a string, I use a boolean to disambiguate
  // between the cases. That means two degrees of freedom in terms of specifying
  // the genre; that plus two degrees of freedom in the way that
  // tagset_processor defines the ID3v2 "tag scope policy" means four ctors:

  /// winamp-genre, either all or no ID3v2 tags processed
  set_genre(unsigned char genre,
            v2_simple_tag_scope_policy v2sp,
            v1_tag_scope_policy v1tsp,
            v2_creation_policy v2c,
            v1_creation_policy v1c,
            bool dry_run,
            bool verbose,
            bool adjust_unsync,
            bool create_backups):
    tagset_processor(v2sp, v1tsp, v2c, v1c, dry_run, verbose, adjust_unsync,
                     create_backups),
    genre_(genre)
  {
    auto x = scribbu::text_for_genre(genre_);
    if (!x) {
      // TODO(sp1ff): logical error-- what to throw?
    }
    content_type_ = *x;

  }
  /// either literal or best-match genre, either all or no ID3v2 tags processed
  set_genre(const std::string &genre,
            bool best_match,
            v2_simple_tag_scope_policy v2sp,
            v1_tag_scope_policy v1tsp,
            v2_creation_policy v2c,
            v1_creation_policy v1c,
            bool dry_run,
            bool verbose,
            bool adjust_unsync,
            bool create_backups):
    tagset_processor(v2sp, v1tsp, v2c, v1c, dry_run, verbose, adjust_unsync,
                     create_backups)
  {
    unsigned char v1genre;
    std::string content_type;
    size_t dist;
    tie(content_type, v1genre, dist) = scribbu::match_winamp_genre(genre);
    if (best_match) {
      content_type_ =  content_type;
      genre_ = v1genre;
    } else {
      content_type_ = genre;
      genre_ = v1genre;
    }
  }
  /// winamp-genre, some ID3v2 tags processed
  template <typename FII>
  set_genre(unsigned char genre,
            FII p0,
            FII p1,
            v1_tag_scope_policy v1tsp,
            v2_creation_policy v2c,
            v1_creation_policy v1c,
            bool dry_run,
            bool verbose,
            bool adjust_unsync,
            bool create_backups):
    tagset_processor(p0, p1, v1tsp, v2c, v1c, dry_run, verbose, adjust_unsync,
                     create_backups),
    genre_(genre)
  {
    auto x = scribbu::text_for_genre(genre_);
    if (!x) {
      // TODO(sp1ff): logical error-- what to throw?
    }
    content_type_ = *x;

  }
  /// either literal or best-match genre, some ID3v2 tags processed
  template <typename FII>
  set_genre(const std::string &genre,
            bool best_match,
            FII p0,
            FII p1,
            v1_tag_scope_policy v1tsp,
            v2_creation_policy v2c,
            v1_creation_policy v1c,
            bool dry_run,
            bool verbose,
            bool adjust_unsync,
            bool create_backups):
    tagset_processor(p0, p1, v1tsp, v2c, v1c, dry_run, verbose, adjust_unsync,
                     create_backups)
  {
    std::string content_type;
    unsigned char v1genre;
    size_t dist;
    tie(content_type, v1genre, dist) = scribbu::match_winamp_genre(genre);
    if (best_match) {
      content_type_ =  content_type;
      genre_ = v1genre;
    } else {
      content_type_ = genre;
      genre_ = v1genre;
    }
  }

public:
  /// Create a new ID3v1 tag when there are ID3v2 tags present
  virtual
  std::unique_ptr<scribbu::id3v1_tag>
  create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &v2);
  /// Create a new ID3v1 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v1_tag> create_v1();
  /// Create a new ID3v2 tag when there's an ID3v1 tag present
  virtual std::unique_ptr<scribbu::id3v2_tag>
  create_v2(const scribbu::id3v1_tag &v1);
  /// Create a new ID3v2 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v2_tag> create_v2();
  /// Process the ID3v1 tag
  virtual bool process_v1(scribbu::id3v1_tag &v1);
  /// Process an ID3v2.2 tag
  virtual bool process_v2(scribbu::id3v2_2_tag &v2);
  /// Process an ID3v2.3 tag
  virtual bool process_v2(scribbu::id3v2_3_tag &v2);
  /// Process an ID3v2.4 tag
  virtual bool process_v2(scribbu::id3v2_4_tag &v2);

private:
  unsigned char genre_;
  std::string content_type_;
};

/// Create a new ID3v1 tag when there are ID3v2 tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_genre::create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &v2)
{
  auto p = copy_id3_v2(v2.begin(), v2.end());
  p->set_genre(genre_);
  return p;
}

/// Create a new ID3v1 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_genre::create_v1()
{
  auto p = std::make_unique<scribbu::id3v1_tag>(false, false);
  p->set_genre(genre_);
  return p;
}

/// Create a new ID3v2 tag when there's an ID3v1 tag present
/*virtual*/ std::unique_ptr<scribbu::id3v2_tag>
set_genre::create_v2(const scribbu::id3v1_tag &v1)
{
  auto p = copy_id3_v1(v1);
  p->content_type(content_type_);
  return p;
}

/// Create a new ID3v2 tag when there are no other tags present
/*virtual*/ std::unique_ptr<scribbu::id3v2_tag>
set_genre::create_v2()
{
  // TODO(sp1ff): numeric literal
  auto p = std::make_unique<scribbu::id3v2_3_tag>(1024);
  p->content_type(content_type_);
  return p;
}

/// Process the ID3v1 tag
/*virtual*/ bool
set_genre::process_v1(scribbu::id3v1_tag &v1)
{
  v1.set_genre(genre_);
  if (v1.enhanced()) {
    // TODO(sp1ff): set enhanced genre?
  }
  return true;
}

/// Process an ID3v2.2 tag
/*virtual*/ bool
set_genre::process_v2(scribbu::id3v2_2_tag &v2)
{
  if (v2.text(scribbu::id3v2_text_frames::tcon) == content_type_) {
    return false;
  }
  v2.text(scribbu::id3v2_text_frames::tcon, content_type_);
  return true;
}

/// Process an ID3v2.3 tag
/*virtual*/ bool
set_genre::process_v2(scribbu::id3v2_3_tag &v2)
{
  if (v2.text(scribbu::id3v2_text_frames::tcon) == content_type_) {
    return false;
  }
  v2.text(scribbu::id3v2_text_frames::tcon, content_type_);
  return true;
}

/// Process an ID3v2.4 tag
/*virtual*/ bool
set_genre::process_v2(scribbu::id3v2_4_tag &v2)
{
  if (v2.text(scribbu::id3v2_text_frames::tcon) == content_type_) {
    return false;
  }
  v2.text(scribbu::id3v2_text_frames::tcon, content_type_);
  return true;
}

namespace {

  /// Print the Winamp genre list on stdout
  void print_winamp_genres()
  {
    using namespace std;

    string text;
    unsigned char num;
    vector<tuple<unsigned char, string>> genres;
    scribbu::get_id3v1_genre_list(scribbu::id3v1_genre_generation::winamp_5_6, back_inserter(genres));
    for (auto t: genres) {
      tie(num, text) = t;
      cout << setw(3) << setfill(' ') << num << ": " << text << endl;
    }
  }

  /// Take the three command line args that specify the genre, return a
  /// two-tuple consisting of the textual genre and a bool indicating whether
  /// that text is to be matched against the Winamp genres or taken literally;
  /// if ("",*) is returned, just use the numeric Winamp genre
  std::tuple<std::string, bool>
  textual_genre_for_args(unsigned char winamp_genre,
                         const std::string &winamp_text_genre,
                         const std::string &free_form_genre)
  {
    using namespace std;

    // There are three ways to specify the genre; they are mutually
    // exlcusive & exactly one must be given:
    //
    //   1. winamp-genre (-w N): N is the Winamp genre 0-191
    //   2. genre (-g GENRE): match GENRE to winamp genre
    //   3. Genre (-G GENRE): take GENRE unconditionally

    unsigned char scratch = 0;
    // TODO(sp1ff): numeric literal
    if (255 != winamp_genre) {
      scratch |= 1;
    }
    if (!winamp_text_genre.empty()) {
      scratch |= 2;
    }
    if (!free_form_genre.empty()) {
      scratch |= 4;
    }

    bool best_match;
    string textual_genre;

    switch (scratch) {
    case 0:
      // TODO(sp1ff): EXCEPTION
      throw std::runtime_error("at least one of -w, -g or -G must be given");
    case 1:
      /* OK */
      break;
    case 2:
      textual_genre = winamp_text_genre;
      best_match = true;
      break;
    case 4:
      textual_genre = free_form_genre;
      best_match = false;
      break;
    default:
      // TODO(sp1ff): EXCEPTION
      throw std::runtime_error("at most one of -w, -g or -G may be given");
    }

    return make_tuple(textual_genre, best_match);
  }

  std::tuple<tagset_processor::v2_tag_scope_policy, tagset_processor::v1_tag_scope_policy>
  tag_scope_policies_for_args(bool v2_only,
                              bool v1_only,
                              const std::vector<size_t> &tags)
  {
    tagset_processor::v1_tag_scope_policy v1tsp;
    tagset_processor::v2_tag_scope_policy v2tsp;

    // Scoping the operation: three options affect this:
    //  1. v1-only
    //  2. v2-only
    //  3. tags
    unsigned char scratch = 0;
    if (v1_only) {
      scratch |= 1;
    }
    if (v2_only) {
      scratch |= 2;
    }
    if (!tags.empty()) {
      scratch |= 4;
    }

    scratch = 0;
    switch (scratch) {
    case 0: // nothing specified-- defaults
      v1tsp = tagset_processor::v1_tag_scope_policy::yes;
      v2tsp = tagset_processor::v2_tag_scope_policy::all;
      break;
    case 1: // v1 tags only, nothing else
      v1tsp = tagset_processor::v1_tag_scope_policy::yes;
      v2tsp = tagset_processor::v2_tag_scope_policy::none;
      break;
    case 2: // v2 tags only, nothing else
      v1tsp = tagset_processor::v1_tag_scope_policy::no;
      v2tsp = tagset_processor::v2_tag_scope_policy::all;
      break;
    case 4: // -t only, defaults else
      v1tsp = tagset_processor::v1_tag_scope_policy::yes;
      v2tsp = tagset_processor::v2_tag_scope_policy::some;
      break;
    case 6: // -t only, v2_only
      v1tsp = tagset_processor::v1_tag_scope_policy::no;
      v2tsp = tagset_processor::v2_tag_scope_policy::some;
      break;
    default:
      // TODO(sp1ff): EXCEPTION
      throw std::runtime_error("invalid combination of -1, -2 & -t");
    }

    return std::make_tuple(v2tsp, v1tsp);
  }

  std::tuple<tagset_processor::v2_creation_policy, tagset_processor::v1_creation_policy>
  tag_creation_policies_for_args(bool create_v2,
                                 bool create_v2_always,
                                 bool create_v1,
                                 bool create_v1_always)
  {
    using namespace scribbu;

    // Handling non-existant tags: 1 & 2 below are independent
    //   1. create-v2 or always-create v2
    //   2. create-v1 or always-create-v1
    unsigned char scratch = 0;
    if (create_v1) {
      scratch |= 1;
    }
    if (create_v1_always) {
      scratch |= 2;
    }
    if (create_v2) {
      scratch |= 4;
    }
    if (create_v2_always) {
      scratch |= 8;
    }
    tagset_processor::v1_creation_policy v1cp;
    tagset_processor::v2_creation_policy v2cp;
    switch (scratch) {
    case 0: // defaults
      v1cp = tagset_processor::v1_creation_policy::never;
      v2cp = tagset_processor::v2_creation_policy::never;
      break;
    case 1: // create ID3v1
      v1cp = tagset_processor::v1_creation_policy::when_v2_present;
      v2cp = tagset_processor::v2_creation_policy::never;
      break;
    case 2: // always create ID3v1
      v1cp = tagset_processor::v1_creation_policy::always;
      v2cp = tagset_processor::v2_creation_policy::never;
      break;
    case 4: // create ID3v2
      v1cp = tagset_processor::v1_creation_policy::never;
      v2cp = tagset_processor::v2_creation_policy::when_v1_present;
      break;
    case 5: // create ID3v1, create ID3v2
      v1cp = tagset_processor::v1_creation_policy::when_v2_present;
      v2cp = tagset_processor::v2_creation_policy::when_v1_present;
      break;
    case 6: // alwyas create ID3v1, create ID3v2
      v1cp = tagset_processor::v1_creation_policy::always;
      v2cp = tagset_processor::v2_creation_policy::when_v1_present;
      break;
    case 8: // always create ID3v2
      v1cp = tagset_processor::v1_creation_policy::never;
      v2cp = tagset_processor::v2_creation_policy::always;
      break;
    case 9: // create ID3v1, always create ID3v2
      v1cp = tagset_processor::v1_creation_policy::when_v2_present;
      v2cp = tagset_processor::v2_creation_policy::always;
      break;
    case 10: // always create ID341, always create ID3v2
      v1cp = tagset_processor::v1_creation_policy::always;
      v2cp = tagset_processor::v2_creation_policy::always;
      break;
    default:
      throw po::validation_error(po::validation_error::multiple_values_not_allowed,
                                 "Only one each of -a & -A, -c & -C may be given");
    }

    return std::make_tuple(v2cp, v1cp);
  }

  void
  process_dirent_arg(const boost::filesystem::path &pth, set_genre &F)
  {
    using namespace std;

    using boost::filesystem::directory_iterator;
    using boost::filesystem::is_directory;
    using boost::filesystem::path;

    if (is_directory(pth)) {
      for_each(directory_iterator(pth), directory_iterator(),
               [&](const path &p) { process_dirent_arg(p, F); });
    } else if (is_regular_file(pth)) {
      F.process_file(pth);
    } else if (F.verbose()) {
      cout << "skipping non-file `" << pth << "'" << endl;
    }
  }

  /// Set the genre for some files--
  void genre(unsigned char winamp_genre,
             const std::string &winamp_text_genre,
             const std::string &free_form_genre,
             bool v1_only,
             bool v2_only,
             bool create_v2,
             bool create_v2_always,
             bool create_v1,
             bool create_v1_always,
             // TODO(sp1ff): pass a range instead?
             const std::vector<size_t> &tags,
             bool dry_run,
             bool verbose,
             bool adjust_unsync,
             bool create_backups,
             const std::vector<boost::filesystem::path>& args)
  {
    using namespace std;

    bool best_match;
    string textual_genre;
    tie(textual_genre, best_match) = textual_genre_for_args(winamp_genre,
                                                            winamp_text_genre,
                                                            free_form_genre);
    tagset_processor::v2_tag_scope_policy v2tsp;
    tagset_processor::v1_tag_scope_policy v1tsp;
    tie(v2tsp, v1tsp) = tag_scope_policies_for_args(v2_only, v1_only, tags);

    tagset_processor::v2_creation_policy v2cp;
    tagset_processor::v1_creation_policy v1cp;
    tie(v2cp, v1cp) = tag_creation_policies_for_args(create_v2, create_v2_always,
                                                     create_v1, create_v1_always);

    // TODO(sp1ff): in-progress
    typedef tagset_processor::v2_tag_scope_policy v2_tag_scope_policy;
    typedef tagset_processor::v2_simple_tag_scope_policy v2_simple_tag_scope_policy;
    typedef tagset_processor::v2_tag_scope_policy v2_tag_scope_policy;

    // Ho-kay: let's build-up the functor that will actually set the genre...
    std::unique_ptr<set_genre> pF;
    // TODO(sp1ff): numeric literal
    if (winamp_genre != 255 && v2_tag_scope_policy::some != v2tsp) {
      pF.reset(new set_genre(winamp_genre,
                             v2_tag_scope_policy::all == v2tsp ?
                             v2_simple_tag_scope_policy::all :
                             v2_simple_tag_scope_policy::none,
                             v1tsp, v2cp, v1cp, dry_run, verbose,
                             adjust_unsync, create_backups));
    } else if (winamp_genre == 255 && v2_tag_scope_policy::some != v2tsp) {
    // TODO(sp1ff): numeric literal
      pF.reset(new set_genre(textual_genre, best_match,
                             v2_tag_scope_policy::all == v2tsp ?
                             v2_simple_tag_scope_policy::all :
                             v2_simple_tag_scope_policy::none,
                             v1tsp, v2cp, v1cp, dry_run, verbose,
                             adjust_unsync, create_backups));
    // TODO(sp1ff): numeric literal
    } else if (winamp_genre != 255 && v2_tag_scope_policy::some == v2tsp) {
      pF.reset(new set_genre(winamp_genre, tags.begin(), tags.end(),
                             v1tsp, v2cp, v1cp, dry_run, verbose,
                             adjust_unsync, create_backups));
    // TODO(sp1ff): numeric literal
    } else if (winamp_genre == 255 && v2_tag_scope_policy::some == v2tsp) {
      pF.reset(new set_genre(textual_genre, best_match,
                             tags.begin(), tags.end(),
                             v1tsp, v2cp, v1cp, dry_run, verbose,
                             adjust_unsync, create_backups));
    } else {
      // TODO(sp1ff): should never be here-- provide more information?
      throw std::logic_error("");
    }

    for_each(args.begin(), args.end(), [&pF](const fs::path &pth) {
      set_genre &F = const_cast<set_genre&>(*pF);
      process_dirent_arg(pth, F);
    });
  }

  //////////////////////////////////////////////////////////////////////////////
  //                                 handler                                  //
  //////////////////////////////////////////////////////////////////////////////

  /**
   * \brief `genre' sub-command handler
   *
   * \sa handler_type
   * \sa register_command
   *
   *
   * `winamp genre' is a sub-command that takes a lot of options governing a lot
   * of behavior (fifteen options not including help at the time of this
   * writing, and they interact in various ways). It's about as complex as I can
   * make a scribbu sub-command and still have it be usable via the command-line
   * interface (as opposed to exposing it via the Scheme library).
   *
   *
   */

  int
  handle_genre(int argc, char **argv)
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
    //                cli & env |  opts  |  xopts  |                           //
    //                          +--------+---------+                           //
    //                                                                         //
    /////////////////////////////////////////////////////////////////////////////

    po::options_description clopts("command-line only options");
    clopts.add_options()
      ("help,h", po::bool_switch(), "Display help & exit; `--help' will display"
       "the man page for this sub-command & `-h' will display this sub-"
       "command's usage message")
      ("info", po::bool_switch(), "Display this sub-command's node in the "
       "scribbu Info manual");

    po::options_description xclopts("command-line only developer options");
    xclopts.add_options()
      ("man", po::bool_switch(), "Display the man page for this sub-command");

    po::options_description opts("general options");
    opts.add_options()
      ("adjust-unsync,u", po::bool_switch(), "Update the unsynchronisation "
       "flag if needed")
      ("always-create-v2,a", po::bool_switch(), "Always create an ID3v2 tag"
       "with a TCON frame for any file that has no ID3v2 tag")
      ("always-create-v1,A", po::bool_switch(), "Always create an ID3v1 tag"
       "with the genre field set appropriately for any file that has no "
       "ID3v1 tag")
      ("create-backups,b", po::bool_switch(), "Create a backup of each file "
       "before modifying it")
      ("create-v2,c", po::bool_switch(), "Create an ID3v2.3 tag containing "
       "a TCON frame for any file that an ID3v1 tag, but no ID3v2 tag")
      ("create-v1,C", po::bool_switch(), "Create an ID3v1 tag with the genre"
       " field set appropriately for any file that has an ID3v2 tag, but no "
       "ID3v1 tag")
      ("dry-run,n", po::bool_switch(), "Don't actually update any files; "
       "just print what *would* be done")
      ("genre,g", po::value<string>(), "Winamp genre, textual")
      ("Genre,G", po::value<string>(), "Genre, free-form text")
      ("list-winamp-genres,W", po::bool_switch(), "Display all 192 Winamp genres"
       " by number & name on stdout & exit")
      ("tag,t", po::value<vector<size_t>>(), "Zero-based index of the ID3v2 "
       "tag on which to operate; may be given more than once to select "
       "multiple tags")
      ("v1,v", po::value<unsigned char>(), "Genre to use for ID3v1 tags in "
       "combination with free-form text")
      ("v1-only,1", po::bool_switch(), "Operate on ID3v1 tags only; incompatible "
       "with --Genre")
      ("v2-only,2", po::bool_switch(), "Operate on ID3v2 tags only; incompatible "
       "with --v1")
      ("verbose,v", po::bool_switch(), "Produce more verbose output during operation")
      ("winamp,w", po::value<unsigned int>(), "Winamp genre, 0-191 (inclusive)");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
       "files or directories to be examined; if a directory is given, it will "
       "be searched recursively");

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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-genre",
                        "(scribbu) Invoking scribbu genre");

      po::store(parsed, vm);

      parsed = po::parse_environment(nocli, "SCRIBBU");
      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      //////////////////////////////////////////////////////////////////////////
      //                         parsing arguments                            //
      //////////////////////////////////////////////////////////////////////////

      bool adj_unsync         = vm["adjust-unsync"     ].as<bool>();
      bool always_create_v2   = vm["always-create-v2"  ].as<bool>();
      bool always_create_v1   = vm["always-create-v1"  ].as<bool>();
      bool create_backups     = vm["create-backups"    ].as<bool>();
      bool create_v2          = vm["create-v2"         ].as<bool>();
      bool create_v1          = vm["create-v1"         ].as<bool>();
      bool dry_run            = vm["dry-run"           ].as<bool>();
      bool list_winamp_genres = vm["list-winamp-genres"].as<bool>();
      bool v1_only            = vm["v1-only"           ].as<bool>();
      bool v2_only            = vm["v2-only"           ].as<bool>();
      bool verbose            = vm["verbose"           ].as<bool>();

      unsigned char winamp_int_genre = 255;
      if (vm.count("winamp")) {
        unsigned int scratch = vm["winamp"].as<unsigned int>();
        if (192 > scratch) {
          winamp_int_genre = (unsigned char) scratch;
        } else {
          // TODO(sp1ff): exceptoin
          throw std::runtime_error("bad integer winamp genre");
        }
      }

      string winamp_text_genre;
      if (vm.count("genre")) {
        winamp_text_genre = vm["genre"].as<string>();
        // TODO(sp1ff): convert to UTF-8
      }

      string free_form_genre;
      if (vm.count("Genre")) {
        // TODO(sp1ff): convert to UTF-8
        free_form_genre = vm["Genre"].as<string>();
      }

      // TODO(sp1ff): numeric literal
      // TODO(sp1ff): used? I think there's a bug lurking here
      unsigned char v1_genre = 255;
      if (vm.count("v1")) {
        v1_genre = vm["v1"].as<unsigned char>();
      }

      vector<size_t> tags = {{ 0 }};
      if (vm.count("tag")) {
        tags = vm["tag"].as<vector<size_t>>();
      }

      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      std::vector<fs::path> args;
      if (vm.count("arguments")) {
        for (auto s: vm["arguments"].as<std::vector<string>>()) {
          args.push_back(fs::path(s));
        }
      }

      //////////////////////////////////////////////////////////////////////////
      //                           implementation                             //
      //////////////////////////////////////////////////////////////////////////

      // At this point, we've un-packed all the options & arguments
      // successfully. Help-replated commands have been handled; the only reason
      // we wouldn't start processing `arguments' is if the Winamp genre list
      // was requested:
      if (list_winamp_genres) {
        print_winamp_genres();
      }
      else {
        genre(winamp_int_genre, winamp_text_genre, free_form_genre, v1_only, v2_only,
              create_v2, always_create_v2, create_v1, always_create_v1, tags,
              dry_run, verbose, adj_unsync, create_backups, args);
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

  register_command r("genre", handle_genre);

}
