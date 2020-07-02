/**
 * \file popm.cc
 *
 * Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>
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

#include <queue>
#include <string>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/regex.hpp>

#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/tagset.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

const std::string USAGE(R"(scribbu popm -- manage playcount and/or popularimeter frames

scribbu popm [OPTION...] FILE-OR-DIRECTORY [FILE-OR-DIRECTORY...]

By default, increment the play count in all PCNT & POPM frames in all tags
in the files named on the command line. If an argument is a directory, increment
the play count in all files in the directory tree rooted at that argument
containing ID3v2 tags.

This operation can be scoped in a few ways:

    scribbu popm -t=1 ...

will only increment the play count in the second tag in every file.

    scribbu popm -p ...

will only modify PCNT frames (not POPM).

    scribbu popm -m

will only modify POPM frames (not PCNT).

    scribbu popm -o sp1ff@pobox.com

will only modify POPM frames with an owner field of "sp1ff@pobox.com".

While the default effect on the play count(s) is to increment by one, this can
also be modified in a few ways:

      --count=C,-c C: explicitly set the play count to C
  --increment=I,-i I: increment the play count by I (instead of 1)

This command can also be used to set the rating. The ID3v2 POPM frame expresses
the rating as an unsigned int from 0 to 255 inclusive. The user may specify that
rating in one of two ways:

    1. by simply providing such a number: e.g. --rating=128

    2. in terms of "stars"; many applications express ratings in terms of one to
       five stars. The most natural way to express this would seem to be the
       asterisk, but since that would likely be inconvenient in the shell,
       scribbu will accept almost any character, repeated one-to-five times, as
       "stars": --rating=###

In the second case, scribbu will map from "stars" to POPM ratings as follows:

    | "stars" | numeric |
    +---------+---------|
    | *       |       1 |
    | **      |      64 |
    | ***     |     128 |
    | ****    |     196 |
    | *****   |     255 |

Readers of a certain age may recognize that as Winamp's scheme.

The question arises: what if a given frame (PCNT, say) doesn't exist in a tag
selected for procesing? By default, nothing. That can be changed with the
--create flag, which will create the missing frame if possible.

If no ID3v2 tag exists whatsoever, by default no action will be taken. This
behavior can be changed with the following two flags:

         -c,--create-v2  create an ID3v2.3 tag & add the relevant frame(s) to
                         it for any file that has an ID3v1 tag, but no ID3v2 tag
  -a,--always-create-v2  create an ID3v2.3 tag & add the relevant frame(s) to
                         it for any file that does not have an ID3v2.3 tag,
                         regardless of the presence or absence of an ID3v1 tag;
                         use this with caution when operating on directories,
                         or you may find assorted non-musical files have had
                         an ID3v2 tag prepended to them


Finally, serialization of new or updated tags can be modified with the following
options:

          -n,--dry-run  don't actually modify any files; just print what *would*
                        be done on stdout
    -u,--adjust-unsync  update the unsynchronisatoin flag as needed on write
                        (the default is to just never use it)
   -b,--create-backups  create a backup copy of each file before writing new or
                        updated tags (this is good for experimenting or gaining
                        confidence with the tool)

For detailed help, say `scribbu popm --help'. To see the manual, say
`info "scribbu (popm) "'.
)");

/**
 * \struct rating_tag
 *
 * \brief represents a POPM rating; exists as a separate type only to
 * participate in boost program options validation
 *
 *
 * Cf. <a href="https://www.boost.org/doc/libs/1_58_0/doc/html/program_options/howto.html#idp337860416">
 * here</a> for the documentation on this system, and
 * <a href="https://github.com/boostorg/program_options/blob/develop/example/regex.cpp">
 * here</a> for an example.
 *
 *
 */

////////////////////////////////////////////////////////////////////////////////
//                       functor for processing tagsets                       //
////////////////////////////////////////////////////////////////////////////////

/**
 * \brief process_dirent_args-compliant functor for adjusting the ratings and/or
 * playcount
 *
 * \sa process_dirent_args
 * \sa tagset_processor
 *
 *
 * Since a lot of the logic of processing one or more "file-or-directory"
 * sub-command arguments, reading their tags & making adjustments is
 * boilerplate, I've factored it out into the process_dirent_args template free
 * function & the tagset_processor functor. The logic specific to the `popm'
 * sub-command resides here.
 *
 *
 */

class set_pcnt_popm: public tagset_processor
{
public:
  /// Which frames shall we update?
  enum class frame_policy { both, pcnt_only, popm_only };
  /// Is the playcount an increment or an absolute number?
  enum class pc_policy { increment, absolute };

  const size_t DEFAULT_PADDING = 1024;

public:
  set_pcnt_popm(boost::optional<unsigned char> rating,
                frame_policy fp,
                const boost::optional<std::string> &owner,
                size_t pc,
                pc_policy pc_pol,
                bool create_frame,
                v2_creation_policy v2c,
                bool dry_run,
                bool verbose,
                bool adjust_unsync,
                bool create_backups):
    tagset_processor(v2_simple_tag_scope_policy::all,
                     v1_tag_scope_policy::no,
                     v2c,
                     v1_creation_policy::never,
                     dry_run, verbose, adjust_unsync, create_backups),
    rating_(rating),
    frame_policy_(fp),
    owner_(owner),
    play_count_(pc),
    play_count_policy_(pc_pol),
    create_frame_(create_frame)
  { }
  template <typename FII>
  set_pcnt_popm(boost::optional<unsigned char> rating,
                frame_policy fp,
                const boost::optional<std::string> &owner,
                size_t pc,
                pc_policy pc_pol,
                bool create_frame,
                v2_creation_policy v2c,
                FII p0, FII p1,
                bool dry_run,
                bool verbose,
                bool adjust_unsync,
                bool create_backups):
    tagset_processor(p0, p1,
                     v1_tag_scope_policy::no,
                     v2c,
                     v1_creation_policy::never,
                     dry_run, verbose, adjust_unsync, create_backups),
    rating_(rating),
    frame_policy_(fp),
    owner_(owner),
    play_count_(pc),
    play_count_policy_(pc_pol),
    create_frame_(create_frame)
  { }

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
  template <typename tag_type>
  struct tag_traits
  {
    // typename tag_type
    // typename frame_type
    // typename pcnt_type
    // typename popm_type
    // bool is_pcnt(const frame_type&);
    // bool is_popm(const frame_type&);
    // pcnt_type make_pcnt(size_t pc);
    // popm_type make_popm(const string &email, unsigned char rating, size_t pc);
  };

  /// Process a PCNT frame
  template <typename tag_type>
  size_t process_pcnt(typename tag_type::pcnt_type &G)
  {
    using namespace std;
    using namespace scribbu;

    size_t new_pc = play_count_;
    if (pc_policy::increment == play_count_policy_) {
      new_pc += G.count();
    }
    if (dry_run() || verbose()) {
      cout << "Setting PCNT to " << new_pc << "." << endl;
    }
    if (!dry_run()) {
      G.count(new_pc);
    }
    return new_pc;
  }

  /// Process a POPM frame
  template <typename tag_type>
  size_t process_popm(typename tag_type::popm_type &G,
                      const std::string &email)
  {
    using namespace std;
    using namespace scribbu;

    size_t new_pc = play_count_;
    if (pc_policy::increment == play_count_policy_) {
      new_pc += G.count();
    }
    unsigned char rating = rating_ ? *rating_ : 0;
    if (dry_run() || verbose()) {
      cout << "Setting POPM/" << email << "counter to " << new_pc <<
        ", & rating to " << (unsigned int)rating << "." << endl;
    }
    if (!dry_run()) {
      G.count(new_pc);
      G.rating(rating);
    }

    return new_pc;
  }
  /**
   * \brief template member function for creating or updating the PCNT and/or
   * POPM frames, in terms if id3v2_tag sub-classes
   *
   *
   * \param tag [in,out] reference to an ID3v2 tag upon which to operate
   *
   * \return true if the tag was modified in any way, false else (NB this value
   * ignores the "dry-run" flag; i.e. if `--dry-run' was given, and the tag
   * *would* have been modified, this method will still return true
   *
   *
   * At the time of this writing, class id3v2_tag doesn't offer virtual
   * functions for handling playcounts or ratings so I need to work in terms of
   * the concrete sub-class (i.e. class id3v2_2_tag, id3v2_3_tag, or
   * id3v2_4_tag). Since the logic is the same in each case, I've written this
   * template, and factored out type-specific information and logic into a
   * traits class (tag_traits).
   *
   * \todo Consider adding playcount- and popularimeter-related virtuals to
   * class id3v2_tag & cleaning-up the popm sub-command implementation
   * considerably.
   *
   *
   */

  template <typename tag_type>
  bool
  process_tag(tag_type &tag, bool create_frame)
  {
    using namespace std;
    using namespace scribbu;

    typedef tag_traits<tag_type> traits_type;

    typedef typename traits_type::frame_type frame_type;
    typedef typename traits_type::pcnt_type  pcnt_type;
    typedef typename traits_type::popm_type  popm_type;

    size_t num_pcnt = 0, // # PCNT frames processed so far
           num_popm = 0, // # POPM frames processed so far
           curr_pc  = 0; // maximal play count written so far
    // Walk the tag, processing an PCNT and/or POPM frames we find, and
    // which our configuration directs us to process:
    for (frame_type &F: tag) {

      // PCNT
      if ((create_frame || frame_policy::popm_only != frame_policy_) &&
          traits_type::is_pcnt(F)) {
        // Just ugh.
        pcnt_type &G = dynamic_cast<pcnt_type&>(F);
        size_t new_pc;
        if (frame_policy::popm_only == frame_policy_) {
          new_pc = G.count();
        } else {
          new_pc = process_pcnt<traits_type>(G);
          ++num_pcnt;
        }
        if (new_pc > curr_pc) curr_pc = new_pc;
      }

      // POPM
      if ((create_frame || frame_policy::pcnt_only != frame_policy_) &&
          traits_type::is_popm(F)) {
        // Just ugh.
        popm_type &G = dynamic_cast<popm_type&>(F);
        string email = G.template email<string>();
        size_t new_pc = G.count();
        if (!owner_ || email == *owner_) {
          new_pc = process_popm<traits_type>(G, email);
          ++num_popm;
        }
        if (new_pc > curr_pc) curr_pc = new_pc;
      }

    } // End  iteration over the frames in `tag'.

    // Ho-kay: at this point, we've updated any frames we needed to in `tag'
    // (there may have been none), and `curr_pc' is the highest play count
    // written to this tag (which could be zero).

    // If we've found no frames of either type, and our configuration says
    // we should be processing frames of that type, and `create_frame' is
    // true, now's the time to do so.
    bool upd = (num_popm != 0) || (num_pcnt != 0);

    if (create_frame) {

      if (frame_policy::popm_only != frame_policy_ && 0 == num_pcnt) {
        if (dry_run() || verbose()) {
          cout << "Creating a new PCNT frame with a play count of " << curr_pc <<
            "." << endl;
        }
        if (!dry_run()) {
          tag.push_back(traits_type::make_pcnt(curr_pc));
        }
        upd = true;
      }

      // Only create new POPM frames in the presence of a rating & an owner
      if (frame_policy::pcnt_only != frame_policy_ && 0 == num_popm &&
          owner_ && rating_) {
        string email = *owner_;
        unsigned char rating = *rating_ ;
        if (dry_run() || verbose()) {
          cout << "Creating a new POPM frame with an owner of " << email <<
            " a rating of " << (unsigned int)rating << " and a play count of " <<
            curr_pc << "." << endl;
        }
        if (!dry_run()) {
          tag.push_back(traits_type::make_popm(email, rating, curr_pc));
        }
        upd = true;
      }

    } // End if on `create_frame'.

    return upd;
  }

private:
  boost::optional<unsigned char> rating_;
  frame_policy frame_policy_;
  boost::optional<std::string> owner_;
  size_t play_count_;
  pc_policy play_count_policy_;
  bool create_frame_;

};

/// Create a new ID3v1 tag when there are ID3v2 tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_pcnt_popm::create_v1(
    const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &v2)
{
  throw unknown_op(unknown_op::create_v1_from_v2);
}

/// Create a new ID3v1 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v1_tag>
set_pcnt_popm::create_v1()
{
  throw unknown_op(unknown_op::create_v1);
}

/// Create a new ID3v2 tag when there's an ID3v1 tag present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_pcnt_popm::create_v2(const scribbu::id3v1_tag &v1)
{
  auto p = copy_id3_v1(v1);
  process_tag(*p, true);
}

/// Create a new ID3v2 tag when there are no other tags present
/*virtual*/
std::unique_ptr<scribbu::id3v2_tag>
set_pcnt_popm::create_v2()
{
  auto p = std::make_unique<scribbu::id3v2_3_tag>(DEFAULT_PADDING);
  process_tag(*p, true);
}

/// Process the ID3v1 tag
/*virtual*/
bool
set_pcnt_popm::process_v1(scribbu::id3v1_tag &v1)
{
  throw unknown_op(unknown_op::process_v1);
}

/// Process an ID3v2.2 tag
/*virtual*/
bool
set_pcnt_popm::process_v2(scribbu::id3v2_2_tag &v2)
{
  return process_tag(v2, create_frame_);
}

/// Process an ID3v2.3 tag
/*virtual*/
bool
set_pcnt_popm::process_v2(scribbu::id3v2_3_tag &v2)
{
  return process_tag(v2, create_frame_);
}

/// Process an ID3v2.4 tag
/*virtual*/
bool
set_pcnt_popm::process_v2(scribbu::id3v2_4_tag &v2)
{
  return process_tag(v2, create_frame_);
}

template <>
struct set_pcnt_popm::tag_traits<scribbu::id3v2_2_tag>
{
  typedef scribbu::id3v2_2_tag   tag_type;
  typedef scribbu::id3v2_2_frame frame_type;
  typedef scribbu::CNT           pcnt_type;
  typedef scribbu::POP           popm_type;

  static bool is_pcnt(const frame_type &F) {
    static const scribbu::frame_id3 PCNT("CNT");
    return F.id() == PCNT;
  }

  static bool is_popm(const frame_type &F) {
    static const scribbu::frame_id3 POPM("POP");
    return F.id() == POPM;
  }

  static pcnt_type make_pcnt(std::size_t n) {
    return scribbu::CNT(n);
  }

  static popm_type make_popm(const std::string &email,
                             unsigned char    rating,
                             std::size_t      n) {
    return scribbu::POP(email, rating, n);
  }
};

template <>
struct set_pcnt_popm::tag_traits<scribbu::id3v2_3_tag>
{
  typedef scribbu::id3v2_3_tag   tag_type;
  typedef scribbu::id3v2_3_frame frame_type;
  typedef scribbu::PCNT          pcnt_type;
  typedef scribbu::POPM          popm_type;

  static bool is_pcnt(const frame_type &F) {
    static const scribbu::frame_id4 PCNT("PCNT");
    return F.id() == PCNT;
  }

  static bool is_popm(const frame_type &F) {
    static const scribbu::frame_id4 POPM("POPM");
    return F.id() == POPM;
  }

  static pcnt_type make_pcnt(std::size_t n) {
    using namespace scribbu;
    return PCNT(n, id3v2_3_plus_frame::tag_alter_preservation::preserve,
                id3v2_3_plus_frame::file_alter_preservation::preserve,
                id3v2_3_plus_frame::read_only::clear, boost::none,
                boost::none, boost::none);
  }

  static popm_type make_popm(const std::string &email,
                             unsigned char    rating,
                             std::size_t      n) {
    using namespace scribbu;
    return POPM(email, rating, n,
                id3v2_3_plus_frame::tag_alter_preservation::preserve,
                id3v2_3_plus_frame::file_alter_preservation::preserve,
                id3v2_3_plus_frame::read_only::clear, boost::none,
                boost::none, boost::none);
  }
};

template <>
struct set_pcnt_popm::tag_traits<scribbu::id3v2_4_tag>
{
  typedef scribbu::id3v2_4_tag   tag_type;
  typedef scribbu::id3v2_4_frame frame_type;
  typedef scribbu::PCNT_2_4      pcnt_type;
  typedef scribbu::POPM_2_4      popm_type;

  static bool is_pcnt(const frame_type &F) {
    static const scribbu::frame_id4 PCNT("PCNT");
    return F.id() == PCNT;
  }

  static bool is_popm(const frame_type &F) {
    static const scribbu::frame_id4 POPM("POPM");
    return F.id() == POPM;
  }

  static pcnt_type make_pcnt(std::size_t n) {
    using namespace scribbu;
    return PCNT_2_4(n, id3v2_3_plus_frame::tag_alter_preservation::preserve,
                    id3v2_3_plus_frame::file_alter_preservation::preserve,
                    id3v2_3_plus_frame::read_only::clear, boost::none,
                    boost::none, false, false, boost::none);
  }

  static popm_type make_popm(const std::string &email,
                             unsigned char    rating,
                             std::size_t      n) {
    using namespace scribbu;
    return POPM_2_4(email, rating, n,
                    id3v2_3_plus_frame::tag_alter_preservation::preserve,
                    id3v2_3_plus_frame::file_alter_preservation::preserve,
                    id3v2_3_plus_frame::read_only::clear, boost::none,
                    boost::none, false, false, boost::none);
  }
};

namespace {

  /// Overload po::validate
  unsigned char
  rating_from_text(const std::string &text)
  {
    using namespace std;
    using namespace boost;

    // Now try to interpret `text' as an unsigned char...
    try {
      unsigned short rating = lexical_cast<unsigned short>(text);
      if (rating <= 255) {
        return (unsigned char)rating;
      }
    } catch (bad_lexical_cast&) {
    }

    // OK-- that didn't work. Let's match `text' against a regex
    // selecting a single character repeated between one & five times.
    smatch what;
    regex re1("[a-zA-Z@#%*+]{1,5}");
    if (! regex_match(text, what, re1)) {
      // Strange, but still we give up:
      throw po::error("Couldn't interpret '" + text + "' as a rating.");
    }

    // It is-- the length of `text' is the number of "stars"
    unsigned char rating;
    switch (text.length()) {
    case 1:
      rating = 1;
      break;
    case 2:
      rating = 64;
      break;
    case 3:
      rating = 128;
      break;
    case 4:
      rating = 196;
      break;
    default:
      rating = 255;
      break;
    }

    return rating;
  }


  //////////////////////////////////////////////////////////////////////////////
  //                                  handler                                 //
  //////////////////////////////////////////////////////////////////////////////

  /**
   * \brief `popm' sub-command handler
   *
   * \sa handler_type
   * \sa register_command
   *
   *
   * `scribbu popm' is a sub-command that can create & update PCNT & POPM
   * ID3v2 frames in a number of ways. This is the command handler for it
   * invoked directly from main.
   *
   *
   */

  int
  handle_popm(int argc, char **argv)
  {
    using namespace std;
    using namespace scribbu;

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
      ("info", po::bool_switch(), "Display help in Info format & exit");

    po::options_description xclopts("command-line only developer options");
    xclopts.add_options()
      ("man", po::bool_switch(), "Display the man page & exit");

    po::options_description opts("general options");
    opts.add_options()
      ("adjust-unsync,u", po::bool_switch(), "Update the unsynchronisation "
       "flag as needed on write (default is to never use it).")
      ("always-create-v2,a", po::bool_switch(), "Always create an ID3v2 tag"
       "with the relevant frame(s) for any file that has no ID3v2 tag")
      ("count,C", po::value<size_t>(), "Set the play count to this value")
      ("create-v2,c", po::bool_switch(), "Create an ID3v2.3 tag containing "
       "the relevant frames for any file that has an ID3v1 tag, but no v2")
      ("create-frame,A", po::bool_switch(), "Create a new frame when directed "
       "to process frames of a given sort and none are present in the tag")
      ("create-backups,b", po::bool_switch(), "Create backup copies of all "
       "files before modifying them.")
      ("dry-run,n", po::bool_switch(), "Don't do anything; just print what "
       "*would* be done")
      ("increment,i", po::value<size_t>(), "Increment the play count by this"
       "amount")
      ("owner,o", po::value<string>(), "Operate only on POPM frames with "
       "this owner, or specify the owner in case a POPM frame is being created")
      ("playcount-only,p", po::bool_switch(), "Operate only on PCNT frames")
      ("popularimeter-only,m", po::bool_switch(), "Operate only on POPM frames")
      ("rating,r", po::value<string>(), "Set the rating in popularimeter "
       "tag (s); either 0-255, or [a-zA-Z@#%*+]{1,5}")
      ("tag,t", po::value<vector<size_t>>(), "Zero-based index of the tag "
       "on which to operate; may be given more than once to select "
       "multiple tags")
      ("verbose,v", po::bool_switch(), "Produce more verbose output during "
       "operation");

    po::options_description xopts("hidden options");
    xopts.add_options()
      // Work around to https://svn.boost.org/trac/boost/ticket/8535
      ("arguments", po::value<std::vector<string>>()->required(), "one or more "
       "files or directories to be examined; if a directory is given, it will "
       "be searched recursively");

    po::options_description docopts;
    docopts.add(clopts).add(opts);

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

      maybe_handle_help(parsed, docopts, USAGE, "scribbu-popm",
                        "(scribbu) Invoking scribbu popm");

      po::store(parsed, vm);

      const map<string, string> ENV_OPTS {
        make_pair("SCRIBBU_ADJUST_UNSYNC", "adjust-unsync"),
        make_pair("SCRIBBU_CREATE", "create"),
        make_pair("SCRIBBU_CREATE_BACKUPS", "create-backups"),
        make_pair("SCRIBBU_DRY_RUN", "dry-run"),
        make_pair("SCRIBBU_OWNER", "owner"),
        make_pair("SCRIBBU_VERBOSE","verbose"),
      };
      parsed = po::parse_environment(opts, [&ENV_OPTS](const string &var) {
        auto p = ENV_OPTS.find(var);
        return ENV_OPTS.end() == p ? "" : p->second.c_str();
      });
      po::store(parsed, vm);

      // That's it-- the list of files and/or directories to be processed
      // should be waiting for us in 'arguments'...
      po::notify(vm);

      //////////////////////////////////////////////////////////////////////////
      //                     process our arguments                            //
      //////////////////////////////////////////////////////////////////////////

      bool adj_unsync         = vm["adjust-unsync"     ].as<bool>();
      bool always_create_v2   = vm["always-create-v2"  ].as<bool>();
      bool create_backups     = vm["create-backups"    ].as<bool>();
      bool create_frame       = vm["create-frame"      ].as<bool>();
      bool create_v2          = vm["create-v2"         ].as<bool>();
      bool dry_run            = vm["dry-run"           ].as<bool>();
      bool playcount_only     = vm["playcount-only"    ].as<bool>();
      bool popularimeter_only = vm["popularimeter-only"].as<bool>();
      bool verbose            = vm["verbose"           ].as<bool>();

      boost::optional<size_t> increment = boost::none;
      if (vm.count("increment")) {
        increment = vm["increment"].as<size_t>();
      }

      boost::optional<size_t> play_count = boost::none;
      if (vm.count("count")) {
        play_count = vm["count"].as<size_t>();
      }

      boost::optional<string> owner;
      if (vm.count("owner")) {
        owner = vm["owner"].as<string>();
        // I considered also checking the environment variables EMAIL &
        // DEBEMAIL, but that seemed too opaque for my tastes.
      }

      vector<size_t> tags;
      if (vm.count("tag")) {
        vector<size_t> V = vm["tag"].as<vector<size_t>>();
      }

      boost::optional<unsigned char> rating;
      if (vm.count("rating")) {
        rating = rating_from_text(vm["rating"].as<string>());
      }

      // Validate our options:

      // 1. At most one of `playcount_only' & `popularimeter_only' may be true
      typedef set_pcnt_popm::frame_policy frame_policy;
      if (playcount_only && popularimeter_only) {
        throw po::invalid_option_value("at most one of `playcount-only' & "
                                       "`popularimeter-only' may be given");
      }
      frame_policy fp = frame_policy::both;
      if (playcount_only) {
        fp = frame_policy::pcnt_only;
      } else if (popularimeter_only) {
        fp = frame_policy::popm_only;
      }

      // 2. `playcount' & `increment' may not both be given
      if (increment && play_count) {
        throw po::invalid_option_value("at most one of `playcount' & "
                                       "`increment' may be given");
      }
      typedef set_pcnt_popm::pc_policy pc_policy;
      size_t pc = 1;
      pc_policy pc_pol = pc_policy::increment;
      if (increment) {
        pc = *increment;
      } else if (play_count) {
        pc = *play_count;
        pc_pol = pc_policy::absolute;
      }

      // 3. at most one of `create-v2' and `always-create-v2' may be given
      if (create_v2 && always_create_v2) {
        throw po::invalid_option_value("at most one of `create-v2' and "
                                       "`always-create-v2' may be given");
      }

      typedef tagset_processor::v2_creation_policy v2_creation_policy;
      v2_creation_policy v2c = v2_creation_policy::never;
      if (always_create_v2) {
        v2c = v2_creation_policy::always;
      } else if (create_v2) {
        v2c = v2_creation_policy::when_v1_present;
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
      // successfully. Help-replated commands have been handled. Now we just
      // create our functor & hand-off to `process_dirent_args'.
      unique_ptr<set_pcnt_popm> pF;
      if (! tags.empty()) {
        pF.reset(new set_pcnt_popm(rating, fp, owner, pc, pc_pol, create_frame,
                                   v2c, tags.begin(), tags.end(), dry_run,
                                   verbose, adj_unsync, create_backups));
      } else {
        pF.reset(new set_pcnt_popm(rating, fp, owner, pc, pc_pol, create_frame,
                                   v2c, dry_run, verbose, adj_unsync,
                                   create_backups));
      }

      process_dirent_args(args.begin(), args.end(), *pF);

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

  register_command r("popm", handle_popm);

}
