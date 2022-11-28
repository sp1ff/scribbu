/**
 * \file command-utilities.hh
 *
 * \brief Assorted utilities for scribbu & its sub-commands
 *
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

#ifndef COMMAND_UTILITIES_HH_INCLUDED
#define COMMAND_UTILITIES_HH_INCLUDED 1

#include <deque>
#include <iostream>

#include <filesystem>
#include <boost/optional/optional.hpp>
#include <boost/program_options.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/tagset.hh>
#include <scribbu/winamp-genres.hh>

/// Like EXIT_SUCCESS & EXIT_FAILURE, but reflecting the convention that
/// calling with incorrect parameters shall exit with status code 2
const int EXIT_INCORRECT_USAGE = 2;

////////////////////////////////////////////////////////////////////////////
//                                   errors                               //
////////////////////////////////////////////////////////////////////////////

class file_not_found: public scribbu::error {
public:
  file_not_found(const std::filesystem::path &pth): pth_(pth)
  { }
  virtual const char * what() const noexcept(true);
private:
  std::filesystem::path pth_;
  mutable std::shared_ptr<std::string> pwhat_;
};

////////////////////////////////////////////////////////////////////////////
//                      types & methods related to help                   //
////////////////////////////////////////////////////////////////////////////

/// help can be requested at three levels
enum class help_level {
  none, regular, verbose
};

/// at level verbose, help can be requested as man or info
enum class verbose_flavor {
  man, info
};

/// Extract help_level from a set of parsed_options-- this has to be done
/// on a set of parsed_options because that's the only time we can get
/// to the raw tokens (e.g. to distinguish between "-h" & "--help").
std::tuple<help_level, boost::optional<verbose_flavor>>
help_level_for_parsed_opts(const boost::program_options::parsed_options &opts);

/// Print a usage message-- this is help at level `regular'
void
print_usage(std::ostream                              &os,
    const boost::program_options::options_description &opts,
    const std::string                                 &usage);

/// exec `man' with argument \a page
void
show_man_page(const std::string &page);

/// exec `info' with argument \a node
void
show_info_node(const std::string &node);

/**
 * \brief Provide stock handling for help requests
 *
 *
 * \param opts [in] const reference to parseed_options (this has to be done on a
 * set of parsed_options because that's the only time we can get to the raw
 * tokens (e.g. to distinguish between "-h" & "--help")
 *
 * \param page [in] name of this (sub-)command's man page
 *
 * \param node [in] name of this (sub-)command's info node
 *
 *
 * If this function returns, help was not requested-- the implementation
 * should carry out it's work. If help was requested at any level, this
 * function will never return.
 *
 *
 */

void
maybe_handle_help(const boost::program_options::parsed_options &opts,
    const boost::program_options::options_description          &dsc,
    const std::string                                          &usage,
    const std::string                                          &page,
    const std::string                                          &node);

////////////////////////////////////////////////////////////////////////////
//                           sub-command utilities                        //
////////////////////////////////////////////////////////////////////////////

/// Convenience typedef for a sub-command implementation
typedef
std::function<int(int argc, char **argv)>
handler_type;

/**
 * \brief Trivial struct for use as a static initializer
 *
 *
 * scribbu commands, having implemented their logic in the form of a
 * handler_type, can register themselves by constructing an instance
 * of this class like so:
 *
 \code

 namespace {

   int handler(blahblahblah...) {
     ...
   }

   register_command("<sub-command name>", handler);

 }
 \endcode
 *
 *
 */

struct register_command
{
  register_command(const std::string &s, handler_type f);
};

/// Return true if a sub-command named \a s has been registered
bool has_sub_command(const char*);

/// Retrieve a sub-command my name
handler_type get_sub_command(const char *);

namespace detail {
  typedef std::map<std::string, handler_type> handler_map;
  handler_map& get_handler_map();
}

/// Retrieve the names of all sub-commands
template <typename forward_output_iterator>
forward_output_iterator
get_sub_command_names(forward_output_iterator pout)
{
  const detail::handler_map &H = detail::get_handler_map();

  for (auto x: H) {
    *pout++ = x.first;
  }

  return pout;
}

////////////////////////////////////////////////////////////////////////////////
//                           class tagset_processor                           //
////////////////////////////////////////////////////////////////////////////////

/**
 * \class tagset_processor
 *
 * \brief ABC for compliant functors that process tagsets as part of sub-command
 * implementation
 *
 * \sa process_dirent_args
 *
 *
 * This class defines the interface to which functors passed to
 * process_dirent_args shall comply. IOW, to implement a sub-command that:
 *
 * - accepts one or more file-or-directory names defining the files on
 *   which to operate
 * - processes the tags therein in some way
 *
 * subclass tagset_processor, implement the pure virtuals, and in your
 * sub-command implementation, instantiate it & invoke process_dirent_args
 * supplying your instance as the third parameter.
 *
 *
 */

class tagset_processor
{
public:
  /// Policy governing when a new ID3v2 tag shall be created if there are none
  enum class v2_creation_policy { never, when_v1_present, always };
  /// Policy governing when a new ID3v1 tag shall be created if there are none
  enum class v1_creation_policy { never, when_v2_present, always };
  /// Policy governing whether the ID3v1 tag shall be processed when present
  enum class v1_tag_scope_policy { yes, no };
  /// Policy governing whether the ID3v2 tag shall be processed when present
  enum class v2_tag_scope_policy { all, none, some };
  /// If specifying that "some" ID3v2 tags should be processed, a range of
  /// indicies needs to be passed-- this enum can be passed to the ctor that
  // does *not* accept such a range
  enum class v2_simple_tag_scope_policy { all, none };

public:
  enum class unknown_op {
    create_v1_from_v2,
    create_v1,
    create_v2_from_v1,
    create_v2,
    process_v1,
    process_v2_2,
    process_v2_3,
    process_v2_4,
  };
  /// subclasses will throw in response to un-implemented virtuals being invoked
  class bad_operation: public scribbu::error {
  public:
    bad_operation(unknown_op op): op_(op)
    { }
    virtual const char * what() const noexcept(true);
  private:
    unknown_op op_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /// Thrown in response to an unknown ID3v2 version
  class bad_id3v2_version: public scribbu::error {
  public:
    bad_id3v2_version(unsigned char ver): ver_(ver)
    { }
    virtual const char * what() const noexcept(true);
  private:
    unsigned char ver_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

public:
  tagset_processor(v2_simple_tag_scope_policy v2sp,
                   v1_tag_scope_policy v1tsp,
                   v2_creation_policy v2c,
                   v1_creation_policy v1c,
                   bool dry_run,
                   bool verbose,
                   bool adjust_unsync,
                   bool create_backups):
    tagset_processor(v2_simple_tag_scope_policy::all == v2sp ?
                     v2_tag_scope_policy::all :
                     v2_tag_scope_policy::none,
                     v1tsp, v2c, v1c, dry_run, verbose, adjust_unsync,
                     create_backups)
  { }
  template <typename FII>
  tagset_processor(FII p0, FII p1,
                   v1_tag_scope_policy v1tsp,
                   v2_creation_policy v2c,
                   v1_creation_policy v1c,
                   bool dry_run,
                   bool verbose,
                   bool adjust_unsync,
                   bool create_backups) :
    tagset_processor(v2_tag_scope_policy::some, v1tsp, v2c, v1c, dry_run,
                     verbose, adjust_unsync, create_backups)
  {
    std::copy(p0, p1, std::back_inserter(v2_tags_));
  }

protected:
  /// Default ID3v2 tag padding, in bytes
  const size_t DEFAULT_PADDING = 1024;

private:
  // For internal use through delegation
  tagset_processor(v2_tag_scope_policy v2tsp,
                   v1_tag_scope_policy v1tsp,
                   v2_creation_policy v2c,
                   v1_creation_policy v1c,
                   bool dry_run,
                   bool verbose,
                   bool adjust_unsync,
                   bool create_backups);

public:
  bool dry_run() const
  { return dry_run_; }
  v1_creation_policy get_v1_creation_policy() const
  { return v1_creation_policy_; }
  v1_tag_scope_policy get_v1_tag_scope_policy() const
  { return v1_tag_scope_policy_; }
  v2_creation_policy get_v2_creation_policy () const
  { return v2_creation_policy_; }
  v2_tag_scope_policy get_v2_tag_scope_policy() const
  { return v2_tag_scope_policy_; }
  bool verbose() const
  { return verbose_; }

  /// Process the tagset contained in \a pth; in an implementation-specific
  /// manner
  void
  process_file(const std::filesystem::path &pth);

private:
  /**
   * \brief Perhaps write a tagset out to disk
   *
   *
   * \param dirty [in] the caller shall set this to true IFF the tagset is
   * different than that already on-disk in \a pth
   *
   * \parm p0 [in] a forward input iterator referencing the first element in a
   * range of unique_ptr-s to id3v2_tag-s to be written to \a pth
   *
   * \parm p1 [in] a forward input iterator referencing the "one-past-the-end"
   * element in a range of unique_ptr-s to id3v2_tag-s to be written to \a pth
   *
   * \param pv1 [in] a unique_ptr to an id3v1_tag that may or may not contain an
   * ID3v1 tag to be written to \a pth
   *
   * \param pth [in] filesystem path (absolute or relative to the present
   * working directory) to which the tagset shall be written
   *
   *
   * If dry_run_ is true, this method shall print a message on stdout describing
   * whether or not the tagset would be written to \a pth.
   *
   * Else, if \a dirty is true, this method shall write the tagset to \a
   * pth. The unsynchronisation flag will be set according to adj_unsync_. If
   * create_backups_ is true, the entire file shall be copied, and the new
   * tagset written (along with track data) to \a pth. If false, this method
   * will attempt to emplace the new tagset, if possible.
   *
   *
   */

  template <typename FII> // Forward Input Iterator => id3v2_tag
  void
  write_tagset(bool dirty, FII p0, FII p1,
               const std::unique_ptr<scribbu::id3v1_tag> &pv1,
               const std::filesystem::path &pth) const
  {
    using namespace std;
    using scribbu::apply_unsync;
    using scribbu::emplace_strategy;
    using scribbu::padding_strategy;
    if (dirty) {
      if (dry_run_) {
        cout << "the dry-run flag was given-- nothing written";
        if (create_backups_) {
          cout << " (with backup).";
        }
        cout << endl;
      } else {
        apply_unsync au = adjust_unsync_ ? apply_unsync::as_needed :
          apply_unsync::never;

        if (create_backups_) {
          replace_tagset_copy(pth, p0, p1, au);
        } else {
          maybe_emplace_tagset(pth, p0, p1, au,
                               emplace_strategy::reduce_padding_evenly,
                               padding_strategy::adjust_padding_evenly,
                               false);
        } // End if on `create_backups'.
        if (pv1) {
          replace_id3v1(pth, *pv1);
        }
      } // End if on `dry_run'.
    } else if (dry_run_) {
      cout << "the tagset wasn't updated, so nothing would be written" << endl;
    }
  }

public:
  /// Create a new ID3v1 tag when there are ID3v2 tags present
  virtual
  std::unique_ptr<scribbu::id3v1_tag>
  create_v1(const std::vector<std::unique_ptr<scribbu::id3v2_tag>> &v2) = 0;
  /// Create a new ID3v1 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v1_tag> create_v1() = 0;
  /// Create a new ID3v2 tag when there's an ID3v1 tag present
  virtual std::unique_ptr<scribbu::id3v2_tag>
  create_v2(const scribbu::id3v1_tag &v1) = 0;
  /// Create a new ID3v2 tag when there are no other tags present
  virtual std::unique_ptr<scribbu::id3v2_tag> create_v2() = 0;
  /// Process the ID3v1 tag
  virtual bool process_v1(scribbu::id3v1_tag &v1) = 0;
  /// Process an ID3v2.2 tag
  virtual bool process_v2(scribbu::id3v2_2_tag &v2) = 0;
  /// Process an ID3v2.3 tag
  virtual bool process_v2(scribbu::id3v2_3_tag &v2) = 0;
  /// Process an ID3v2.4 tag
  virtual bool process_v2(scribbu::id3v2_4_tag &v2) = 0;

private:
  v1_creation_policy v1_creation_policy_;
  v2_creation_policy v2_creation_policy_;
  v1_tag_scope_policy v1_tag_scope_policy_;
  v2_tag_scope_policy v2_tag_scope_policy_;
  bool dry_run_;
  bool verbose_;
  bool adjust_unsync_;
  bool create_backups_;
  std::deque<size_t> v2_tags_;

};


////////////////////////////////////////////////////////////////////////////////
//                  utilities for implementing sub-commands                   //
////////////////////////////////////////////////////////////////////////////////

namespace detail {

  template <typename FII> // Forward Input Iterator => id3v2_tag
  std::tuple<bool, std::string>
  find_frame(scribbu::id3v2_text_frames frm, FII p0, FII p1)
  {
    using namespace std;
    using namespace scribbu;
    string text;
    auto p = find_if(p0, p1, [&](const unique_ptr<id3v2_tag> &p) {
      text = p->text(frm);
      return !text.empty();
    });
    return make_tuple(p != p1, text);
  }

  template <typename FII> // Forward Input Iterator => id3v2_tag
  std::tuple<bool, std::array<char, 4>>
  find_year(FII p0, FII p1)
  {
    using namespace std;
    using namespace scribbu;
    std::array<char, 4> year = {{ 0, 0, 0, 0 }};
    string text;

    typedef typename FII::value_type T;
    auto p = find_if(p0, p1, [&](const T &p) {
      text = p->year();
      return 4 == text.length();
    });
    if (p != p1) {
      copy_n(text.begin(), 4, year.data());
    }
    return make_tuple(p != p1, year);
  }

  template <typename FII> // Forward Input Iterator => id3v2_tag*
  std::tuple<bool, std::string, unsigned char>
  find_content_type(FII p0, FII p1)
  {
    using namespace std;

    bool hit = false;
    size_t best_dl = SIZE_MAX;
    std::string best_text_match;
    unsigned char best_numeric_match;

    typedef typename FII::value_type T;
    for_each(p0, p1, [&](const T &p) {
      string text = p->content_type();
      if (text.length()) {
        size_t dl;
        string textual_genre;
        unsigned char numeric_genre;
        tie(textual_genre, numeric_genre, dl) = scribbu::match_winamp_genre(text);
        if (dl < best_dl) {
          hit = true;
          best_dl = dl;
          best_text_match = textual_genre;
          best_numeric_match = numeric_genre;
        }
      }
    });

    if (hit) {
      return make_tuple(true, best_text_match, best_numeric_match);
    } else {
      return make_tuple<bool, string, unsigned char>(false, "", 255);
    }
  }
}

/// Create a new ID3v1 tag from a collection of ID3v2 tags
template <typename FII> // Forward Input Iterator => unique_ptr<id3v2_tag>
std::unique_ptr<scribbu::id3v1_tag>
copy_id3_v2(FII p0, FII p1)
{
  using namespace std;
  using namespace scribbu;

  auto p = make_unique<id3v1_tag>(false, false);

  bool hit;
  string text;
  tie(hit, text) = ::detail::find_frame(id3v2_text_frames::talb, p0, p1);
  if (hit) p->set_album(text, encoding::UTF_8, encoding::UTF_8);
  tie(hit, text) = ::detail::find_frame(id3v2_text_frames::tpe1, p0, p1);
  if (hit) p->set_artist(text, encoding::UTF_8, encoding::UTF_8);
  tie(hit, text) = ::detail::find_frame(id3v2_text_frames::tit2, p0, p1);
  if (hit) p->set_title(text, encoding::UTF_8, encoding::UTF_8);

  array<char, 4> year;
  tie(hit, year) = ::detail::find_year(p0, p1);
  if (hit) p->set_year(year.data());

  unsigned char g;
  tie(hit, text, g) = ::detail::find_content_type(p0, p1);
  if (hit) p->set_genre(g);

  return p;
}

/// Create a new ID3v2 tag given an ID3v1 tag
std::unique_ptr<scribbu::id3v2_3_tag>
copy_id3_v1(const scribbu::id3v1_tag &v1);

template <typename FII,  // Forward Input Iterator => fs::path
          typename Functor>
void
process_dirent_args(FII p0, FII p1, Functor &F)
{
  using namespace std;

  using std::filesystem::directory_iterator;
  using std::filesystem::is_directory;
  using std::filesystem::path;

  for_each(p0, p1, [&F](const std::filesystem::path &pth) {
    if (is_directory(pth)) {
      process_dirent_args(directory_iterator(pth), directory_iterator(), F);
    } else if (is_regular_file(pth)) {
      F.process_file(pth);
    } else if (F.verbose()) {
      cout << "skipping non-file `" << pth << "'" << endl;
    }
  });
}

// Convert (argc, argv) style parameters to a collection of tokens
template <typename forward_output_iterator>
forward_output_iterator convert_tokens(int argc,
                                       char **argv,
                                       forward_output_iterator pout,
                                       bool skip_first = false)
{
  while (argc) {
    if (skip_first) {
      skip_first = false;
    } else {
      *pout++ = std::string(argv[0]);
    }
    --argc; ++argv;
  }
  return pout;
}

#endif // not COMMAND_UTILITIES_HH_INCLUDED
