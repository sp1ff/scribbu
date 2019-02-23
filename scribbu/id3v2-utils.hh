/**
 * \file id3v2-utils.hh
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
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

#ifndef ID3V2_UTILS_HH_INCLUDED
#define ID3V2_UTILS_HH_INCLUDED 1

#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/tbt-support.hh>

#include <memory>

namespace scribbu {

  /**
   * \brief Attempt to read an ID3v2 tag from \a is
   *
   *
   * \param is [in] An input stream from which the ID3v2 tag shall be read
   *
   * \return An ID3v2 tag, typed as pointer to id3v2_tag
   *
   * \throw An id3v2_tag::error on detecting an invalid tag
   *
   *
   * This function shall:
   *
   * - read the first ten bytes from \a is; if it's not an ID3v2 tag, restore
   *   the get ptr & so indicate to the caller (by returning a nil pointer)
   *
   * - if it *is*, construct an instance of the appropriate type and return it
   *   as a ptr to id3v2_tag.
   *
   *
   */

  std::unique_ptr<id3v2_tag> maybe_read_id3v2(std::istream &is);

  /**
   * \brief Read an ID3v2 tag from \a is
   *
   *
   * \param is [in] A standard input stream from which the tag shall be read
   *
   * \param idx [in] Index of the frame to be read
   *
   * \return a unique_ptr to an id3v2_2_tag, id3v2_3_tag, or id3v_2_4 tag, depending
   * on the ID3v2 version of the \a idx -th tag
   *
   * \throw An id3v2_tag::error on detecting an invalid tag
   *
   *
   * Unlike maybe_read_id3v2, this method will attempt the read the given tag
   * from \a is, and throw should it fail to do so.
   *
   *
   */

  std::unique_ptr<id3v2_tag> read_id3v2(std::istream &is, std::size_t idx = 0);

  /**
   * \brief Read all ID3v2 tags from an input stream
   *
   *
   * \param is [in] An input stream from which ID3v2 tags shall be read
   *
   * \param p [in,out] A forward output iterator to which unique_ptr-s to
   * id3v2_tag-s shall be copied
   *
   * \return p, after copy
   *
   * \throw id32_tag::error sub-class on encountering an invalid tag
   *
   *
   * There may be multiple ID3v2 tags at the start of a file; use this
   * method to read all possible & copy pointers to the resulting tags.
   *
   *
   */

  template <typename forward_output_iterator>
  forward_output_iterator read_all_id3v2(std::istream           &is,
                                         forward_output_iterator p) {

    std::unique_ptr<id3v2_tag> ptag = maybe_read_id3v2(is);
    while (ptag) {
      *p = std::move(ptag); p++;
      ptag = maybe_read_id3v2(is);
    }

    return p;
  }
  
  /**
   * \brief Compute the size, on disk, taken up by a sequence of ID3v2 tags
   *
   *
   * \param p0 [in] a forward input iterator marking the beginning of a range
   * of pointers to id3v2_tag
   *
   * \param p1 [in] a forward input iterator marking the one-past-the-end of a
   * range of pointers to id3v2_tag
   *
   * \param unsync [in] if true, compute the serialized size applying the
   * unsynchronisation scheme
   *
   * \return a pair of size_t; the first is the size, in bytes, needed
   * to serialize [p0, p1) & the second the total padding included
   * in [p0, p1)
   *
   *
   */
  template <typename forward_input_iterator>
  std::tuple<size_t, size_t>
  total_id3v2_size(forward_input_iterator p0,
                   forward_input_iterator p1,
                   bool unsync) {
    size_t sz = 0, pad = 0;
    for ( ; p0 != p1; ++p0) {
      sz  += (*p0)->size(unsync);
      pad += (*p0)->padding();
    }
    return std::make_tuple(sz, pad);
  }

  /**
   * \class processor
   *
   * \brief Functor for evaluating tag-based templates
   *
   *
   * I want to build a templating system where the template parameters are
   * filled in based on ID3v{1,2} tags, but I'm not sure what I want, yet.
   *
   * Examples:
   *
   * 1. "%A - %T%E"-- produce strings of the form "$artist -
   * $title$extension"; both tags are rendered verbatim in UTF-8, and the tag
   * value shall be determined by referring to the ID3v2 tag first, if present,
   * falling back to ID3v1 if not, and failing otherwise. Note that %E is the
   * original file extension (including the dot), and is *not* derived from any
   * tag.
   *
   * 2. "%(artist:encoding=cp1252&compress&sourcing=preferv1) -
   * %(title)(%extension:default=.mp3)"-- produce strings of the form "$artist
   * - $title$extension". The title shall be rendered in CP1252, with
   * whitespace compressed, and the ID3v1 tag shall be prefered to the
   * ID3v2. If the source file has no extension, ".mp3" shall be used.
   *
   * Options I can imagine:
   *
   * - compress whitespace
   * - different encodings
   * - different sourcing behaviour (e.g. prefer the ID3v1, or just fail if
   *   ID3v2 not present)
   * - provide a default value if the tag can't be evaluated
   *
   * 3. "%A - %T( \(track #%(track)\))?.mp3"-- produce strings of the form "$artist
   * - $title (track #$track).mp3" when the track number is available, falling
   * back to just "$artist - $title.mp3" else
   *
   * I wanted to use boost::spirit for this, but I spent the better part of
   * three days trying to figure out how the heck it worked. The docs, written
   * in form of "type X, type Y, type Z-- there, now it works!" were useless.
   *
   * EBNF:
   <code>

   template := term *

   term := text | replacement

   replacement := %(repl-name(:format)?) | %repl-abbrev | (term *)'?'

   repl-name := "artist" | "title" |...

   repl-abbrev := A | T |...

   format := option (&option) *

   option := ...

   </code>
   *
   *
   */

  class template_processor
  {
  public:
    /// Construct with the template in textual form
    template_processor(const std::string &templat);
    /// Given the path of the file, process our template
    std::string operator()(const boost::filesystem::path &pth) const;

  private:
    std::vector<std::shared_ptr<scribbu::tbt_support::term>> terms_;

  };

} // End namespace scribbu.


#endif // not ID3V2_UTILS_HH_INCLUDED
