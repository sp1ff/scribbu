/**
 * \file id3v1.hh
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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

#ifndef ID3V1_HH_INCLUDED
#define ID3V1_HH_INCLUDED 1

/**
 * \page scribbu_id3v1 ID3v1 Tags
 *
 * \sa scribbu_winamp_genres
 *
 *
 * \section scribbu_id3v1_discuss Discussion
 *
 * The ID3v1 tag is attributed to NamkraD (AKA Eric Kemp), in the program
 * Studio3 in 1996 \ref scribbu_id3v1_refs_1 "[1]", and quickly became the de
 * facto standard for storing metadata in MP3s. It is a block of 128 bytes
 * appended to the audio data in the following format:
 *
 \code

  offset length contents

      0    3    the ASCII text "TAG"
      3   30    song title/track name
     33   30    artist
     63   30    album
     93    4    the year expressed as ASCII text
     97   30    comment
    127    1    genre

  \endcode
  *
  * ID3v1 defined a set of genres denoted by numerical codes (see
  * \ref scribbu_winamp_genres "here"). Winamp extended that list, but support
  * for the extended Winamp list is not universal. In some cases, only the first
  * 80 genres are supported.
  *
  * AFAICT, ID3v1 is very loosely specified, being more a collection of
  * conventions. For instance, according to \ref scribbu_id3v1_refs_2 "[2]",
  * "The specification asks for all fields to be padded with null character
  * (ASCII 0). However, not all applications respect this (an example is WinAmp
  * which pads fields with <space>, ASCII 32)."
  *
  * Michael Mutschier observed that since the standard calls for padding the
  * fields with zero bytes, it's a good bet that readers will stop when they
  * encounter any NULL value. Therefore, if the second-to-last byte of a field
  * is zero, a value may be stored in the last byte. He specifically proposed
  * adding the album track in this way to the comment field
  * \ref scribbu_id3v1_refs_1 "[1]". This modification is known as ID3 v1.1.
  *
  * Here is an example ID3v1 tag:
  *
  \verbatim

   0000000 54 41 47 4c 6f 72 63 61 27 73 20 4e 6f 76 65 6e  >TAGLorca's Noven<
   0000020 61 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >a...............<
   0000040 00 54 68 65 20 50 6f 67 75 65 73 00 00 00 00 00  >.The Pogues.....<
   0000060 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 48  >...............H<
   0000100 65 6c 6c 27 73 20 44 69 74 63 68 20 5b 45 78 70  >ell's Ditch [Exp<
   0000120 61 6e 64 65 64 5d 20 28 55 53 20 56 65 31 39 39  >anded] (US Ve199<
   0000140 30 41 6d 61 7a 6f 6e 2e 63 6f 6d 20 53 6f 6e 67  >0Amazon.com Song<
   0000160 20 49 44 3a 20 32 30 33 35 35 38 32 35 00 05 ff  > ID: 20355825...<
   0000200

  \endverbatim
  *
  * Broken out:
  *
  \verbatim

   0000000 54 41 47                                         TAG
   0000003 4c 6f 72 63 61 27 73 20 4e 6f 76 65 6e 61 00 00  Lorca's Novena..
   0000013 00 00 00 00 00 00 00 00 00 00 00 00 00 00        ..............
   0000021 54 68 65 20 50 6f 67 75 65 73 00 00 00 00 00 00  The Pogues......
   0000031 00 00 00 00 00 00 00 00 00 00 00 00 00 00        ...............
   000003f 48 65 6c 6c 27 73 20 44 69 74 63 68 20 5b 45 78  Hell's Ditch [Ex
   000004f 70 61 6e 64 65 64 5d 20 28 55 53 20 56 65        panded] (US Ve
   000005d 31 39 39 30                                      1990
   0000061 41 6d 61 7a 6f 6e 2e 63 6f 6d 20 53 6f 6e 67 20  Amazon.com Song
   0000071 20 49 44 3a 20 32 30 33 35 35 38 32 35 00        ID: 20355825
   000007f 05                                               Track #5
   0000080 ff                                               no/unknown genre

  \endverbatim
  *
  * Notice that this is an ID3v1.1 tag, and that the implementation that wrote
  * this tag padded out each field with NULLs.
  *
  * The thirty byte limitation in the ID3v1 fields soon became apparent,
  * leading to the Enhanced, or Extended (ID3v1) Tag
  * \ref scribbu_id3v1_refs_3 "[3]". The ID3v1 Enhanced tag is even more loosely
  * specified than the ID3v1; it is formed by prepending an additional 227 bytes
  * to the ID3v1 tag (so the entire tag is a block of 355 bytes \em appended to
  * the audio data). The additional 227 bytes are laid out as follows:
  *
  \verbatim

  offset length contents

      0   4     the ASCII text "TAG+"
      4  60     song title/track name
     64  60     artist
    124  60     album
    184   1     speed
    185  30     genre
    215   6     start time
    221   6     end time

  \endverbatim
  *
  * For title, artist, and album, the value is written to the ID3v1 tag first,
  * and any text beyond thirty characters is written to the extended tag
  * (giving a total of 90 characters for these three fields).
  *
  * The "speed" field can be set to 1 (slow), 2 (medium), 3 (fast) or 4
  * (hardcore).
  *
  * The "genre" field is free-form text, and it's relationship to the ID3v1
  * genre field is unspecified. The start & end times are intended to refer to
  * the track being tagged, and are strings expressing timestamps in the form
  * "mmm:ss" (no further interpretation is given).
  *
  * If none of these fields are used, then the extended tag is
  * omitted. According to Wikipedia, the extended tag was never widely adopted
  * (significantly, neither XMMS nor Winamp support it).
  *
  *
  * \section scribbu_id3v1_charsets ID3v1 Character Encodings
  *
  * The ID3v1 \ref scribbu_id3v1_refs_1 "specification" makes no mention of
  * character encoding; the various fields are defined as fixed-length arrays of
  * characters, which to me suggests ASCII. However, as arrays of char, they are
  * in principle capable of containing text in any encoding whose code unit is
  * one octet, and there are certainly other character encodings to be found in
  * the wild (generally the Window ANSI code page in use on the computer on
  * which the tags were written).
  *
  * Furthermore, it is impossible to detect the encoding reliably. A BOM, when
  * present, is suggestive, but not dispositive, and in the case of the European
  * ISO-8859 code pages, a given byte sequence will likely have many valid
  * interpretations.
  *
  * Consequently, this class provides two accessors for each such field; one
  * which simply copies the constituent bytes into a buffer, and a second that
  * will attempt to perform an encoding conversion from the tag's native
  * encoding to whatever the caller desires. Furthermore, we depend on the
  * caller to *tell us* what the tag's internal encoding is:
  *
  * - if the caller says nothing, then ASCII is assumed
  *
  * - if the caller passes a member of the encoding enumeration, then that
  *   value will be used
  *
  * - if the caller passes boost::none, then the system locale will be used
  *
  * When writing text we use the identical hierarchy for the \em destination
  * encoding (i.e. if the caller says nothing, then we'll attempt to write
  * ASCII, if the caller passes a member of the encoding enum...).
  *
  *
  * \section scribbu_id3v1_refs References
  *
  * 1. \anchor scribbu_id3v1_refs_1 [1] unknown. ID3v1 http://id3.org/ID3v1
  * (updated September 1, 2019)
  *
  * 2. \anchor scribbu_id3v1_refs_2 [2] unknown. MPEG Audio Tag ID3v1
  * http://mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm#MPEGTAG (updated
  * September 1, 2019)
  *
  * 3. \anchor scribbu_id3v1_refs_3 [3] unknown. MP3 TAG & Enhanced TAG
  * description (english) Originally available online at
  * http://www.fortunecity.com/underworld/sonic/3/id3tag.html, now cached at
  * https://web.archive.org/web/20120310015458/http://www.fortunecity.com/underworld/sonic/3/id3tag.html
  *
  * 4. \anchor scribbu_id3v1_refs_4 [4] unknown. ID3v1
  * https://en.wikipedia.org/wiki/ID3#ID3v1 (updated September 1, 2019)
  *
  *
  */

#include <exception>

#include <boost/exception/all.hpp>
#include <boost/optional.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/errors.hh>
#include <scribbu/charsets.hh>

namespace scribbu {

  /**
   *
   * \class id3v1_tag
   *
   * \brief A class whose instances represent ID3v1 & ID3v1(.1) tags (enhanced
   * or not)
   *
   *
   * Class id3v1_tag doesn't attempt any interpretation of "artist", "comment",
   * and so forth. Instances are copies of what's in the tag, broken out by
   * field. So, for instance, "artist" is not represented as a string, but as a
   * block of 30 or 90 bytes. It is up to the caller to interpret it.
   *
   *
   * I can see a few situations when instantiating id3v1_tag:
   *
   * - you have an opaque block of data that *might* contain an ID3V1 tag: call
   *   process_id3v1, which will return null if there is not a tag at the start
   *   of the block
   *
   * - you have a block of data that you *know* contains an ID3V1 tag, but not
   *   whether it's extended or not: construct an id3v1_tag instance using a
   *   constructor overload that takes a block of bytes only
   *
   * - you have a block of data that you *know* contains an ID3V1 tag, and you
   *   know whether it's extended or not (perhaps because you've already peeked
   *   at the first three or four bytes): construct an id3v1_tag instance using
   *   a constructor overload that takes a block of bytes along with a boolean
   *   indicating whether it's extended or not
   *
   *
   */

  class id3v1_tag {

  public:

    class invalid_tag: public scribbu::error
    {
    public:
      invalid_tag()
      { }
      virtual const char * what() const noexcept(true);
    };

  public:
     /// Construct using an input stream known to be currently pointing at an
    /// ID3V1 tag; the implementation will determine whether the tag is V1 or
    /// V1.1, and whether the tag is enhanced or not. It will throw if \a is is
    /// not pointing to a valid tag
    id3v1_tag(std::istream &is);
    /// Construct using an input stream known to be currently pointing at an
    /// ID3V1 tag which is known a priori to be extended (or not); the
    /// implementation will determine whether the tag is V1 or V1.1.
    id3v1_tag(std::istream &is, bool enhanced);
    /// Construct "from scratch"-- all fields wil be zeroed
    id3v1_tag(bool v11, bool enh):
      extended_(enh), v1_1_(v11),
      album_(enh ? 90 : 30),
      artist_(enh ? 90 : 30),
      genre_(0),
      ext_genre_(enh ? 30 : 0),
      title_(enh ? 90 : 30)
    {
      if (v11) {
        comment_.resize(28);
      } else {
        comment_.resize(30);
      }

      std::fill(start_time_.data(), start_time_.data() + 6, 0);
      std::fill(end_time_.data(), end_time_.data() + 6, 0);
      std::fill(year_.data(), year_.data() + 4, 0);
    }

    static const boost::optional<encoding> DEF_SRC_ENCODING;
    static const encoding                  DEF_DST_ENCODING;
    static const on_no_encoding            DEF_ON_NO_ENCODING;
    static const size_t                    TEXT_SZ     = 30;
    static const size_t                    TEXT_SZ_ENH = 90;

  public:

    ////////////////////////////////////////////////////////////////////////////
    //                                 album                                  //
    ////////////////////////////////////////////////////////////////////////////

    /// retrieve the 'album' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator album(forward_output_iterator p) const
    {
      return std::copy(album_.begin(), album_.end(), p);
    }
    /// retrieve the 'album' field (encoded)
    template <typename string_type>
    string_type
    album(const boost::optional<encoding> &src = DEF_SRC_ENCODING,
          encoding dst = DEF_DST_ENCODING,
          on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    // NB. I'm assumiing the caller is working in UTF-8 & that the tag's text is
    // in ASCII; had to call this set_ to avoid the above template from
    // shadowing it
    template <typename string_type>
    void set_album(const string_type &text,
                   encoding srcenc = DEF_DST_ENCODING,
                   boost::optional<encoding> dstenc = DEF_SRC_ENCODING,
                   bool add_bom = false,
                   on_no_encoding rsp = on_no_encoding::fail)
    {
      encoding dst = dstenc ? dstenc.get() : encoding_from_system_locale();
      album_ = convert_encoding<string_type>(text, srcenc, dst, add_bom, rsp);
      album_.resize(enhanced() ? TEXT_SZ_ENH : TEXT_SZ, 0);
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                 artist                                 //
    ////////////////////////////////////////////////////////////////////////////

    /// retrieve the 'artist' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator artist(forward_output_iterator p) const
    {
      return std::copy(artist_.begin(), artist_.end(), p);
    }
    /// retrieve the 'artist' field (encoded)
    template<typename string_type>
    string_type
    artist(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
           encoding dst = DEF_DST_ENCODING,
           on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // NB. I'm assumiing the caller is working in UTF-8 & that the tag's text is
    // in ASCII; had to call this set_ to avoid the above template from
    // shadowing it
    template <typename string_type>
    void set_artist(const string_type &text,
                    encoding srcenc = DEF_DST_ENCODING,
                    boost::optional<encoding> dstenc = DEF_SRC_ENCODING,
                    bool add_bom = false,
                    on_no_encoding rsp = on_no_encoding::fail)
    {
      encoding dst = dstenc ? dstenc.get() : encoding_from_system_locale();
      artist_ = convert_encoding<string_type>(text, srcenc, dst, add_bom, rsp);
      artist_.resize(enhanced() ? TEXT_SZ_ENH : TEXT_SZ, 0);
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                comment                                 //
    ////////////////////////////////////////////////////////////////////////////

    /// retrieve the 'comment' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator comment(forward_output_iterator p) const
    {
      return std::copy(comment_.begin(), comment_.end(), p);
    }
    /// retrieve the 'comment' field (encoded)
    template<typename string_type>
    string_type
    comment(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
            encoding dst = DEF_DST_ENCODING,
            on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // NB. I'm assumiing the caller is working in UTF-8 & that the tag's text is
    // in ASCII; had to call this set_ to avoid the above template from
    // shadowing it
    template <typename string_type>
    void set_comment(const string_type &text,
                     encoding srcenc = DEF_DST_ENCODING,
                     boost::optional<encoding> dstenc = DEF_SRC_ENCODING,
                     bool add_bom = false,
                     on_no_encoding rsp = on_no_encoding::fail)
    {
      encoding dst = dstenc ? dstenc.get() : encoding_from_system_locale();
      comment_ = convert_encoding(text, srcenc, dst, add_bom, rsp);
      comment_.resize(enhanced() ? TEXT_SZ_ENH : TEXT_SZ, 0);
    }

    ////////////////////////////////////////////////////////////////////////////
    //                               genre                                    //
    ////////////////////////////////////////////////////////////////////////////

    /// retrieve the one-byte genre field
    unsigned char genre() const
    {
      return genre_;
    }
    // set the one-byte genre field; named it set_ for consistency with other
    // setters
    void set_genre(unsigned char genre)
    {
      genre_ = genre;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                             enhanced genre                             //
    ////////////////////////////////////////////////////////////////////////////

    template <typename forward_output_iterator>
    forward_output_iterator enh_genre(forward_output_iterator p) const
    {
      return std::copy(ext_genre_.begin(), ext_genre_.end(), p);
    }
    template<typename string_type>
    string_type
    enh_genre(const boost::optional<encoding> &src = DEF_SRC_ENCODING,
              encoding dst = DEF_DST_ENCODING,
              on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // NB. I'm assumiing the caller is working in UTF-8 & that the tag's text is
    // in ASCII; had to call this set_ to avoid the above template from
    // shadowing it
    template <typename string_type>
    void set_enh_genre(const string_type &text,
                       encoding srcenc = DEF_DST_ENCODING,
                       boost::optional<encoding> dstenc = DEF_SRC_ENCODING,
                       bool add_bom = false,
                       on_no_encoding rsp = on_no_encoding::fail)
    {
      encoding dst = dstenc ? dstenc.get() : encoding_from_system_locale();
      ext_genre_ = convert_encoding<string_type>(text, srcenc, dst, add_bom,
                                                 rsp);
      ext_genre_.resize(TEXT_SZ, 0);
      extended_ = true;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                 speed                                  //
    ////////////////////////////////////////////////////////////////////////////

    std::pair<bool, unsigned char> speed() const
    {
      return std::make_pair(extended(), speed_);
    }
    void set_speed(unsigned char speed)
    {
      speed_ = speed;
      extended_ = true;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                               start time                               //
    ////////////////////////////////////////////////////////////////////////////

    template <typename forward_output_iterator>
    forward_output_iterator start_time(forward_output_iterator p) const
    {
      return std::copy(start_time_.begin(), start_time_.end(), p);
    }
    template<typename string_type>
    string_type
    start_time(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
               encoding dst = DEF_DST_ENCODING,
               on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // start & end times are six ASCII characters; they are supposed to be
    // in the form "mmm:ss", but beyond length we perform no validation
    void set_start_time(const char *text)
    {
      size_t i = 0, ntext = strlen(text);
      while (i < ntext && i < 7) {
        start_time_[i] = (char)text[i];
        ++i;
      }
      while (i < 7) {
        start_time_[i++] = 0;
      }
      extended_ = true;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                end time                                //
    ////////////////////////////////////////////////////////////////////////////

    template <typename forward_output_iterator>
    forward_output_iterator end_time(forward_output_iterator p) const
    {
      return std::copy(end_time_.begin(), end_time_.end(), p);
    }

    template<typename string_type>
    string_type
    end_time(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
             encoding dst = DEF_DST_ENCODING,
             on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // start & end times are six ASCII characters; they are supposed to be
    // in the form "mmm:ss", but beyond length we perform no validation
    void set_end_time(const char *text)
    {
      size_t i = 0, ntext = strlen(text);
      while (i < ntext && i < 7) {
        end_time_[i] = (char)text[i];
        ++i;
      }
      while (i < 7) {
        end_time_[i++] = 0;
      }
      extended_ = true;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                 title                                  //
    ////////////////////////////////////////////////////////////////////////////

    template <typename forward_output_iterator>
    forward_output_iterator title(forward_output_iterator p) const
    {
      return std::copy(title_.begin(), title_.end(), p);
    }

    template<typename string_type>
    string_type
    title(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
          encoding dst = DEF_DST_ENCODING,
          on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // NB. I'm assumiing the caller is working in UTF-8 & that the tag's text is
    // in ASCII; had to call this set_ to avoid the above template from
    // shadowing it
    template <typename string_type>
    void set_title(const string_type &text,
                   encoding srcenc = DEF_DST_ENCODING,
                   boost::optional<encoding> dstenc = DEF_SRC_ENCODING,
                   bool add_bom = false,
                   on_no_encoding rsp = on_no_encoding::fail)
    {
      encoding dst = dstenc ? dstenc.get() : encoding_from_system_locale();
      title_ = convert_encoding<string_type>(text, srcenc, dst, add_bom, rsp);
      title_.resize(enhanced() ? TEXT_SZ_ENH : TEXT_SZ, 0);
    }

    ////////////////////////////////////////////////////////////////////////////
    //                            track number                                //
    ////////////////////////////////////////////////////////////////////////////

    std::pair<bool, unsigned char> track_number() const
    {
      return std::make_pair(v1_1(), track_number_);
    }
    void set_track_number(unsigned char track)
    {
      track_number_ = track;
      v1_1_ = true;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                                  year                                  //
    ////////////////////////////////////////////////////////////////////////////

    template <typename forward_output_iterator>
    forward_output_iterator year(forward_output_iterator p) const
    {
      return std::copy(year_.begin(), year_.end(), p);
    }
    template<typename string_type>
    string_type year(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
                     encoding dst = DEF_DST_ENCODING,
                     on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    // year is four ASCII characters; they are supposed to be in the form
    // "YYYY", but beyond length we perform no validation
    void set_year(const char text[])
    {
      size_t i = 0;
      while (i < 4 && text[i]) {
        year_[i] = (char)text[i];
        ++i;
      }
      while (i < 4) {
        year_[i++] = 0;
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    //                             miscellaneous                              //
    ////////////////////////////////////////////////////////////////////////////

    bool enhanced() const
    {
      return extended();
    }

    bool extended() const
    {
      return extended_;
    }

    size_t size() const
    {
      return extended() ? ID3V1_EXT_TAG_SIZE : ID3V1_TAG_SIZE;
    }

    bool v1_1() const
    {
      return v1_1_;
    }

    std::ostream& write(std::ostream &os) const;

  private:

    static const std::size_t ID3V1_TAG_SIZE = 128U;
    static const std::size_t ID3V1_EXT_TAG_SIZE = 355U;

    void init_standard(unsigned char *p);
    void init_extended(unsigned char *p);

  private:

    bool                         extended_;
    bool                         v1_1_;
    std::vector<unsigned char>   album_;
    std::vector<unsigned char>   artist_;
    std::vector<unsigned char>   comment_;
    unsigned char                genre_;
    std::vector<unsigned char>   ext_genre_;
    std::array<unsigned char, 6> start_time_;
    std::array<unsigned char, 6> end_time_;
    unsigned char                speed_;
    std::vector<unsigned char>   title_;
    unsigned char                track_number_;
    std::array<unsigned char, 4> year_;


  };

  enum class id3_v1_tag_type { none, v_1, v_1_extended };

  struct id3v1_info {
    id3_v1_tag_type type_;
    std::streampos  start_;
    std::streampos  end_;
  };

  /**
   * \brief Test an input stream to see if there is an ID3v1 tag at the end
   *
   *
   * \param is [in,out] An input stream which may or may not have an ID3v1 tag
   * appended to other content; the read pointer will always be restored on
   * exit, regardless of the result
   *
   * \return An id3v1_tag_info struct describing the ID3v1 tag, if any (on
   * which more below)
   *
   *
   * This method returns an id3v1_info struct:
   *
   * - if \a is does \em not have an ID3v1 tag at the end, the \c type_ field
   *   will be set to none and the start_ & end_ fields are undefined
   *
   * - else, the \c type_ field will be set to v_1 or v_1_extended as
   *   appropriate, and start_ and end_ will be set to offsets within the files
   *   such that the tag is contained in the range [start_, end_)
   *
   *
   */

  id3v1_info ends_in_id3v1(std::istream &is);

  /**
   * \brief Attempt to read an ID3v1 tag from a stream
   *
   *
   * \param is [in,out] An input stream which may or may not have an ID3v1 tag
   * appended to other content; the read pointer will always be restored on
   * exit, regardless of the result. Note that this stream's get ptr may be
   * positioned anywhere on entry.
   *
   * \return a (possibly nil) unique_ptr to an id3v1_tag indicating that the
   * caller now owns the memory associated with it
   *
   *
   */

  std::unique_ptr<id3v1_tag> process_id3v1(std::istream &is);

  /**
   * \brief Remove the ID3v1 tag, if present
   *
   *
   * \param pth [in] path naming the file of interest
   *
   *
   * Summary says it all-- if \a pth ends in an ID3v1 tag, remove it.
   *
   *
   */

  void maybe_remove_id3v1(const std::filesystem::path &pth);

  /**
   * \brief Replace any ID3v1 tag that may be present
   *
   *
   * \param pth [in] path naming the file of interest
   *
   * \param tag [in] ID3v1 tag to be written
   *
   *
   * This method will replace theID3v1 tag at the end of \a pth (if any), and
   * replace it with \a tag.
   *
   *
   */

  void replace_id3v1(const std::filesystem::path &pth, const id3v1_tag &tag);

} // End namespace scribbu.

#endif // not ID3V1_HH_INCLUDED
