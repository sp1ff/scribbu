/**
 * \file scribbu.hh
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

#ifndef SCRIBBU_H_INCLUDED
#define SCRIBBU_H_INCLUDED 1
/**
 * \mainpage scribbu
 *
 * \section scribbu_intro Introduction
 *
 * scribbu is a C++ library & associated command-line tool for working with
 * <a href="http://id3.org">ID3</a> tags. It was born when I retired my last
 * Windows machine & could no longer use
 * <a href="https://en.wikipedia.org/wiki/Winamp">Winamp</a> to manage my library
 * of digital music. The scribbu library offers classes & methods for reading,
 * editing & writing ID3v1 & ID3v2 tags. The scribbu program provides assorted
 * sub-commands for working with ID3-tagged files (e.g. re-naming files based on
 * their tags), but its real power lies in its embedded Scheme
 * <a href="https://www.gnu.org/software/guile/">interpreter</a> in which scribbu
 * library features are exported as a Scheme module (on which more below).
 *
 * For introductory usage examples, see README.md in the project root directory.
 *
 *
 * \section scribbu_back ID3 tags
 *
 * \ref ref_04 "[4]" has the best overview of ID3 tags I've found, so I'm just
 * going to quote him:
 *
 * \par ID3
 * "MP3, also known as MPEG Audio Layer 3, is a format for storing compressed
 * audio data, designed by researchers at Fraunhofer IIS and standardized by the
 * Moving Picture Experts Group, a joint committee of the International
 * Organization for Standardization (ISO) and the International Electrotechnical
 * Commission (IEC). However, the MP3 format, by itself, defines only how to
 * store audio data. That's fine as long as all your MP3 files are managed by a
 * single application that can store metadata externally and keep track of which
 * metadata goes with which files. However, when people started passing around
 * individual MP3 files on the Internet, via file-sharing systems such as
 * Napster, they soon discovered they needed a way to embed metadata in the MP3
 * files themselves.
 *
 * \par
 * Because the MP3 standard was already codified and a fair bit of
 * software and hardware had already been written that knew how to decode the
 * existing MP3 format, any scheme for embedding information in an MP3 file
 * would have to be invisible to MP3 decoders. Enter ID3.
 *
 * \par
 * The original ID3 format, invented by programmer Eric Kemp, consisted
 * of 128 bytes stuck on the end of an MP3 file where it'd be ignored by most
 * MP3 software. It consisted of four 30-character fields, one each for the song
 * title, the album title, the artist name, and a comment; a four-byte year
 * field; and a one-byte genre code. Kemp provided standard meanings for the
 * first 80 genre codes. Nullsoft, the makers of Winamp, a popular MP3 player,
 * later supplemented this list with another 60 or so genres.
 *
 * \par
 * This format was easy to parse but obviously quite limited. It had no
 * way to encode names longer than 30 characters; it was limited to 256 genres,
 * and the meaning of the genre codes had to be agreed upon by all users of
 * ID3-aware software. There wasn't even a way to encode the CD track number of
 * a particular MP3 file until another programmer, Michael Mutschler, proposed
 * embedding the track number in the comment field, separated from the rest of
 * the comment by a null byte, so existing ID3 software, which tended to read up
 * to the first null in each of the text fields, would ignore it. Kemp's version
 * is now called ID3v1, and Mutschler's is ID3v1.1.
 *
 * \par
 * Limited as they were, the version 1 proposals were at least a
 * partial solution to the metadata problem, so they were adopted by many MP3
 * ripping programs (which had to put the ID3 tag into the MP3 files) and MP3
 * players (which would extract the information in the ID3 tag to display to the
 * user).1
 *
 * \par
 * By 1998, however, the limitations were really becoming annoying, and
 * a new group, led by Martin Nilsson, started work on a completely new tagging
 * scheme, which came to be called ID3v2. The ID3v2 format is extremely
 * flexible, allowing for many kinds of information to be included, with almost
 * no length limitations. It also takes advantage of certain details of the MP3
 * format to allow ID3v2 tags to be placed at the beginning of an MP3 file."
 *
 * You can still find the specs on-line at \ref ref_05 "[5]" and
 * \ref ref_06 "[6]". I've also downloaded them into the \c doc
 * sub-directory in the project root. You can find much more detail
 * at the pages for \ref scribbu_id3v1 "ID3v1" & \ref scribbu_id3v2 "ID3v2."
 *
 *
 * \section scribbu_references References
 *
 * 1. \anchor ref_01 [1] Alexandrescu, Andrei, Modern C++ Design,
 *   Addison-Wesley, Boston, 2001.
 *
 * 2. \anchor ref_02 [2] Langer, Angelika and Kreft, Klaus, Standard C++
 *    IOStrems and Locales. Addison-Wesley, Boston, 1999.
 *
 * 3. \anchor ref_03 [3] Meyer, Bertrand, Object Oriented Software
 *    Construction, Prentice Hall, 1997.
 *
 * 4. \anchor ref_04 [4] Seibel, Peter, Practical Common Lisp,
 *    Chapter 25. Practical: An ID3 Parser, 2005.
 *
 * 5. \anchor ref_05 [5] unknown. ID3v1. http://id3.org/ID3v1 (updated September
 * 1, 2019)
 *
 * 6. \anchor ref_06 [6] Nilsson, Martin.ID3 tag version 2
 * http://id3.org/id3v2-00 (updated September 1, 2019)
 *
 */

#include <array>
#include <iostream>
#include <optional>

#include <filesystem>
#include <boost/exception/exception.hpp>

#include <openssl/err.h>
#include <scribbu/errors.hh>

/// All exported functions, types &c should be in namespace scribbu
namespace scribbu {

  /// Library initialization
  void static_initialize();

  // Library cleanup
  void static_cleanup();

  /**
   * \class file_info
   *
   * \brief Information about a music track independent of tags or
   * content
   *
   *
   * Instantiate this class with a path naming a musical track; the
   * ctor will gather assorted data about the filesystem entity (as
   * opposed to ID3 tags, or information derived from the track data
   * itself).
   *
   *
   */

  class file_info {

  public:
    file_info()
    { }
    file_info(std::filesystem::path pth);

  public:
    /// Absolute path of the directory containing this file
    std::filesystem::path parent() const
    { return parent_; }
    /// Unqualified filename for this track
    std::filesystem::path filename() const
    { return filename_; }
    /// The size, in bytes, of this file
    std::uintmax_t size() const
    { return size_; }

  private:
    std::filesystem::path parent_;
    std::filesystem::path filename_;
    std::uintmax_t size_;
    std::optional<time_t> atime_;
    std::optional<time_t> mtime_;
    std::optional<time_t> ctime_;

  };

  /// Open a file & return a stream & a file_info instance describing that file
  std::pair<std::ifstream, file_info>
  open_file(std::filesystem::path pth);

  /// Error thrown on failure to open a file
  class file_open_error: public error
  {
  public:
    file_open_error(const std::string &name, int err): name_(name), errno_(err)
    { }
    virtual const char * what() const noexcept(true);
    std::string name() const
    { return name_; }
    int err() const
    { return errno_; }
  private:
    std::string name_;
    int errno_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /**
   * \brief Instantiate an ifstream & associate it with a file
   *
   *
   * \param name [in] name of the file to open
   *
   * \param mode [in] stream open mode; analagous to the basic_ifstream ctor,
   * defaults to ios_base::in, and will have that value or'd into the provided
   * mask (if any) when opening the file
   *
   * \return a newly instantiated std::ifstream associated with the file \a name
   *
   *
   * If you call basic_fstream's ctor with a filename, and for any reason it
   * can't open it, errno will be set, and the constructed basic_fstream
   * instance will have failbit set in it's error mask. This is unfortunate,
   * since it will cause subsequent operations to fail if you forget to check
   * the mask or is_open immediately after. IOW you have a zombie object.
   *
   * If you invoke the default ctor, then call open, you have the same
   * situation-- a basic_ifstream instance whose failbit is set, but no external
   * indication that the open failed.
   *
   * This is a convenienice function to construct an ifstream in a sane way: if
   * it returns successfully, the returned ifstream is ready for use. If it for
   * some reason cannot open the given file, it will throw an exception with
   * salient information. It makes use of the C++ 11 move constructor to simply
   * move the constructed instance into the return value.
   *
   *
   */

  std::ifstream
  open_ifstream(const std::string &name,
                std::ios_base::openmode mode = std::ios_base::in);

  class openssl_error: public scribbu::error
  {
  public:
    openssl_error(): err_(ERR_get_error())
    { }
    virtual const char * what() const noexcept;
  private:
    unsigned long err_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /// Locate the ID3v1 tag-- returns [here, there) where here is the current
  /// stream position and there is either the first byte of the ID3v1 tag or
  /// the one-past-the-end position, so that the track data is bracketed in
  /// [here, there)
  std::tuple<std::streampos, std::streampos>
  find_id3v1_tag(std::istream &is);

  /// 128 bits in bytes
  const size_t MD5_DIGEST_SIZE = 16;

  /**
   * \brief Compute the MD5 checksum over track data
   *
   *
   * \param is [in, out] istream whose get pointer is positioned at the
   * beginning of the audio data
   *
   * \param hint [in, opt] offset of either the ID3v1 tag if present, or the
   * EOF position, if known
   *
   * \return the 128-bit MD5 checksum
   *
   *
   * This function will locate the end of the track via find_id3v1_tag (or use
   * \a hint). On return, get ptr will be left either at the first byte of the
   * ID3v1 tag or just past the end of the file if there isn't one (eof bit will
   * be cleared, however).
   *
   */

  std::array<unsigned char, MD5_DIGEST_SIZE>
  compute_track_md5(std::istream &is, std::streampos hint = EOF);

  /**
   * \brief Container for information about the track data itself
   *
   *
   * Construct with an istream positioned just after any ID3v2 tags.  It will
   * scan the track data from the current position to the ID3v1 tag, if any (or
   * the end if no ID3v1 tag is present). After construction, assorted
   * information about the track may be retrieved.
   *
   * Note that this implementation is not particularly efficient: the entirety
   * of track data will be read into memory (in chunks), so as to compute an MD5
   * checksum (handy for verifying that tag-related operations haven't affected
   * the music data).  In addition, the track length is computed, in the worst
   * case, by decoding each MPEG audio frame header to determine its duration
   * (if there's a variable-rate encoding header in the first frame, the second
   * scan won't be necessary).
   *
   *
   */

  class track_data {

  public:
    /// mnemonic constant for the # of bytes in an MD5 checksum
    static const std::size_t DIGEST_SIZE = 16;
    /// Construct with an input stream positioned just after any ID3v2 tags; on
    /// return, the stream will be positioned to the first byte after all the
    /// track data has been consumed
    track_data(std::istream &is);
    template <typename forward_input_iterator>
    forward_input_iterator data(forward_input_iterator p0)
    { return std::copy(p0, p0 + DIGEST_SIZE, md5_.begin()); }
    template <typename forward_output_iterator>
    void get_md5(forward_output_iterator p) const
    { std::copy(md5_.begin(), md5_.end(), p); }
    /// Retrieve the song duration, in seconds
    std::optional<double> duration() const
    { return duration_secs_; }
    /// Retrieve the # of bytes in the track data
    std::size_t size() const
    { return size_bytes_; }

  private:
    std::array<unsigned char, DIGEST_SIZE> md5_;
    std::size_t size_bytes_;
    /// some tracks are corrupted; in such cases it is impossible to
    /// strictly compute the duration, so I use an optional to account
    /// for this; maybe in the future I'll add a 'verify' sub-command
    /// or something like that
    std::optional<double> duration_secs_;

  };

  /// Apply URL-encoding per RFC 3986
  std::string urlencode(const std::string &text);

  /// Remove URL-encoding per RFC 3986
  std::string urldecode(const std::string &text);

}

#endif // not SCRIBBU_H_INCLUDED
