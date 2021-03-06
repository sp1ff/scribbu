/**
 * \file scribbu.hh
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
 * \section scribbu_discuss Discussion
 *
 * scribbu is currently a hobby project for working with ID3 tags. A few years
 * ago, I excised Windows from my life, and was therefore left without Winamp
 * for managing my mp3 library. There are packages out there,
 * <a href="http://taglib.org/">TagLib</a> e.g., but nothing that did everything
 * I wanted. Necessity being the mother of invention, I started on my own
 * solution.
 *
 * \subsection scribbu_discuss_gs Getting Started
 *
 * This project comes in two parts: a C++ library (\c libscribbu.{a,so},
 * generally installed into ${prefix}/${libdir}, or \c /usr/local/lib by
 * default) and a command built using the library (the \c scribbu executable,
 * generally installed into ${prefix}/${bindir}, or \c /usr/local/bin by
 * default). To get started using the \c scribbu program, start with the
 * Info manual: \c info \c scribbu. To get started coding against the
 * library, or hacking on the library itself, read on.
 *
 * \todo write the "Getting Started" section
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

#include <boost/filesystem.hpp>
#include <boost/exception/exception.hpp>

#include <openssl/err.h>

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
   * \todo add atime & mtime
   *
   * \todo add a checksum on the track data
   *
   * \todo add tagset locations (i.e. beginning & end of the ID3v tags in the
   * file
   *
   *
   */

  class file_info {

  public:
    file_info()
    { }
    file_info(boost::filesystem::path pth);

  public:
    /// Absolute path of the directory containing this file
    boost::filesystem::path parent() const
    { return parent_; }
    /// Unqualified filename for this track
    boost::filesystem::path filename() const
    { return filename_; }
    /// The size, in bytes, of this file
    std::uintmax_t size() const
    { return size_; }

  private:
    boost::filesystem::path parent_;
    boost::filesystem::path filename_;
    std::uintmax_t size_;

  };

  /**
   * \brief Open a file & return a stream & a file_info instance
   * describing that file
   *
   *
   */

  std::pair<std::unique_ptr<std::istream>, file_info>
  open_file(boost::filesystem::path pth);

  class openssl_error: public virtual boost::exception,
                       public virtual std::runtime_error
  {
  public:
    openssl_error(): std::runtime_error(""), err_(ERR_get_error())
    { }
    virtual const char * what() const noexcept;
  private:
    unsigned long err_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /**
   * \brief Container for information about the track data itself
   *
   *
   * Construct with an istream positioned just after any ID3v2 tags.  It will
   * scan the track data from the current position to the ID3v1 tag, if any (or
   * the end if no ID3v1 tag is present). After construction, assorted
   * information about the track may be retrieved.
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
    /// Retrieve the # of bytes in the track data
    std::size_t size() const
    { return size_; }

  private:
    std::array<unsigned char, DIGEST_SIZE> md5_;
    std::size_t size_;

  };

  /// Apply URL-encoding per RFC 3986
  std::string urlencode(const std::string &text);

  /// Remove URL-encoding per RFC 3986
  std::string urldecode(const std::string &text);

}

#endif // not SCRIBBU_H_INCLUDED
