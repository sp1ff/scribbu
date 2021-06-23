/**
 * \file framesv22.hh
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

#ifndef FRAMESV22_HH_INCLUDED
#define FRAMESV22_HH_INCLUDED 1
#include <scribbu/framesv2.hh>

#include <algorithm>
#include <memory>

#include <boost/optional/optional.hpp>

#include <scribbu/charsets.hh>

namespace scribbu {

  /**
   * \class id3v2_2_frame
   *
   * \brief Common interface shared by all ID3v2.2 frames
   *
   *
   * After the header, ID3v2.2 tags consist of one or more frames, each
   * satisfying the following layout:
   *
   * \code

     | field      | representation | bytes      |
     |------------+----------------+------------|
     | frame ID   | [A-Z0-9]{3}    |         3  |
     | frame size | 3*$xx          |         3  |
     | data       | $xx...         | frame size |

   * \endcode
   *
   * The identifier is a three-letter name made up of the upper-case characters
   * A-Z and the digits 0-9.  The ID is followed by a (non-sync-safe) three
   * byte unsigned integer giving he the size of the frame excluding the ID &
   * size (i.e.  total size - 6) after unsynchronisation.
   *
   *
   */

  class id3v2_2_frame: public id3v2_frame {

  protected:

    /// Index of the cached copy of this frame in serialized form
    static const unsigned char SERIALIZED = 0;
    /// Index of the cached copy of this frame in serialized form with
    /// unsynchronisation applied
    static const unsigned char SERIALIZED_WITH_UNSYNC = 1;

  public:

    id3v2_2_frame(const frame_id3 &id):
      id3v2_frame(id.experimental()), id_(id)
    { }
    id3v2_2_frame(unsigned char id0, unsigned char id1, unsigned char id2):
      id3v2_frame('X' == id0 || 'Y' == id0 || 'Z' == id0),
      id_(id0, id1, id2)
    { }
    id3v2_2_frame(const char id[3]):
      id3v2_frame('X' == id[0] || 'Y' == id[0] || 'Z' == id[0]),
      id_(id)
    { }
    virtual id3v2_2_frame* clone() const = 0;

  public:

    /// Return the three character ID for this frame
    frame_id3 id() const
    { return id_; }

    /**
     * \brief Convert ID3v2.2 encoded text to an arbitrary encoding
     *
     *
     * \param pbuf [in] Address of a buffer whose contents shall be interpreted
     * as text
     *
     * \param cbbuf [in] The size, in octets, of the buffer at \a pbuf
     *
     * \param srcenc [in] The ID3v2.2 encoding (on which more below) of the
     * text at \a pbuf
     *
     * \param dstenc [in] The encoding to be used in the returned string
     *
     * \param rsp [in] The desired response should \a pbuf contain a code point
     * that cannot be represented in \a dstenc
     *
     * \return The text at \a pbuf, encoded as \a dstenc
     *
     * \pre The code unit for character encoding \a dstenc is equal to the
     * character type of string_type
     *
     *
     * According to the ID3v2.2 spec:
     *
     * \remark If nothing else is said a string is represented as ISO-8859-1
     * [ISO-8859-1] characters in the range $20 - $FF. All unicode strings
     * [UNICODE] use 16-bit unicode 2.0 (ISO/IEC 10646-1:1993, UCS-2). All
     * numeric strings are always encoded as ISO-8859-1. Terminated strings are
     * terminated with $00 if encoded with ISO-8859-1 and $00 00 if encoded as
     * unicode. If nothing else is said newline character is forbidden. In
     * ISO-8859-1 a new line is represented, when allowed, with $0A
     * only. Frames that allow different types of text encoding have a text
     * encoding description byte directly after the frame size. If ISO-8859-1
     * is used this byte should be $00, if unicode is used it should be $01.
     *
     * There are several ID3v2.2 frames which contain text encoded as
     * described, including all text information frames, the comment frame,
     * user-defined text et al. Desiring to locate the logic for converting
     * from frame internal encoding to an arbitrary encoding, I've located the
     * logic here.
     *
     *
     */

    template <typename string_type>
    static
    string_type as_str(const unsigned char *pbuf,
                       std::size_t cbbuf,
                       unsigned char unicode,
                       scribbu::encoding dstenc,
                       scribbu::on_no_encoding rsp =
                         scribbu::on_no_encoding::fail,
                       const boost::optional<scribbu::encoding> &force =
                         boost::none);

    /// Return the number of bytes this frame will occupy when serialized to
    /// disk, including the header
    virtual std::size_t serialized_size(bool unsync, bool last_no_pad = false) const;
    /// Return zero if this tag would not contain false syncs if serialized in
    /// its present state; else return the number of false sync it would
    /// contain
    virtual std::size_t needs_unsynchronisation(bool last_no_pad = false) const;
    /// Serialize this tag to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised"
    /// will be updated accordingly)
    virtual std::size_t write(std::ostream &os, bool unsync) const;


    /// Return the number of bytes the header will occupy when serialized to
    /// disk
    std::size_t serialized_header_size(bool unsync) const;
    /// Return zero if this tag's header would not contain false syncs if
    /// serialized in its present state; else return the number of false sync
    /// it would contain
    std::size_t header_needs_unsynchronisation() const;
    /// Serialize this tag's header to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised"
    /// will be updated accordingly)
    std::size_t write_header(std::ostream &os, std::size_t cb_payload) const;

  protected:
    /// Mark this instance as dirty & invalidate the cache
    virtual void dirty(bool f) const;

  private:
    /// Refresh the cache, if dirty (otherwise, do nothing)
    void ensure_cached_data_is_fresh() const;
    /// Serialize this frame to \a os, exclusive of the frame header, as well
    /// as any compression, encryption or unsynchronisation; return the number
    /// of bytes written
    virtual std::size_t serialize(std::ostream &os) const = 0;
    /// Write a three-tuple while removing false syncs
    std::size_t unsynchronise_triplet(std::ostream &os, char buf[3]) const;

  private:
    frame_id3 id_;
    mutable std::size_t num_false_syncs_;
    mutable std::vector<unsigned char> cache_[2];

  }; // End class id3v2_2_frame.

  class unknown_id3v2_2_frame: public id3v2_2_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_2_frame(const frame_id3       &id,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      id3v2_2_frame(id),
      data_(p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_2_frame(unsigned char          id0,
                          unsigned char          id1,
                          unsigned char          id2,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_2_frame(frame_id3(id0, id1, id2), p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_2_frame(const char             id[3],
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_2_frame(frame_id3(id), p0, p1)
    { }

    virtual id3v2_2_frame* clone() const
    { return new unknown_id3v2_2_frame(*this); }

  public:

    /// Retrieve this frames payload, exclusive of identifier & size
    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const
    { return std::copy(data_.begin(), data_.end(), p); }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_2_frame.

  /// Unique File Identifier (cf. 4.1 of the ID3v2.2 spec)
  class UFI: public id3v2_2_frame, public unique_file_id {

  public:
    template <typename forward_input_iterator>
    UFI(forward_input_iterator p0, forward_input_iterator p1):
      id3v2_2_frame("UFI"),
      unique_file_id(p0, p1)
    { }

    virtual id3v2_2_frame* clone() const
    { return new UFI(*this); }

    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  public:

    template <typename string_type>
    string_type
    owner(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = encoding::ASCII) const
    {
      using namespace std;
      vector<unsigned char> buf;
      unique_file_id::ownerb(back_inserter(buf));
      return convert_encoding<string>(&(buf[0]), buf.size(), src.get(), dst, rsp);
    }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class UFI.

  /**
   * \class id3v2_2_text_frame
   *
   * \brief ID3 v2.2 text information frame
   *
   *
   * "The text information frames are the most important frames, containing
   * information like artist, album and more. There may only be one text
   * information frame of its kind in an tag. If the textstring is followed by
   * a termination ($00 (00)) all the following information should be ignored
   * and not be displayed." ["ID3 tag version 2", Sec. 4.2. "Text information
   * frames"]
   *
   \code

     +-------------+-------------------------------------+
     |      ID     | "T00" - "TZZ" , excluding "TXX" (1) |
     +-------------+-------------------------------------+
     |  frame size |         $xx xx xx (2)               |
     +-------------+-------------------------------------+
     |  encoding   |            $xx                      |
     +-------------+-------------------------------------+
     | information |         <textstring>                |
     +-------------+-------------------------------------+

     [1] described in 4.2.2.
     [2] not sync-safe

   \endcode
   *
   * The encoding field may be $00 or $01, the former denoting ISO-8859-1
   * characters in the range $20 - $FF and the latter 16-bit unicode 2.0
   * (ISO/IEC 10646-1:1993, UCS-2). If nothing else is said newline character
   * is forbidden. In ISO-8859-1 a new line is represented, when allowed, with
   * $0A only."
   *
   *
   *
   */

  class id3v2_2_text_frame: public id3v2_2_frame {

  public:

    template <typename forward_input_iterator>
    id3v2_2_text_frame(const frame_id3       &id,
                       forward_input_iterator p0,
                       forward_input_iterator p1):
      id3v2_2_frame(id)
    {
      unicode_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }

    template <typename forward_input_iterator>
    id3v2_2_text_frame(unsigned char id0,
                       unsigned char id1,
                       unsigned char id2,
                       forward_input_iterator p0,
                       forward_input_iterator p1):
      id3v2_2_text_frame(frame_id3(id0, id1, id2), p0, p1)
    { }

    template <typename forward_input_iterator>
    id3v2_2_text_frame(const char             id[3],
                       forward_input_iterator p0,
                       forward_input_iterator p1):
      id3v2_2_text_frame(frame_id3(id), p0, p1)
    { }

    /**
     * \brief Construct an arbitrary ID3v2.2 text frame
     *
     *
     * \param id [in] frame identfier; must begin with "T" and not be "TXX"
     *
     * \param text [in] an std basic_string containing the text to be encoded
     * into the text frame
     *
     * \param srcenc [in] the character encoding used to represent \a text
     *
     * \param ucs2 [in] the caller shall set this to true to encode the text as
     * UCS2, false to ecnoded it as ISO-8859-1
     *
     *
     */

    template <typename string_type>
    id3v2_2_text_frame(const frame_id3 &id,
                       const string_type &text,
                       encoding src,
                       bool add_bom = false,
                       on_no_encoding rsp = on_no_encoding::fail,
                       bool ucs2 = false) :
      id3v2_2_text_frame(id, ucs2, convert_encoding(text, src, ucs2 ? encoding::UCS_2LE :
                                                    encoding::ISO_8859_1, add_bom, rsp))
    { }

    virtual id3v2_2_frame* clone() const
    { return new id3v2_2_text_frame(*this); }

    static std::unique_ptr<id3v2_2_text_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    id3v2_2_text_frame(const frame_id3 &id,
                       bool unicode,
                       const std::vector<unsigned char> &text):
      id3v2_2_frame(id),
      unicode_(unicode),
      text_(text)
    { }

  public:

    /// Retrieve the raw textual data
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const
    { return std::copy(text_.begin(), text_.end(), p); }

    typedef scribbu::encoding encoding;
    typedef scribbu::on_no_encoding on_no_encoding;

    /**
     * \brief Retrieve tag text as a basic_string in an arbitrary encoding
     *
     *
     * Use cases:
     *
     *   1. Accept the frame's internal encoding, encode from that to UTF-8:
     *   the caller should have to supply no arguments in that case
     *
     *   2. Accept the frame's internal encoding, encode from that to some
     *   other encoding: the caller will have to specify that encoding
     *
     *   3. Override the frame's declared encoding, encode from that to UTF-8;
     *   the caller will have to specify the internal encoding
     *
     *   4. Both override the frame's internal encoding and encode to some
     *   other encoding scheme: the caller will have to specify both
     *
     *
     */

    template <typename string_type>
    string_type
    as_str(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src = boost::none) const
    {
      return id3v2_2_frame::as_str<string_type>(&(text_[0]), text_.size(),
                                                unicode(), dst, rsp, src);
    }

    unsigned char unicode() const
    { return unicode_; }

    void set(const std::string &text,
             encoding src = encoding::UTF_8,
             bool add_bom = false,
             on_no_encoding rsp = on_no_encoding::fail);

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  private:
    unsigned char unicode_;
    std::vector<unsigned char> text_;

  }; // End class id3v2_2_text_frame.

  /**
   * \class TXX
   *
   * \brief User-defined text information
   *
   *
   * "This frame is intended for one-string text information concerning the
   * audiofile in a similar way to the other "T"xx frames. The frame body
   * consists of a description of the string, represented as a terminated
   * string, followed by the actual string. There may be more than one "TXX"
   * frame in each tag, but only one with the same description." [ID3 tag
   * version 2, Sec. 4.2.2. "User defined text information frame"]
   *
   \code

     +-------------+-----------------------+
     |     ID      | "TXX"                 |
     +-------------+-----------------------+
     | frame size  | $xx xx xx (1)         |
     +-------------+-----------------------+
     |   encoding  | $xx                   |
     +-------------+-----------------------+
     | description | <textstring> $00 (00) |
     +-------------+-----------------------+
     |    value    | <textstring>          |
     +-------------+-----------------------+

     [1] not sync-sfae

   \endcode
   *
   *
   */

  class TXX: public id3v2_2_frame, public user_defined_text {

  public:
    template <typename forward_input_iterator>
    TXX(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("TXX"),
      user_defined_text(id3v2_version::v2, p0, p1)
    { }

    TXX(const std::string &text,
        encoding src,
        use_unicode unicode,
        const std::string &dsc = std::string()):
      id3v2_2_frame("TXX"),
      user_defined_text(id3v2_version::v2, text, src, unicode, dsc)
    { }

    virtual id3v2_2_frame* clone() const
    { return new TXX(*this); }

    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

    template <typename string_type>
    string_type
    description(encoding dst = encoding::UTF_8,
                on_no_encoding rsp = on_no_encoding::fail,
                const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      user_defined_text::descriptionb(back_inserter(buf));
      return id3v2_2_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

    template <typename string_type>
    string_type
    text(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      user_defined_text::textb(back_inserter(buf));
      return id3v2_2_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class TXX.

  /// Comments
  class COM: public id3v2_2_frame, public comments {

  public:
    template <typename forward_input_iterator>
    COM(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("COM"),
      comments(id3v2_version::v2, p0, p1)
    { }

    COM(language lang,
        const std::string &text,
        encoding src,
        use_unicode unicode,
        const std::string &dsc = std::string()):
      id3v2_2_frame("COM"),
      comments(id3v2_version::v2, lang, text, src, unicode, dsc)
    { }

    virtual id3v2_2_frame* clone() const
    { return new COM(*this); }

    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  public:

    template <typename string_type>
    string_type
    description(encoding dst = encoding::UTF_8,
                on_no_encoding rsp = on_no_encoding::fail,
                const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      comments::descriptionb(back_inserter(buf));
      return id3v2_2_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

    template <typename string_type>
    string_type
    text(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      comments::textb(back_inserter(buf));
      return id3v2_2_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class COM.

  /// ID3v2.2 play count
  class CNT: public id3v2_2_frame, public play_count {

  public:
    /// Construct from a raw buffer (forward_input_iterator shall dereferene
    /// to an unsigned char)
    template <typename forward_input_iterator>
    CNT(forward_input_iterator p0, forward_input_iterator p1):
      id3v2_2_frame("CNT"),
      play_count(p0, p1)
    { }
    /// Construct "from scratch"
    CNT(std::size_t n):
      id3v2_2_frame("CNT"),
      play_count(n)
    { }

    virtual id3v2_2_frame* clone() const
    { return new CNT(*this); }

    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  public:
    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class CNT.

  /// ID3v2.2 popularimeter
  class POP: public id3v2_2_frame, public popularimeter {

  public:
    /// Construct from a raw buffer (forward_input_iterator shall dereference
    /// to unsigned char)
    template <typename forward_input_iterator>
    POP(forward_input_iterator p0, forward_input_iterator p1):
      id3v2_2_frame("POP"),
      popularimeter(p0, p1)
    { }
    /// Construct "from scratch"
    POP(const std::string &own, unsigned char rating, std::size_t count):
      id3v2_2_frame("POP"),
      popularimeter(own, rating, count)
    { }

    virtual id3v2_2_frame* clone() const
    { return new POP(*this); }

    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  public:

    template <typename string_type>
    string_type
    email(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const encoding &src = encoding::ASCII) const
    {
      using namespace std;
      vector<unsigned char> buf;
      popularimeter::emailb(back_inserter(buf));
      return convert_encoding<string>(&(buf[0]), buf.size(), src, dst, rsp);
    }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class POP.

  /// ID2v2.2 tag cloud
  class XTG: public id3v2_2_frame, public tag_cloud {

  public:
    /// Construct from a serialized frame (forward_input_iterator shall
    /// dereference to unsigned char)
    template <typename forward_input_iterator>
    XTG(forward_input_iterator p0, forward_input_iterator p1):
      id3v2_2_frame("XTG"),
      tag_cloud(p0, p1)
    { }
    /// Construct "from scratch"
    XTG(const std::string &own, std::initializer_list<value_type> init):
      id3v2_2_frame("XTG"),
      tag_cloud(own, init)
    { }
    /// Construct "from scratch"-- [p0, p1) will be used to initialize the
    /// tag cloud, so the value_type shall be pair<const string, set<string>>
    template <typename forward_input_iterator>
    XTG(const std::string &own,
        forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("XTG"),
      tag_cloud(own, p0, p1)
    { }
    /// Construct "from scratch"-- text shall be a query-string style
    /// representation of the tag cloud (i.e. that which is returned from
    /// urlencoded())
    XTG(const std::string &owner, const std::string &text):
      id3v2_2_frame("XTG"),
      tag_cloud(owner, text)
    { }

    virtual id3v2_2_frame* clone() const
    { return new XTG(*this); }
    static std::unique_ptr<id3v2_2_frame>
    create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  public:
    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class XTG.

} // End namespace scribbu.

#endif // not FRAMESV22_HH_INCLUDED
