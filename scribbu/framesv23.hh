/**
 * \file framesv23.hh
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#ifndef FRAMESV23_HH_INCLUDED
#define FRAMESV23_HH_INCLUDED 1
#include <algorithm>
#include <memory>

#include <boost/optional.hpp>

#include <scribbu/charsets.hh>
#include <scribbu/framesv2.hh>

namespace scribbu {

  /**
   * \brief Represents an ID3v2.3 \em or ID3v2.4 frame
   *
   *
   * ID3v2.3 & .4 frames share enough structure that some functionality can be
   * hoisted up into a common sub-class.
   *
   * In the case of ID3v2.3 frames, after the header, and possibly after the
   * extended header, ID3v2.3 tags consist of one more frames, each satisfying
   * the following layout:
   *
   \code

     +--------+-------------------+
     |Frame ID|   $xx xx xx xx    |
     +--------+-------------------+
     |  Size  |   $yy yy yy yy    |
     +--------+-------------------+
     |  Flags |%abc00000 %ijk00000|
     +--------+-------------------+

     Frame ID: [A-Z0-9]{4}-- Identifiers beginning with "X", "Y" and "Z"
     are for experimental use

     Size: A non-sync-safe integer giving the size of the frame, in
     bytes, not including the header (i.e. total frame size - 10)

     Flags:

       - a: Tag alter preservation
       - b: File alter preservation
       - c: Read only
       - i: Compression
       - j: Encryption
       - k: Grouping identity

   \endcode
   *
   * ID3v2.4 frames have the following structure:
   *
   \code

     +--------+---------------------+
     |Frame ID|   $xx xx xx xx      |
     +--------+---------------------+
     | Size   | 4 * %0xxxxxxx       |
     +--------+---------------------+
     | Flags  | %0abc0000 %0h00kmnp |
     +--------+---------------------+

     Frame ID: [A-Z0-9]{4}-- Identifiers beginning with "X", "Y" and "Z"
     are for experimental use

     Size: A sync-safe integer giving the size of the frame, in
     bytes, not including the header (i.e. total frame size - 10)

     Flags:

       - a: Tag alter preservation
       - b: File alter preservation
       - c: Read only
       - h: Grouping identity
       - k: Compression
       - m: Encryption
       - n: Unsynchronisation
       - p: Data length indicator

   \endcode
   *
   * So while frames of either version detect these attributes differently,
   * they share:
   *
   *     - Tag alter preservation
   *     - File alter preservation
   *     - Read only
   *     - Compression
   *     - Encryption
   *     - Grouping identity
   *
   *
   */

  class id3v2_3_plus_frame: public id3v2_frame {

  public:

    // "This flag tells the software what to do with this frame if it is
    // unknown and the tag is altered in any way. This applies to all kinds of
    // alterations, including adding more padding and reordering the frames."
    // Sec 3.3.1
    enum class tag_alter_preservation {
      preserve, // 0: Frame should be preserved
      discard   // 1: Frame should be discarded
    };

    // "This flag tells the software what to do with this frame if it is
    // unknown and the file, excluding the tag, is altered. This does not apply
    // when the audio is completely replaced with other audio data." Sec 3.3.1
    enum class file_alter_preservation {
      preserve, // 0: Frame should be preserved
      discard   // 1: Frame should be discarded
    };

    // "This flag, if set, tells the software that the contents of this frame
    // is intended to be read only. Changing the contents might break
    // something, e.g. a signature. If the contents are changed, without
    // knowledge in why the frame was flagged read only and without taking the
    // proper means to compensate, e.g. recalculating the signature, the bit
    // should be cleared." Sec 3.3.1
    enum class read_only {
      clear,
      set
    };

    /// Convenience typedef-- values such as encryption method & group ID are
    /// represented by a single byte, when they're present.
    typedef boost::optional<unsigned char> id_type;

    /// Convenience typedef-- both ID3v2.3 & .4 frames optionally provide size
    /// information
    typedef boost::optional<std::size_t> opt_sz_type;

  protected:

    /// Index of the cached copy of this frame in serialized form with
    /// compression and/or ecnryption applied
    static const unsigned char SERIALIZED_WITH_CE = 0;
    /// Index of the cached copy of this frame in serialized form with
    /// compression, ecnryption and/or unsynchronisation applied
    static const unsigned char SERIALIZED_WITH_CEU = 1;

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             Construction                              //
    ///////////////////////////////////////////////////////////////////////////

    /// Construct with a frame ID et al.
    id3v2_3_plus_frame(const frame_id4        &id,
                       tag_alter_preservation  tap,
                       file_alter_preservation fap,
                       read_only               ro,
                       bool                    cmp,
                       const id_type          &enc,
                       const id_type          &gid):
      id3v2_frame(id.experimental()),
      id_        (id ),
      tap_       (tap),
      fap_       (fap),
      read_only_ (ro ),
      compressed_(cmp),
      enc_method_(enc),
      group_id_  (gid)
    { }

    /// Construct with an array of four chars et al.
    id3v2_3_plus_frame(const char              id[4],
                       // std::size_t             size,
                       tag_alter_preservation  tap,
                       file_alter_preservation fap,
                       read_only               ro,
                       bool                    cmp,
                       const id_type          &enc,
                       const id_type          &gid):
      id3v2_3_plus_frame(frame_id4(id), tap, fap, ro, cmp, enc, gid)
    { }

    /// Construct with four chars et al.
    id3v2_3_plus_frame(unsigned char           id0,
                       unsigned char           id1,
                       unsigned char           id2,
                       unsigned char           id3,
                       std::size_t             size,
                       tag_alter_preservation  tap,
                       file_alter_preservation fap,
                       read_only               ro,
                       bool                    cmp,
                       const id_type          &encmth,
                       const id_type          &gid):
      id3v2_3_plus_frame(frame_id4(id0, id1, id2, id3), tap, fap, ro, cmp, encmth, gid)
    { }

    /// Copy construct
    id3v2_3_plus_frame(const id3v2_3_plus_frame &that):
      id3v2_frame(that.id().experimental()),
      id_        (that.id()),
      tap_       (that.tag_alter_preserve()),
      fap_       (that.file_alter_preserve()),
      read_only_ (that.readonly()),
      compressed_(that.compressed()),
      enc_method_(that.enc_method_),
      group_id_  (that.group_id_)
    { }


  public:

    ///////////////////////////////////////////////////////////////////////////
    //                           Public Interface                            //
    ///////////////////////////////////////////////////////////////////////////

    frame_id4 id() const
    { return id_; }
    tag_alter_preservation tag_alter_preserve() const
    { return tap_; }
    file_alter_preservation file_alter_preserve() const
    { return fap_; }
    read_only readonly() const
    { return read_only_; }
    bool compressed() const
    { return compressed_; }
    bool encrypted() const
    { return bool(enc_method_); }
    bool grouped() const
    { return bool(group_id_); }

    /// Update a CRC-32 checksum using this frames serialized form
    std::uint32_t crc(std::uint32_t crc) const;

    /// Return the number of bytes this frame will occupy when serialized to
    /// disk, including the header
    virtual std::size_t serialized_size(bool unsync) const;
    /// Return zero if this tag would not contain false syncs if serialized in
    /// its present state; else return the number of false sync it would
    /// contain
    virtual std::size_t needs_unsynchronisation() const;
    /// Serialize this tag to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised"
    /// will be updated accordingly)
    virtual std::size_t write(std::ostream &os, bool unsync) const;

  protected:

    ///////////////////////////////////////////////////////////////////////////
    //                       Interface for Subclasses                        //
    ///////////////////////////////////////////////////////////////////////////

    bool tap() const
    { return tap_ == tag_alter_preservation::discard; }
    bool fap() const
    { return fap_ == file_alter_preservation::discard; }
    bool ro() const
    { return read_only_ == read_only::set; }
    virtual void dirty(bool f) const;
    unsigned char encmeth() const
    { return enc_method_.get(); }
    unsigned char gid() const
    { return group_id_.get(); }

    /// If the dirty flag is set, re-serialize & re-apply compression,
    /// encryption & unsynchronisation
    void ensure_cached_data_is_fresh() const;
    /// Serialize this frame to \a os, exclusive of the frame header, as well
    /// as any compression, encryption or unsynchronisation; return the number
    /// of bytes written
    virtual std::size_t serialize(std::ostream &os) const = 0;
    /// Serialize this tag's header to an output stream, including any special
    /// fields such as decompressed size, group id, &c
    virtual std::size_t write_header(std::ostream &os,
                                     std::size_t cb_payload,
                                     std::size_t dlind) const = 0;

  private:

    ///////////////////////////////////////////////////////////////////////////
    //                            Frame State                                //
    ///////////////////////////////////////////////////////////////////////////

    frame_id4               id_;
    tag_alter_preservation  tap_;
    file_alter_preservation fap_;
    read_only               read_only_;
    bool                    compressed_;
    id_type                 enc_method_;
    id_type                 group_id_;

    mutable std::size_t num_false_syncs_;
    mutable std::vector<unsigned char> cache_[2];

  }; // End class id3v2_3_plus_frame.

  /// Interface shared by all ID3v2.3 frames
  class id3v2_3_frame: public id3v2_3_plus_frame {

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             Construction                              //
    ///////////////////////////////////////////////////////////////////////////

    /// Construct with a frame ID
    id3v2_3_frame(const frame_id4        &id,
                  tag_alter_preservation  tap,
                  file_alter_preservation fap,
                  read_only               ro,
                  const id_type          &encmth,
                  const id_type          &gid,
                  const opt_sz_type      &decsz):
      id3v2_3_plus_frame(id, tap, fap, ro, (bool)decsz, encmth, gid)
    { }

    // construct with a four-character frame ID; forwards to the above ctor
    id3v2_3_frame(const char              id[4],
                  tag_alter_preservation  tap,
                  file_alter_preservation fap,
                  read_only               ro,
                  const id_type          &encmth,
                  const id_type          &gid,
                  const opt_sz_type      &decsz):
      id3v2_3_frame(frame_id4(id), tap, fap, ro, encmth, gid, decsz)
    { }

    virtual id3v2_3_frame* clone() const = 0;

    /// Convert ID3v2.3 encoded text to an arbitrary encoding
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

  protected:
    /// Serialize this tag's header to an output stream, including any special
    /// fields such as decompressed size, group id, &c
    virtual std::size_t write_header(std::ostream &os, std::size_t cb_payload,
                                     std::size_t dlind) const;

  }; // End class id3v2_3_frame.

  class unknown_id3v2_3_frame: public id3v2_3_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const frame_id4        &id,
                          tag_alter_preservation  tap,
                          file_alter_preservation fap,
                          read_only               ro,
                          const id_type          &encmth,
                          const id_type          &gid,
                          const opt_sz_type      &decsz,
                          forward_input_iterator  p0,
                          forward_input_iterator  p1):
      id3v2_3_frame(id, tap, fap, ro, encmth, gid, decsz),
      data_(p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(unsigned char           id0,
                          unsigned char           id1,
                          unsigned char           id2,
                          unsigned char           id3,
                          tag_alter_preservation  tap,
                          file_alter_preservation fap,
                          read_only               ro,
                          const id_type          &encmth,
                          const id_type          &gid,
                          const opt_sz_type      &decsz,
                          forward_input_iterator  p0,
                          forward_input_iterator  p1):
      unknown_id3v2_3_frame(frame_id4(id0, id1, id2, id3), tap, fap, ro, encmth,
                            gid, decsz, p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const char id[4],
                          tag_alter_preservation  tap,
                          file_alter_preservation fap,
                          read_only               ro,
                          const id_type          &encmth,
                          const id_type          &gid,
                          const opt_sz_type      &decsz,
                          forward_input_iterator  p0,
                          forward_input_iterator  p1):
      unknown_id3v2_3_frame(frame_id4(id), tap, fap, ro, encmth, gid,
                            decsz, p0, p1)
    { }

    virtual id3v2_3_frame* clone() const
    { return new unknown_id3v2_3_frame(*this); }

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const
    { return std::copy(data_.begin(), data_.end(), p); }

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_3_frame.

  class UFID: public id3v2_3_frame, public unique_file_id {

  public:
    template <typename forward_input_iterator>
    UFID(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type          &encmth,
         const id_type          &gid,
         const opt_sz_type      &decsz):
      id3v2_3_frame("UFID", tap, fap, ro, encmth, gid, decsz),
    unique_file_id(p0, p1)
    { }

    virtual id3v2_3_frame* clone() const
    { return new UFID(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4        &id,
           const unsigned char    *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type          &encmth,
           const id_type          &gid,
           const opt_sz_type      &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    template <typename string_type>
    string_type
    owner(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const encoding &src = encoding::ASCII) const
    {
      using namespace std;
      vector<unsigned char> buf;
      unique_file_id::ownerb(back_inserter(buf));
      return convert_encoding<string>(&(buf[0]), buf.size(), src, dst, rsp);
    }

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class UFID.

  class ENCR: public id3v2_3_frame, public encryption_method {

  public:
    template <typename forward_input_iterator>
    ENCR(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("ENCR", tap, fap, ro, encmth, gid, decsz),
      encryption_method(p0, p1)
    { }

    virtual id3v2_3_frame* clone() const
    { return new ENCR(*this); }

    static std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    template <typename string_type>
    string_type
    email(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const encoding &src = encoding::ASCII) const
    {
      using namespace std;
      vector<unsigned char> buf;
      encryption_method::emailb(back_inserter(buf));
      return convert_encoding<string>(&(buf[0]), buf.size(), src, dst, rsp);
    }

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class ENCR.

  /**
   * \class id3v2_3_text_frame
   *
   * \brief ID3 v2.3 text information frame
   *
   *
   * "The text information frames are the most important frames, containing
   * information like artist, album and more. There may only be one text
   * information frame of its kind in an tag. If the textstring is followed by
   * a termination ($00 (00)) all the following information should be ignored
   * and not be displayed. All text frame identifiers begin with "T". Only text
   * frame identifiers begin with "T", with the exception of the "TXXX"
   * frame. All the text information frames have the following format:" [ID3
   * tag version 2.3.0, Sec. 4.2. "Text information frames"]
   *
   \code

     +-------------+---------------------------------------+
     |      ID     | "T000" - "TZZZ" , excluding "TXX" (1) |
     +-------------+---------------------------------------+
     |  frame size |         $xx xx xx xx (2)              |
     +-------------+---------------------------------------+
     |  encoding   |              $xx                      |
     +-------------+---------------------------------------+
     | information |           <textstring>                |
     +-------------+---------------------------------------+

     [1] described in 4.2.2.
     [2] not sync-safe

   \endcode
   *
   * If the encoding byte is zero, then the string is represented as ISO-8859-1
   * characters in the range $20 - $FF. If it is one, then the string is
   * represented as Unicode; "All Unicode strings use 16-bit unicode 2.0
   * (ISO/IEC 10646-1:1993, UCS-2). Unicode strings must begin with the Unicode
   * BOM ($FF FE or $FE FF) to identify the byte order." [ID3 tag version
   * 2.3.0, Sec. 3.3. "ID3v2 frame overview"]
   *
   *
   */

  class id3v2_3_text_frame: public id3v2_3_frame {

  public:

    template <typename forward_input_iterator>
    id3v2_3_text_frame(const frame_id4         &id,
                       forward_input_iterator  p0,
                       forward_input_iterator  p1,
                       tag_alter_preservation  tap,
                       file_alter_preservation fap,
                       read_only               ro,
                       const id_type           &encmth,
                       const id_type           &gid,
                       const opt_sz_type       &decsz):
      id3v2_3_frame(id, tap, fap, ro, encmth, gid, decsz)
    {
      unicode_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }

    // forwards to the above ctor
    template <typename forward_input_iterator>
    id3v2_3_text_frame(const char              id[4],
                       forward_input_iterator  p0,
                       forward_input_iterator  p1,
                       tag_alter_preservation  tap,
                       file_alter_preservation fap,
                       read_only               ro,
                       const id_type           &encmth,
                       const id_type           &gid,
                       const opt_sz_type       &decsz):
      id3v2_3_text_frame(frame_id4(id), p0, p1, tap, fap, ro,
                         encmth, gid, decsz)
    { }

    /**
     * \brief Construct an arbitrary ID3v2.3 text frame (the prior two ctors
     * were written for deserialization)
     *
     *
     * \param id [in] frame identfier; must begin with "T" and not be "TXXX"
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
    id3v2_3_text_frame(const frame_id4        &id,
                       const string_type      &text,
                       encoding                src     = encoding::UTF_8,
                       bool                    add_bom = false,
                       on_no_encoding          rsp     = on_no_encoding::fail,
                       bool                    ucs2    = false,
                       tag_alter_preservation  tap     = tag_alter_preservation::preserve,
                       file_alter_preservation fap     = file_alter_preservation::preserve,
                       read_only               ro      = read_only::clear,
                       const id_type          &encmth  = boost::none,
                       const id_type          &gid     = boost::none,
                       const opt_sz_type      &decsz   = boost::none):
      id3v2_3_text_frame(id, ucs2,
                         convert_encoding(text, src,
                                          ucs2 ? encoding::UCS_2LE :
                                          encoding::ISO_8859_1, add_bom, rsp),
                         tap, fap, ro,
                         encmth, gid, decsz)
    { }

    virtual id3v2_3_frame* clone() const
    { return new id3v2_3_text_frame(*this); }

    static std::unique_ptr<id3v2_3_text_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  private:
    id3v2_3_text_frame(const frame_id4                  &id,
                       bool                             unicode,
                       const std::vector<unsigned char> &text,
                       tag_alter_preservation           tap,
                       file_alter_preservation          fap,
                       read_only                        ro,
                       const id_type                    &encmth,
                       const id_type                    &grid,
                       const opt_sz_type                &decsz):
      id3v2_3_frame(id, tap, fap, ro, encmth, grid, decsz),
      unicode_(unicode),
      text_(text)
    { }

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

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
      return id3v2_3_frame::as_str<string_type>(&(text_[0]), text_.size(),
                                                unicode(), dst, rsp, src);
    }

    unsigned char unicode() const
    { return unicode_; }

    void set(const std::string &text,
             encoding src = encoding::UTF_8,
             bool add_bom = false,
             on_no_encoding rsp = on_no_encoding::fail);

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  private:
    unsigned char unicode_;
    std::vector<unsigned char> text_;

  };

  /**
   * \class TXXX
   *
   * \brief User-defined text information; Cf. 4.2.2
   *
   *
   * "This frame is intended for one-string text information concerning the
   * audiofile in a similar way to the other "T"xx frames. The frame body
   * consists of a description of the string, represented as a terminated
   * string, followed by the actual string. There may be more than one "TXX"
   * frame in each tag, but only one with the same description." [ID3 tag
   * version 2.3.0 Sec. 4.2.2. "User defined text information frame"]
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

  class TXXX: public id3v2_3_frame, public user_defined_text {

  public:
    template <typename forward_input_iterator>
    TXXX(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("TXXX", tap, fap, ro, encmth, gid, decsz),
      user_defined_text(id3v2_version::v3, p0, p1)
    { }

    TXXX(const std::string      &text,
         encoding                src,
         use_unicode             unicode,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type          &encmth,
         const id_type          &gid,
         const std::string      &dsc = std::string()):
      id3v2_3_frame("TXXX", tap, fap, ro, encmth, gid, boost::none),
      user_defined_text(id3v2_version::v3, text, src, unicode, dsc)
    { }

    virtual id3v2_3_frame* clone() const
    { return new TXXX(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    template <typename string_type>
    string_type
    description(encoding dst = encoding::UTF_8,
                on_no_encoding rsp = on_no_encoding::fail,
                const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      user_defined_text::descriptionb(back_inserter(buf));
      return id3v2_3_frame::as_str<string_type>(&(buf[0]), buf.size(),
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
      return id3v2_3_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class TXXX.

  // Comments
  class COMM: public id3v2_3_frame, public comments {

  public:

    template <typename forward_input_iterator>
    COMM(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("COMM", tap, fap, ro, encmth, gid, decsz),
      comments(id3v2_version::v3, p0, p1)
    { }

    COMM(language                lang,
         const std::string      &text,
         encoding                src,
         use_unicode             unicode,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type          &encmth,
         const id_type          &gid,
         const std::string      &dsc = std::string()):
      id3v2_3_frame("COMM", tap, fap, ro, encmth, gid, boost::none),
      comments(id3v2_version::v2, lang, text, src, unicode, dsc)
    { }

    virtual id3v2_3_frame* clone() const
    { return new COMM(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

    template <typename string_type>
    string_type
    description(encoding dst = encoding::UTF_8,
                on_no_encoding rsp = on_no_encoding::fail,
                const boost::optional<encoding> &src = boost::none) const
    {
      using namespace std;
      vector<unsigned char> buf;
      comments::descriptionb(back_inserter(buf));
      return id3v2_3_frame::as_str<string_type>(&(buf[0]), buf.size(),
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
      return id3v2_3_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class COMM.

  // play count
  class PCNT: public id3v2_3_frame, public play_count {
  public:
    template <typename forward_input_iterator>
    PCNT(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("PCNT", tap, fap, ro, encmth, gid, decsz),
      play_count(p0, p1)
    { }

    PCNT(std::size_t count,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("PCNT", tap, fap, ro, encmth, gid, decsz),
      play_count(count)
    { }

    virtual id3v2_3_frame* clone() const
    { return new PCNT(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    // TODO(sp1ff): why?
    // std::size_t count() const
    // { return play_count::count(); }

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class PCNT.

  /// Popularimeter; cf. sec 4.18
  class POPM: public id3v2_3_frame, public popularimeter {

  public:

    template <typename forward_input_iterator>
    POPM(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("POPM", tap, fap, ro, encmth, gid, decsz),
      popularimeter(p0, p1)
    { }

    POPM(const std::string      &email,
         unsigned char           rating,
         std::size_t             count,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("POPM", tap, fap, ro, encmth, gid, decsz),
      popularimeter(email,rating, count)
    { }

    virtual id3v2_3_frame* clone() const
    { return new POPM(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

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

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class POPM.

  /// ID3v2.3 Tag cloud
  class XTAG: public id3v2_3_frame, public tag_cloud {

  public:

    template <typename forward_input_iterator>
    XTAG(forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("XTAG", tap, fap, ro, encmth, gid, decsz),
      tag_cloud(p0, p1)
    { }

    XTAG(const std::string &own,
         std::initializer_list<tag_cloud::value_type> init,
         tag_alter_preservation  tap = tag_alter_preservation::preserve,
         file_alter_preservation fap = file_alter_preservation::preserve,
         read_only               ro = read_only::clear,
         const id_type           &encmth = boost::none,
         const id_type           &gid = boost::none,
         const opt_sz_type       &decsz = boost::none):
      id3v2_3_frame("XTAG", tap, fap, ro, encmth, gid, decsz),
      tag_cloud(own, init)
    { }

    /// Construct "from scratch"-- [p0, p1) will be used to initialize the
    /// tag cloud, so the value_type shall be pair<const string, set<string>>
    template <typename forward_input_iterator>
    XTAG(const std::string      &own,
         forward_input_iterator  p0,
         forward_input_iterator  p1,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("XTAG", tap, fap, ro, encmth, gid, decsz),
      tag_cloud(own, p0, p1)
    { }

    /// Construct "from scratch"-- text shall be a query-string style
    /// representation of the tag cloud (i.e. that which is returned from
    /// urlencoded())
    XTAG(const std::string      &owner,
         const std::string      &text,
         tag_alter_preservation  tap,
         file_alter_preservation fap,
         read_only               ro,
         const id_type           &encmth,
         const id_type           &gid,
         const opt_sz_type       &decsz):
      id3v2_3_frame("XTAG", tap, fap, ro, encmth, gid, decsz),
      tag_cloud(owner, text)
    { }

    virtual id3v2_3_frame* clone() const
    { return new XTAG(*this); }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4         &id,
           const unsigned char     *p,
           std::size_t             cb,
           tag_alter_preservation  tap,
           file_alter_preservation fap,
           read_only               ro,
           const id_type           &encmth,
           const id_type           &gid,
           const opt_sz_type       &decsz);

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

  protected:
    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class POPM.

} // End namespace scribbu.

#endif // not FRAMESV23_HH_INCLUDED
