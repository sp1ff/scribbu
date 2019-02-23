/**
 * \file framesv2.hh
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

#ifndef FRAMESV2_HH_INCLUDED
#define FRAMESV2_HH_INCLUDED 1
/**
 * \page scribbu_id3v2_frames ID3v2 Frames
 *
 * This module contains things common to all ID3v2 frames, including class
 * id3v2_frame.
 *
 * The structure of several tags remain constant through all revisions of the
 * specification; that structure has been factored out here.
 *
 * ID3v2 is an improvement on ID3v1 in that the character encoding is now
 * carried along with textual information. However, given the looseness of the
 * specification, as well as the spotty adherence to the spec found in the wild,
 * I think it best to not rely on that information too heavily.
 *
 * All ID3v2 text frames provide two accessors for their data:
 *
 *   - one that treats the data as octets & simply copies them into a
 *     caller-supplied buffer
 *
 *   - one that treats the data as text & attempts to convert it from the tag's
 *     internal encoding to the caller-requested encoding
 *
 * What is the tag's "internal" encoding? ID3v2 text frames provide the
 * following ways for the caller to specify:
 *
 *   - explicitly specify (i.e. pass a scribbu::encoding member)
 *
 *   - trust the tag's encoding field, and make a best effort attempt to
 *     interpret it (e.g. if an ID3v2.2 textual frame specifies "Unicode" the
 *     spec states UCS-2, but says nothing about endianess, and was written
 *     before UTF-16)
 *
 *   - use the system locale's character encoding
 *
 *
 */

#include <arpa/inet.h>

#include <algorithm>
#include <errno.h>
#include <iconv.h>
#include <initializer_list>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <vector>
#include <map>

#include <boost/exception/all.hpp>

#include <scribbu/charsets.hh>
#include <scribbu/scribbu.hh>

namespace scribbu {

  namespace detail {

    template <typename forward_input_iterator>
    forward_input_iterator
    find_trailing_null(std::size_t            code_unit,
                       forward_input_iterator p0,
                       forward_input_iterator p1)
    {
      // Walk [p0, p1) seeking the first sequence of `code_unit' zero bytes.
      std::size_t saw_nil = 0;
      for ( ; p0 < p1; p0 += code_unit) {
        if (1 == code_unit && 0 == *p0) {
          break;
        }
        else {
          unsigned char b0 = *p0, b1 = *(p0 + 1);
          if (0 == b0 && 0 == b1) {
            break;
          }
        }
      }

      return p0;
    }

    /// Return true iff [x, y] contain an MP3 sync word (i.e. the bit
    /// pattern %11111111 111xxxxx)
    bool is_false_sync(unsigned char x, unsigned char y);
    /// Return truee ifff [x, y] contain a bit pattern that would need
    /// "unsynchronisation" if the ID3v2 unsynchronisation scheme is being
    /// applied; this is subtly different than [x, y] being a false sync (\ref
    /// is_false_sync)-- this method will return true if [x, y] is a false sync
    /// *or* if [x, y] = [0xff, 0x00]
    bool needs_unsync(unsigned char x, unsigned char y);
    /// Count the number of "syncs" in [p0, p1). The meaning of the term "sync"
    /// depends upon \a false_only; if that parameter is true, only false 
    /// syncs (i.e. MP3 sync words) will be counted, if false, all pairs
    /// requiring unsynchronisation will be counted
    template <typename forward_iterator>
    std::size_t count_syncs(forward_iterator p0, 
                            forward_iterator p1, 
                            bool false_only) {
      if (p0 == p1) return 0;
      std::size_t count = 0;
      for (forward_iterator p2 = p0 + 1; p2 < p1; ) {
        if (false_only) {
          if (is_false_sync(*p0++, *p2++)) ++count;
        }
        else {
          if (needs_unsync(*p0++, *p2++)) ++count;
        }
      }
      return count;
    }

    /// Count the number of "syncs" in a 32-bit word. The meaning of the term
    /// "sync" depends upon \a false_only; if that parameter is true, only false
    /// syncs (i.e. MP3 sync words) will be counted, if false, all pairs
    /// requiring unsynchronisation will be counted
    std::size_t count_syncs(std::uint32_t n, bool false_only = true);

    template <typename output_iterator,
              typename input_iterator>
    std::size_t unsynchronise(output_iterator pout,
                              input_iterator  p0,
                              input_iterator  p1)
    {
      using namespace std;

      ptrdiff_t cb = 0;
      for (auto p = p0; p != p1; ) {
        auto q = find(p, p1, 0xff);
        pout = copy_n(p, q - p, pout);
        cb += q - p;
        if (q != p1) {
          *pout++ = 0xff; ++cb; ++q;
          if (q != p1 && (0x00 == *q || (*q & 0xe0) == 0xe0)) {
            *pout++ = 0x00; ++cb;
          }
        }
        p = q;

      }

      return cb;
    }
    
  } // End namespace detail.

  /// Enumeration of expressiong a tri-state option: do not use Unicode,
  /// use Unicode, or use Unicode with Byte Order Marking
    enum class use_unicode {
      no, yes, with_bom
    };

  /// Enumeration for arguments that need to express one of the
  /// three ID3v2 versions of which scribbuy is aware
  enum class id3v2_version { v2, v3, v4 };

  /// ID3v2.2 identifier-- a simple UDT representing a three-character,
  /// ASCII-encoded frame ID for use in hashed collections
  class frame_id3
  {
  public:
    frame_id3()
    { id_[0] = id_[1] = id_[2] = 0; }
    frame_id3(unsigned char id0, unsigned char id1, unsigned char id2);
    frame_id3(const unsigned char id[3]);
    frame_id3(const char id[3]);

  public:
    bool experimental() const {
      return experimental_;
    }
    std::string as_string() const {
      return std::string(id_, id_ + 3);
    }
    template <typename forward_output_iterator>
    forward_output_iterator copy(forward_output_iterator p) const {
      *p++ = id_[0]; *p++ = id_[1]; *p++ = id_[2];
      return p;
    }
    bool null() const {
      return 0 == id_[0] && 0 == id_[1] && 0 == id_[2];
    }
    bool text_frame() const {
      return 'T' == id_[0];
    }
    std::size_t write(std::ostream &os) const {
      os.write(id_, 3);
      return 3;
    }

  private:
    bool experimental_;
    char id_[3];

  };

  bool operator==(const frame_id3 &lhs, const frame_id3 &rhs);
  inline bool operator!=(const frame_id3 &lhs, const frame_id3 &rhs) {
    return !(lhs == rhs);
  }

  /// ID3v2.3 & .4 identifier-- a simple UDT representing a four-character,
  /// ASCII-encoded frame ID for use in hashed collections
  class frame_id4
  {
  public:
    frame_id4()
    { id_[0] = id_[1] = id_[2] = id_[3] = 0; }
    frame_id4(unsigned char id0, unsigned char id1,
              unsigned char id2, unsigned char id3);
    frame_id4(const unsigned char id[4]);
    frame_id4(const char id[4]);

  public:
    bool experimental() const {
      return experimental_;
    }
    std::string as_string() const {
      return std::string(id_, id_ + 4);
    }
    template <typename forward_output_iterator>
    forward_output_iterator copy(forward_output_iterator p) const {
      *p++ = id_[0]; *p++ = id_[1]; *p++ = id_[2]; *p++ = id_[3];
      return p;
    }
    bool null() const {
      return 0 == id_[0] && 0 == id_[1] && 0 == id_[2] && 0 == id_[3];
    }
    bool text_frame() const {
      return 'T' == id_[0];
    }
    std::size_t write(std::ostream &os) const {
      os.write(id_, 4);
      return 4;
    }

  private:
    bool experimental_;
    char id_[4];

  };

  bool operator==(const frame_id4 &lhs, const frame_id4 &rhs);
  inline bool operator!=(const frame_id4 &lhs, const frame_id4 &rhs) {
    return !(lhs == rhs);
  }

  std::ostream& operator<<(std::ostream &os, const scribbu::frame_id3 &x);
  std::ostream& operator<<(std::ostream &os, const scribbu::frame_id4 &x);
}

namespace std {

  template <>
  struct hash<scribbu::frame_id3>
  {
    std::size_t operator()(const scribbu::frame_id3 &x) const;
  };

  template <>
  struct hash<scribbu::frame_id4>
  {
    std::size_t operator()(const scribbu::frame_id4 &x) const;
  };

}

namespace scribbu {

  /**
   * \class id3v2_frame
   *
   * \brief Base class for all ID3v2 frames
   *
   *
   * All ID3v2 frames share two attributes: size and the "experimental"
   * flag. It also defines a core interface through which clients can
   * manipulate ID3v2 frames regardless of version.
   *
   *
   */

  class id3v2_frame {

  public:

    id3v2_frame(bool experimental): experimental_(experimental), dirty_(true)
    { }
    virtual ~id3v2_frame()
    { }

    /// Returns true if this is an experimental frame in that the ID begins
    /// with 'X', 'Y', or 'Z'
    bool experimental() const
    { return experimental_; }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const = 0;
    /// Return the number of bytes this frame will occupy when serialized to
    /// disk, including the header
    virtual std::size_t serialized_size(bool unsync) const = 0;
    /// Return zero if this frame would not contain false syncs if serialized in
    /// its present state; else return the number of false sync it would contain
    virtual std::size_t needs_unsynchronisation() const = 0;
    /// Serialize this frame to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised" will
    /// be updated accordingly)
    virtual std::size_t write(std::ostream &os, bool unsync) const = 0;

  protected:

    virtual bool is_dirty() const
    { return dirty_; }
    virtual void dirty(bool f) const
    { dirty_ = f; }

  private:
    bool experimental_;
    mutable bool dirty_;

  }; // End class id3v2_frame.

  /**
   * \brief Unique File Identifier
   *
   * While the identifier and frame header format changes across versions of the
   * spec, the structure of the unique file identifier does not. This suggests
   * modelling this behavior once in a base class. I also handle
   * version-specific differences through id3v2_frame subclasses for each
   * version (id3v2_2_frame, id3v2_3_frame & id3v2_4_frame), meaning that
   * there's no natural place in that inheritence hierarchy in which to locate
   * this class.
   *
   * I'm choosing to model this using multiple inheritence; the ID3v2.2 UFI
   * frame would inherit from both this class and id3v2_2_frame. My choice for
   * this over composition is driven by use case; I already have clients that
   * know they have an ID3v2 tag of some sort, and want all the UFI frames
   * contained therein (IOW, I want to model an "is-a" rather than a "has a"
   * relationship).
   *
   *
   */

  class unique_file_id {

  public:

    /// Construct with a range of bytes; [p0,p1) shall contain the frame, not
    /// including the header, after any resynchronisation, decompression,
    /// and/or decryption
    template <typename forward_input_iterator>
    unique_file_id(forward_input_iterator p0,
                   forward_input_iterator p1) {
      forward_input_iterator p = std::find(p0, p1, 0);
      // N.B. "If a $00 is found directly after the 'Frame size' the whole
      // frame should be ignored, and preferably be removed.", Sec 4.1 of
      // id3v2-00.txt.
      std::copy(p0, p, std::back_inserter(owner_));

      // "The 'Owner identifier' is then followed by the actual identifier,
      // which may be up to 64 bytes", Sec 4.1 of id3v2-00.txt.
      if (p != p1) {
        std::copy(p + 1, p1, std::back_inserter(id_));
      }
    }

  public:
    /// Owner identifier for this scheme
    template <typename forward_output_iterator>
    forward_output_iterator ownerb(forward_output_iterator p) const {
      return std::copy(owner_.begin(), owner_.end(), p);
    }

    /// Unique File Identifier
    template <typename forward_output_iterator>
    forward_output_iterator idb(forward_output_iterator pout) const {
      return std::copy(id_.begin(), id_.end(), pout);
    }

    /// Return the size, in bytes, of this structure, prior to
    /// desynchronisation, compression, and/or encryption exclusive of the
    /// header
    std::size_t size() const;
    std::size_t serialized_size(bool unsync) const;
    std::size_t needs_unsynchronisation() const;
    std::size_t write(std::ostream &os) const;

  private:
    /// Count the number of false syncs or required unsyncs
    std::size_t count_syncs(bool false_only) const;

  private:
    std::vector<unsigned char> owner_;
    std::vector<unsigned char> id_;

  }; // End class unique_file_id.

  /**
   * \brief Encryption method registration
   *
   *
   * While the identifier and frame header format changes across versions of the
   * spec, the structure of the encryption method does not. This class models
   * that, and is meant to be combined into version-specific id3v2_frame
   * subclasses through multiple inheritance
   *
   *.
   */

  class encryption_method {
  public:

    /// Construct with a range of bytes; [p0,p1) shall contain the frame, not
    /// including the header, after any resynchronisation, decompression,
    /// and/or decryption
    template <typename forward_input_iterator>
    encryption_method(forward_input_iterator p0,
                      forward_input_iterator p1) {
      forward_input_iterator p = std::find(p0, p1, 0);
      std::copy(p0, p, std::back_inserter(email_));
      if (p != p1) {
        ++p;
        if (p != p1)  {
          method_symbol_ = *p++;
          if (p != p1) {
            std::copy(p, p1, std::back_inserter(data_));
          }
        }
      }
    }

  public:

    template <typename forward_output_iterator>
    forward_output_iterator emailb(forward_output_iterator p) const {
      return std::copy(email_.begin(), email_.end(), p);
    }
    unsigned char method_symbol() const {
      return method_symbol_;
    }
    template <typename forward_output_iterator>
    forward_output_iterator datab(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

    /// Return the size, in bytes, of this structure, prior to
    /// desynchronisation, compression, and/or encryption exclusive of the
    /// header
    std::size_t size() const;
    std::size_t serialized_size(bool unsync) const;
    std::size_t needs_unsynchronisation() const;
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;

  private:
    std::vector<unsigned char> email_;
    unsigned char method_symbol_;
    std::vector<unsigned char> data_;

  }; // End class encryption_method.

  /**
   * \brief User-defined text
   *
   *
   * While the identifier and frame header format changes across versions of the
   * spec, the structure of the user-defined text frame does not. This class
   * models that structure, and is meant to be combined into version- specific
   * id3v2_frame sub-classes through multiple inheritance.
   *
   *
   */

  class user_defined_text {

  public:

    template <typename forward_input_iterator>
    user_defined_text(id3v2_version ver,
                      forward_input_iterator p0,
                      forward_input_iterator p1):
      cbnil_(1)
    {
      unicode_ = *p0++;

      if (id3v2_version::v2 == ver || id3v2_version::v3 == ver) {
        cbnil_ = unicode_ ? 2: 1;
      }
      else {
        cbnil_ = (0 == unicode_ || 3 == unicode_) ? 1 : 2;
      }

      // Find the first $00 or $00 00, depending on the encoding.
      forward_input_iterator p = detail::find_trailing_null(cbnil_, p0, p1);
      // [p0, p) has description (including the terminating null), [p, p1)
      // contains the string.
      std::copy(p0, p, std::back_inserter(description_));
      if (std::distance(p, p1) >= cbnil_) {
        p += cbnil_;
        std::copy(p, p1, std::back_inserter(text_));
      }
    }

    user_defined_text(id3v2_version ver,
                      const std::string &text,
                      encoding src,
                      use_unicode unicode,
                      const std::string &dsc = std::string());

  public:

    unsigned char unicode() const {
      return unicode_;
    }

    template <typename forward_output_iterator>
    forward_output_iterator descriptionb(forward_output_iterator p) const {
      return std::copy(description_.begin(), description_.end(), p);
    }

    template <typename forward_output_iterator>
    forward_output_iterator textb(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

    /// Return the size, in bytes, of this structure, prior to
    /// desynchronisation, compression, and/or encryption exclusive of the
    /// header
    std::size_t size() const;
    std::size_t serialized_size(bool unsync) const;
    std::size_t needs_unsynchronisation() const;
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;

  private:
    unsigned char cbnil_;
    unsigned char unicode_;
    std::vector<unsigned char> description_;
    std::vector<unsigned char> text_;

  }; // End class user_defined_text.

  /**
   * \brief Comment frames
   *
   *
   * While the identifier and frame header format changes across versions of the
   * spec, the structure of the comment frame does not. This class models that
   * structure, and is meant to be combined into version-specific id3v2_frame
   * sub-classes through multiple inheritence.
   *
   *
   */

  class comments {

  public:

    /// Construct from a raw buffer (forward_input_iterator shall dereference
    /// to an unsigned char)
    template <typename forward_input_iterator>
    comments(id3v2_version ver,
             forward_input_iterator p0,
             forward_input_iterator p1):
      cbnil_(1)
    {
      if (p0 != p1) {

        unicode_ = *p0++;

        if (id3v2_version::v2 == ver || id3v2_version::v3 == ver) {
          cbnil_ = unicode_ ? 2 : 1;
        }
        else {
          cbnil_ = (0 == unicode_ || 3 == unicode_) ? 1 : 2;
        }

        if (std::distance(p0, p1) >= 3) {

          std::copy(p0, p0 + 3, lang_);
          p0 += 3;

          if (p0 != p1) {

            forward_input_iterator p =
              detail::find_trailing_null(cbnil_, p0, p1);
            std::copy(p0, p, std::back_inserter(description_));

            if (std::distance(p, p1) >= cbnil_) {
              p += cbnil_;
              std::copy(p, p1, std::back_inserter(text_));
            }
          }
        }
      }
    }

    /// Construct from arbitrary text
    comments(id3v2_version ver,
             language lang,
             const std::string &text,
             encoding src,
             use_unicode unicode,
             const std::string &dsc = std::string());

  public:

    unsigned char unicode() const {
      return unicode_;
    }

    template <typename forward_output_iterator>
    forward_output_iterator lang(forward_output_iterator p) const {
      *p++ = lang_[0];
      *p++ = lang_[1];
      *p++ = lang_[2];
      return p;
    }

    template <typename forward_output_iterator>
    forward_output_iterator descriptionb(forward_output_iterator p) const {
      return std::copy(description_.begin(), description_.end(), p);
    }

    template <typename forward_output_iterator>
    forward_output_iterator textb(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

    /// Return the size, in bytes, of this structure, prior to
    /// desynchronisation, compression, and/or encryption exclusive of the
    /// header
    std::size_t size() const;
    std::size_t serialized_size(bool unsync) const;
    std::size_t needs_unsynchronisation() const;
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;

  private:
    unsigned char cbnil_;
    unsigned char unicode_;
    unsigned char lang_[3];
    std::vector<unsigned char> description_;
    std::vector<unsigned char> text_;

  }; // End class comments.

  /**
   * \brief Play count
   *
   *
   * While the frame id and frame header format changes across versions of the
   * spec, the structure of the play count frame does not. This class models
   * that structure, and is meant to be combined into version-specific
   * id3v2_frame sub-classes through multiple inheritence.
   *
   *
   */

  class play_count {

  public:

    /// Construct from a raw buffer (forward_input_iterator shall dereference to
    /// an unsigned char)
    template <typename forward_input_iterator>
    play_count(forward_input_iterator p0,
               forward_input_iterator p1):
      counter_(p0, p1)
    { }
    /// Construct from an actual play count
    play_count(std::size_t n)
    { reset_counter(n); }


  public:

    template <typename forward_output_iterator>
    forward_output_iterator counterb(forward_output_iterator p) const {
      std::copy(counter_.begin(), counter_.end(), p);
    }

    std::size_t count() const;
    
    /// Set the count
    void count(std::size_t n)
    { reset_counter(n); }
    /// Increment the count by one
    void inc()
    { count(count() + 1); }

    /// Return the size, in bytes, of this structure, prior to
    /// desynchronisation, compression, and/or encryption exclusive of the
    /// header
    std::size_t size() const;
    /// Return the number of bytes this structure will occupy when serialized to
    /// disk
    std::size_t serialized_size(bool unsync) const;
    /// Return zero if this structure would not contain false syncs if
    /// serialized in its present state; else return the number of false sync it
    /// would contain
    std::size_t needs_unsynchronisation() const;
    /// Serialize this frame to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;
    void reset_counter(std::size_t n);

  private:
    std::vector<unsigned char> counter_;

  }; // End class play_count.

  /**
   * \brief Popularimeter
   *
   *
   * While the frame id and frame header format changes across versions of the
   * spec, the structure of the popularimeter frame does not. This class models
   * that structure, and is meant to be combined into version-specific
   * id3v2_frame sub-classes through multiple inheritence.
   *
   *
   */

  class popularimeter {
  public:

    /// Construct from a raw buffer; forward_input_iterator shall dereference to
    /// an unsigned char
    template <typename forward_input_iterator>
    popularimeter(forward_input_iterator p0,
                  forward_input_iterator p1)
    {
      forward_input_iterator p = std::find(p0, p1, 0);
      std::copy(p0, p, std::back_inserter(email_));
      ++p;
      rating_ = *p++;
      std::copy(p, p1, std::back_inserter(counter_));
    }
    /// Construct "from scratch"
    popularimeter(const std::string &own,
                  unsigned char rating,
                  std::size_t count): rating_(rating)
    {
      std::copy(own.begin(), own.end(), std::back_inserter(email_));
      reset_counter(count);
    }
    
  public:

    template <typename forward_output_iterator>
    forward_output_iterator counterb(forward_output_iterator p) const {
      return std::copy(counter_.begin(), counter_.end(), p);
    }

    template <typename forward_output_iterator>
    forward_output_iterator emailb(forward_output_iterator p) const {
      return std::copy(email_.begin(), email_.end(), p);
    }

    unsigned char rating() const {
      return rating_;
    }
    void rating(unsigned char r) {
      rating_ = r;
    }
    /// Retreive the play count
    std::size_t count() const;
    /// Set the play count
    void count(std::size_t n)
    { reset_counter(n); }
    /// Increment the play count by one
    void inc()
    { count(count() + 1); }

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    std::size_t size() const;
    /// Return the number of bytes this frame will occupy when serialized to
    /// disk, including the header
    std::size_t serialized_size(bool unsync) const;
    /// Return zero if this frame would not contain false syncs if serialized in
    /// its present state; else return the number of false sync it would contain
    std::size_t needs_unsynchronisation() const;
    /// Serialize this frame to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised" will
    /// be updated accordingly)
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;
    void reset_counter(std::size_t n);

  private:
    std::vector<unsigned char> email_;
    unsigned char rating_;
    std::vector<unsigned char> counter_;

  }; // End class popularimeter.

  /**
   * \brief Tag cloud
   *
   *
   * While the frame id and frame header format changes across versions of the
   * spec, the structure of the tag cloud frame does not. This class models that
   * structure, and is meant to be combined into version-specific id3v2_frame
   * sub-classes through multiple inheritence.
   *
   *
   */
  
  class tag_cloud
  {
  private:
    typedef std::map<std::string, std::set<std::string>> map_type;
    
  public:
    typedef map_type::value_type value_type;
    typedef map_type::const_iterator const_iterator;
    typedef map_type::iterator iterator;

  public:
    /// Construct from a raw buffer; forward_input_iterator shall dereference to
    /// an unsigned char
    template <typename forward_input_iterator>
    tag_cloud(forward_input_iterator p0, 
              forward_input_iterator p1)
    {
      using namespace std;

      ++p0; // version

      forward_input_iterator p = find(p0, p1, 0);
      copy(p0, p, back_inserter(own_));
      
      p0 = ++p;
      while (p0 != p1) {
      
        p = find(p0, p1, 0);
        string key;
        copy(p0, p, back_inserter(key));
      
        unsigned char buf[4];
        copy(p + 1, p + 5, buf);
        const uint32_t *pb = (const uint32_t*)(buf);
        uint32_t cb = ntohl(*pb);
      
        set<string> vals;
        p0 = p + 5;
        while (cb) {
          p = find(p0, p1, 0);
          string val;
          copy(p0, p, back_inserter(val));
          p0 = ++p;
          vals.insert(val);
          cb -= val.length() + 1;
        }
        
        tags_[key] = vals;
      
      }
    }
    /// Construct "from scratch"-- the tags can be initialized using brace init
    tag_cloud(const std::string &own, std::initializer_list<value_type> init):
      own_(own), tags_(init)
    { }
    /// Construct "from scratch"-- [p0, p1) will be used to initialize the
    /// tag cloud, so the value_type shall be pair<const string, set<string>>
    template <typename forward_input_iterator>
    tag_cloud(const std::string &own, 
              forward_input_iterator p0,
              forward_input_iterator p1):
      own_(own), tags_(p0, p1)
    { }
    /// Construct "from scratch"-- text shall be a query-string style
    /// representation of the tag cloud (i.e. that which is returned from 
    /// urlencoded())
    tag_cloud(const std::string &owner, const std::string &text);

  public:
    /// Retrive the owner of this tag cloud
    std::string owner() const
    { return own_; }
    /// Does a given key exist?
    bool has_key(const std::string &key) const
    { return 0 != tags_.count(key); }
    /// Does a given value exist for a given key?
    bool has_value(const std::string &key, const std::string &val) const;
    /// Add key with no values; if the key exists this will be a NOP-- a return
    /// value of false means the key already existed
    bool add_key(const std::string &key);
    /// Add a value to an existent key-- a false return value means the key and
    /// value already existed
    bool add_value(const std::string &key, const std::string &val);
    /// Re-set the tags
    void reset(std::initializer_list<value_type> init)
    { tags_ = init; }
    
    iterator begin()
    { return tags_.begin(); }
    const_iterator begin() const
    { return tags_.begin(); }
    iterator end()
    { return tags_.end(); }
    const_iterator end() const
    { return tags_.end(); }
    
    /// Return an URL-encoded representation of the tag cloud
    std::string urlencoded() const;
    /// Merge an URL-coded representation of some tags into the cloud
    void merge(const std::string &merge);
    /// Update the tag cloud
    void update(const std::string &merge);
    
    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    std::size_t size() const;
    /// Return the number of bytes this frame will occupy when serialized to
    /// disk, including the header
    std::size_t serialized_size(bool unsync) const;
    /// Return zero if this frame would not contain false syncs if serialized in
    /// its present state; else return the number of false sync it would contain
    std::size_t needs_unsynchronisation() const;
    /// Serialize this frame to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised" will
    /// be updated accordingly)
    std::size_t write(std::ostream &os) const;

  private:
    std::size_t count_syncs(bool false_only) const;
    void parse_to_map(const std::string &tags, map_type &M);

  private:
    std::string own_;
    map_type tags_;

  }; // End class tag_cloud.

} // End namespace scribbu.

#endif // not FRAMESV2_HH_INCLUDED
