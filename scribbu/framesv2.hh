#ifndef FRAMESV2_HH_INCLUDED
#define FRAMESV2_HH_INCLUDED 1

#include <algorithm>
#include <boost/exception/all.hpp>
#include <errno.h>
#include <iconv.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <scribbu/scribbu.hh>

/**
 * \page scribbu_id3v2_frames "ID3v2 Frames"
 *
 * Things common to all ID3v2 frames, including class id3v2_frame. The design
 * philosophy here is to just parse the data, and leave interpretation to
 * callers.
 *
 * The structure of several tags remain contant through all revisions of the
 * specification; that structure has been factored out here.
 *
 * ID3v2 is an improvement on ID3v1 in that the character encoding is now
 * carried along with textual information. However, given the looseness of the
 * specification, as well as the spotty adherence to the spec found in the
 * wild, I think it best to not rely on that information too heavily.
 *
 * All ID3v2 text frames provide two accessors for their data:
 *
 *   - one that treats the data as octets & simply copies them into a
 *     caller-supplied buffer,
 *
 *   - and one that treats the data as text & attempts to convert it from the
 *     tag's internal encoding to the caller-requested encoding
 *
 * What is the tag's internal encoding; ID3v2 text frames provide the following
 * ways for the caller to specify:
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

namespace scribbu {

  namespace detail {

    template <typename forward_input_iterator>
    forward_input_iterator find_trailing_null(bool                   unicode,
                                              unsigned char          version,
                                              forward_input_iterator p0,
                                              forward_input_iterator p1) {

      std::size_t code_unit;

      if (2 == version || 3 == version) {
        if (0 == unicode) {
          code_unit = 1;
        }
        else {
          code_unit = 2;
        }
      }
      else {
        if (0 == unicode || 3 == unicode) {
          code_unit = 1;
        }
        else {
          code_unit = 2;
        }
      }

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

  } // End namespace detail.

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
   * \brief Base class for all ID3v2 frames
   *
   *
   * All ID3v2 frames share two attributes: size and the "experimental"
   * flag.
   *
   *
   */

  class id3v2_frame {

  public:
    /// N.B. the size parameter is the size of this frame, in bytes, *not*
    /// including the header, and after any resynchronisation, decompression,
    /// and/or decryption
    id3v2_frame(std::size_t size, bool experimental):
      size_(size),
      experimental_(experimental)
    { }
    virtual ~id3v2_frame()
    { }
    /// Returns true if this is an experimental frame in that the ID begins
    /// with 'X', 'Y', or 'Z'
    bool experimental() const {
      return experimental_;
    }
    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption
    std::size_t size() const {
      return size_;
    }
  private:
    std::size_t size_;
    bool experimental_;
  };

  /**
   * \brief Unique File Identifier
   *
   * While the identifier and frame header format changes across versions of
   * the spec, the structure of the unique file identifier does not. This
   * suggests modelling this behavior once in a base class. I also handle
   * version-specific differences through id3v2_frame subclasses for each
   * version (id3v2_2_frame, id3v2_3_frame & id3v2_4_frame), meaning that
   * there's no natural place in that inheritence hierarchy in which to locate
   * this class.
   *
   * I'm tentatively choosing to model this using multiple inheritence; the
   * ID3v2.2 UFI frame would inherit from both this class and id3v2_2_frame. My
   * choice for this over composition is driven by use case; I already have
   * clients that know they have an ID3v2 tag of some sort, and want all the
   * UFI frames contained therein (IOW, I want to model an "is-a" rather than a
   * "has a" relationship).
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
      std::copy(p + 1, p1, std::back_inserter(id_));
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

  private:
    std::vector<unsigned char> owner_;
    std::vector<unsigned char> id_;

  }; // End class unique_file_id.

  /**
   * \brief Encryption method registration
   *
   *
   * While the identifier and frame header format changes across versions of
   * the spec, the structure of the encryption method does not. This class
   * models that, and is meant to be aggregated into version-specific
   * id3v2_frame subclasses.
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
      ++p;
      method_symbol_ = *p++;
      std::copy(p, p1, std::back_inserter(data_));
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

  private:
    std::vector<unsigned char> email_;
    unsigned char method_symbol_;
    std::vector<unsigned char> data_;

  }; // End class encryption_method.

  /**
   * \brief User-defined text
   *
   *
   * While the identifier and frame header format changes across versions of
   * the spec, the structure of the user-defined text frame does not. This
   * suggests modelling this behavior once in a base class. I also handle
   * version-specific differences through id3v2_frame subclasses for each
   * version (id3v2_2_frame, id3v2_3_frame & id3v2_4_frame), meaning that
   * there's no natural place in that inheritence hierarchy in which to locate
   * this class.
   *
   * I'm tentatively choosing to model this using multiple inheritence; the
   * ID3v2.2 UDT frame would inherit from both this class and id3v2_2_frame. My
   * choice for this over composition is driven by use case; I already have
   * clients that know they have an ID3v2 tag of some sort, and want all the
   * UDT frames contained therein (IOW, I want to model an "is-a" rather than a
   * "has a" relationship).
   *
   *
   */

  class user_defined_text {

  public:

    template <typename forward_input_iterator>
    user_defined_text(unsigned char version,
                      forward_input_iterator p0,
                      forward_input_iterator p1)
    {
      unicode_ = *p0++;

      // Find the first $00 or $00 00, depending on the encoding.
      forward_input_iterator p = detail::find_trailing_null(unicode_, version,
                                                            p0, p1);
      // [p0, p) has description (including the terminating null), [p, p1)
      // contains the string.
      std::copy(p0, p, std::back_inserter(description_));
      std::size_t off = 1;
      if ((2 == version || 3 == version) && unicode_) {
        off = 2;
      }
      else if ((4 == version || (1 == unicode_ || 2 == unicode_))) {
        off = 2;
      }
      p += off;
      std::copy(p, p1, std::back_inserter(text_));
    }

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

  private:
    unsigned char unicode_;
    std::vector<unsigned char> description_;
    std::vector<unsigned char> text_;

  }; // End class user_defined_text.

  /**
   * \brief Comment frames
   *
   *
   * While the identifier and frame header format changes across versions of
   * the spec, the structure of the comment frame does not. This suggests
   * modelling this behavior once in a base class. I also handle
   * version-specific differences through id3v2_frame subclasses for each
   * version (id3v2_2_frame, id3v2_3_frame & id3v2_4_frame), meaning that
   * there's no natural place in that inheritence hierarchy in which to locate
   * this class.
   *
   * I'm tentatively choosing to model this using multiple inheritence; the
   * ID3v2.2 COM frame would inherit from both this class and id3v2_2_frame. My
   * choice for this over composition is driven by use case; I already have
   * clients that know they have an ID3v2 tag of some sort, and want all the
   * COM frames contained therein (IOW, I want to model an "is-a" rather than a
   * "has a" relationship).
   *
   *
   */

  class comments {

  public:

    template <typename forward_input_iterator>
    comments(unsigned char version,
             forward_input_iterator p0,
             forward_input_iterator p1)
    {
      unicode_ = *p0++;
      std::copy(p0, p0 + 3, lang_);
      p0 += 3;
      forward_input_iterator p = detail::find_trailing_null(unicode_, version,
                                                            p0, p1);
      std::copy(p0, p, std::back_inserter(description_));
      std::size_t off = 1;
      if ((2 == version || 3 == version) && unicode_) {
        off = 2;
      }
      else if ((4 == version || (1 == unicode_ || 2 == unicode_))) {
        off = 2;
      }
      p += off;
      std::copy(p, p1, std::back_inserter(text_));
    }

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

  private:
    unsigned char unicode_;
    unsigned char lang_[3];
    std::vector<unsigned char> description_;
    std::vector<unsigned char> text_;

  }; // End class comments.

  class play_count {

  public:

    template <typename forward_input_iterator>
    play_count(forward_input_iterator p0,
               forward_input_iterator p1):
      counter_(p0, p1)
    { }

  public:

    template <typename forward_output_iterator>
    forward_output_iterator counterb(forward_output_iterator p) const {
      std::copy(counter_.begin(), counter_.end(), p);
    }
    std::size_t count() const;

  private:
    std::vector<unsigned char> counter_;

  }; // End class play_count.

  class popularimeter {
  public:

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

  public:

    template <typename forward_output_iterator>
    forward_output_iterator counterb(forward_output_iterator p) const {
      std::copy(counter_.begin(), counter_.end(), p);
    }

    template <typename forward_output_iterator>
    forward_output_iterator emailb(forward_output_iterator p) const {
      std::copy(email_.begin(), email_.end(), p);
    }

    unsigned char rating() const {
      return rating_;
    }

  private:
    std::vector<unsigned char> email_;
    unsigned char rating_;
    std::vector<unsigned char> counter_;

  }; // End class popularimeter.

} // End namespace scribbu.

#endif // not FRAMESV2_HH_INCLUDED
