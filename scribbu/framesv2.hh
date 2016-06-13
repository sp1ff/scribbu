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
 *
 */

namespace scribbu {

  class iconv_error: public virtual boost::exception,
                     public virtual std::runtime_error
  {
  public:
    iconv_error(int err):
      std::runtime_error(""), errno_(err)
    { }
    int get_errno() const
    { return errno_; }
    virtual const char * what() const noexcept;

  private:
    int errno_;
    mutable std::shared_ptr<std::string> pwhat_;

  };

  namespace detail {

    template <typename forward_input_iterator>
    forward_input_iterator find_trailing_null(bool                   unicode,
                                              forward_input_iterator p0,
                                              forward_input_iterator p1) {

      // Find the first $00 or $00 00, depending on the encoding.
      forward_input_iterator p;
      if (unicode) {

        bool saw_null = false;
        forward_input_iterator p2 = p0;
        for ( ; p0 < p1; ++p0) {
          if (saw_null && !*p0) {
            break;
          }
          else if (saw_null) {
            saw_null = false;
          }
          else if (!*p0) {
            saw_null = true;
          }
          p2 = p0;
        }

        if (p0 == p1) {
          p = p1;
        }
        else {
          p = p2;
        }

      } else {
        p = std::find(p0, p1, 0);
      }

      return p;
    }

    /// guard class for iconv descriptors
    class iconv_guard {
    public:
      iconv_guard(const char *tocode, const char *fromcode)
      {
        using std::stringstream;
        cd_ = iconv_open(tocode, fromcode);
        if ((iconv_t)-1 == cd_) {
          throw iconv_error(errno);
        }
      }
      ~iconv_guard() {
        if (-1 == iconv_close(cd_)) {
          throw iconv_error(errno);
        }
      }
      operator iconv_t() const {
        return cd_;
      }
    private:
      iconv_t cd_;
    }; // End class iconv_guard.

    // TODO: Re-design this API, once I figure out the details in all versions
    // of the ID3v2 spec
    std::string to_utf8(iconv_t              cd,
                        const unsigned char *pbuf,
                        std::size_t          cbbuf);

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
    frame_id4(unsigned char id0, unsigned char id1, unsigned char id2, unsigned char id3);
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

}

std::ostream& operator<<(std::ostream &os, const scribbu::frame_id3 &x);
std::ostream& operator<<(std::ostream &os, const scribbu::frame_id4 &x);

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
   * class models that, and is meant to be aggregated into version-specific
   * id3v2_frame subclasses.
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
    forward_output_iterator owner(forward_output_iterator p) const {
      return std::copy(owner_.begin(), owner_.end(), p);
    }
    /// Unique File Identifier
    template <typename forward_output_iterator>
    forward_output_iterator id(forward_output_iterator pout) const {
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
    forward_output_iterator email(forward_output_iterator p) const {
      return std::copy(email_.begin(), email_.end(), p);
    }
    unsigned char method_symbol() const {
      return method_symbol_;
    }
    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

  private:
    std::vector<unsigned char> email_;
    unsigned char method_symbol_;
    std::vector<unsigned char> data_;

  }; // End class encryption_method.

  class user_defined_text {

  public:
    template <typename forward_input_iterator>
    user_defined_text(forward_input_iterator p0,
                      forward_input_iterator p1)
    {
      unicode_ = *p0++;

      // Find the first $00 or $00 00, depending on the encoding.
      forward_input_iterator p = detail::find_trailing_null(unicode_, p0, p1);
      // [p0, p) has description (including the terminating null), [p, p1)
      // contains the string.
      std::copy(p0, p, std::back_inserter(description_));
      std::copy(p, p1, std::back_inserter(text_));
    }

  public:
    unsigned char unicode() const {
      return unicode_;
    }
    template <typename forward_output_iterator>
    forward_output_iterator description(forward_output_iterator p) const {
      return std::copy(description_.begin(), description_.end(), p);
    }
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

  private:
    unsigned char unicode_;
    std::vector<unsigned char> description_;
    std::vector<unsigned char> text_;

  }; // End class user_defined_text.

  class comments {

  public:

    template <typename forward_input_iterator>
    comments(forward_input_iterator p0,
             forward_input_iterator p1)
    {
      unicode_ = *p0++;
      std::copy(p0, p0 + 3, lang_);
      p0 += 3;
      forward_input_iterator p = detail::find_trailing_null(unicode_, p0, p1);
      std::copy(p0, p, std::back_inserter(description_));
      p += unicode_ ? 2 : 1;
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
    forward_output_iterator description(forward_output_iterator p) const {
      return std::copy(description_.begin(), description_.end(), p);
    }
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
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
    forward_output_iterator counter(forward_output_iterator p) const {
      std::copy(counter_.begin(), counter_.end(), p);
    }

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
    forward_output_iterator counter(forward_output_iterator p) const {
      std::copy(counter_.begin(), counter_.end(), p);
    }
    template <typename forward_output_iterator>
    forward_output_iterator email(forward_output_iterator p) const {
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
