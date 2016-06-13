#ifndef FRAMESV22_HH_INCLUDED
#define FRAMESV22_HH_INCLUDED 1

#include <scribbu/framesv2.hh>

#include <algorithm>
#include <memory>

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

     +----+---+
     | ID | 3 |
     +----+---+
     |size| 3 |
     +----+---+
     |data|...|
     +----+---+

   * \endcode
   *
   * The identifier  is a  three-letter name made  up of  the upper-case
   * characters A-Z and  the digits 0-9.  The ID is  followed by a (non-
   * sync-safe) three  byte unsigned integer  giving he the size  of the
   * frame  excluding  the ID  &  size  (i.e.   total  size -  6)  after
   * unsynchronisation.
   *
   *
   */

  class id3v2_2_frame: public id3v2_frame {

  public:
    id3v2_2_frame(const frame_id3 &id,
                  std::size_t      size):
      id3v2_frame(size, id.experimental()),
      id_(id)
    { }
    id3v2_2_frame(unsigned char id0,
                  unsigned char id1,
                  unsigned char id2,
                  std::size_t   size):
      id3v2_frame(size, ('X' == id0 || 'Y' == id0 || 'Z' == id0)),
      id_(id0, id1, id2)
    { }
    id3v2_2_frame(const char  id[3],
                  std::size_t size):
      id3v2_frame(size, ('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])),
      id_(id)
    { }

  public:
    /// Return the three character ID for this frame
    frame_id3  id() const {
      return id_;
    }

  private:
    frame_id3 id_;

  }; // End class id3v2_2_frame.

  class unknown_id3v2_2_frame: public id3v2_2_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_2_frame(const frame_id3       &id,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      id3v2_2_frame(id, p1 - p0),
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

  public:
    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_2_frame.

  class UFI: public id3v2_2_frame {

  public:
    template <typename forward_input_iterator>
    UFI(forward_input_iterator p0, forward_input_iterator p1):
      id3v2_2_frame("UFI", p1 - p0),
      unique_file_id_(p0, p1)
    { }

    unique_file_id file_id() const {
      return unique_file_id_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    unique_file_id unique_file_id_;

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
   * $0A only.
   *
   *
   * TODO: Figure out the impact of UTF-16/UTF-32 on "Unicode" encoding
   *
   *
   */

  class id3v2_2_text_frame: public id3v2_2_frame {

  public:

    template <typename forward_input_iterator>
    id3v2_2_text_frame(const frame_id3       &id,
                       forward_input_iterator p0,
                       forward_input_iterator p1):
      id3v2_2_frame(id, p1 - p0)
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

  public:
    std::string as_utf8() const;
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }
    unsigned char unicode() const {
      return unicode_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

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

  class TXX: public id3v2_2_frame {

  public:
    template <typename forward_input_iterator>
    TXX(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("TXX", p1 - p0),
      user_defined_text_(p0, p1)
    { }

  public:
    user_defined_text udt() const {
      return user_defined_text_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    user_defined_text user_defined_text_;

  }; // End class TXX.

  /// Comments
  class COM: public id3v2_2_frame {

  public:
    template <typename forward_input_iterator>
    COM(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("COM", p1 - p0),
      comments_(p0, p1)
    { }

  public:
    comments data() const {
      return comments_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    comments comments_;

  };

  /// play count
  class CNT: public id3v2_2_frame {

  public:
    template <typename forward_input_iterator>
    CNT(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("CNT", p1 - p0),
      count_(p0, p1)
    { }

  public:
    play_count count() const {
      return count_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    play_count count_;

  };

  /// Popularimeter
  class POP: public id3v2_2_frame {

  public:
    template <typename forward_input_iterator>
    POP(forward_input_iterator p0,
        forward_input_iterator p1):
      id3v2_2_frame("POP", p1 - p0),
      popularimeter_(p0, p1)
    { }

  public:
    popularimeter pop() const {
      return popularimeter_;
    }

    static std::unique_ptr<id3v2_2_frame> create(const frame_id3& id, const unsigned char *p, std::size_t cb);

  private:
    popularimeter popularimeter_;

  }; // End class POP.

} // End namespace scribbu.

#endif // not FRAMESV22_HH_INCLUDED
