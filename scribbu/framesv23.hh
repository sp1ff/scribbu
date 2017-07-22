#ifndef FRAMESV23_HH_INCLUDED
#define FRAMESV23_HH_INCLUDED 1

#include <scribbu/framesv2.hh>

#include <algorithm>
#include <memory>

#include <boost/optional.hpp>

#include <scribbu/charsets.hh>

namespace scribbu {

  /**
   * \brief Represents an ID3v2.3 or ID3v2.4 frame
   *
   *
   * \subsubsection scribbu_id3v2_discuss_frames_23 "ID3v2.3 Frames"
   *
   * After the header, and possibly after the extended header, ID3v2.3 tags
   * consist of one more frames, each satisfying the following layout:
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

  public:

    id3v2_3_plus_frame(const frame_id4 &id,
                       std::size_t size,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id):
      id3v2_frame(size, id.experimental()),
      id_(id),
      tap_(tap),
      fap_(fap),
      read_only_(read_only),
      encmth_(encmth),
      group_id_(group_id)
    { }

    id3v2_3_plus_frame(const char id[4],
                       std::size_t size,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id):
      id3v2_3_plus_frame(frame_id4(id), size, tap, fap, read_only,
                         encmth, group_id)
    { }
    id3v2_3_plus_frame(unsigned char id0,
                       unsigned char id1,
                       unsigned char id2,
                       unsigned char id3,
                       std::size_t size,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id):
      id3v2_3_plus_frame(frame_id4(id0, id1, id2, id3), size, tap,
                         fap, read_only, encmth, group_id)
    { }

  public:

    frame_id4 id() const {
      return id_;
    }

  private:
    frame_id4                      id_;
    tag_alter_preservation         tap_;
    file_alter_preservation        fap_;
    read_only                      read_only_;
    boost::optional<unsigned char> encmth_;
    boost::optional<unsigned char> group_id_;

  }; // End class id3v2_3_plus_frame.

  /// Interface shared by all ID3v2.3 frames
  class id3v2_3_frame: public id3v2_3_plus_frame {

  public:

    id3v2_3_frame(const frame_id4 &id,
                  std::size_t size,
                  tag_alter_preservation tap,
                  file_alter_preservation fap,
                  read_only read_only,
                  const boost::optional<unsigned char> &encmth,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t> &decsz):
      id3v2_3_plus_frame(id, size, tap, fap, read_only, encmth, group_id),
      decsz_(decsz)
    { }

    id3v2_3_frame(const char id[4],
                  std::size_t size,
                  tag_alter_preservation tap,
                  file_alter_preservation fap,
                  read_only read_only,
                  const boost::optional<unsigned char> &encmth,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t> &decsz):
      id3v2_3_frame(frame_id4(id), size, tap, fap, read_only,
                    encmth, group_id, decsz)
    { }

    id3v2_3_frame(unsigned char id0,
                  unsigned char id1,
                  unsigned char id2,
                  unsigned char id3,
                  std::size_t size,
                  tag_alter_preservation tap,
                  file_alter_preservation fap,
                  read_only read_only,
                  const boost::optional<unsigned char> &encmth,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t> &decsz):
      id3v2_3_frame(frame_id4(id0, id1, id2, id3), size, tap, fap,
                    read_only, encmth, group_id, decsz)
    { }

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

  private:
    boost::optional<std::size_t> decsz_;

  }; // End class id3v2_3_frame.

  class unknown_id3v2_3_frame: public id3v2_3_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const frame_id4 &id,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &encmth,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t> &decsz,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      id3v2_3_frame(id, p1 - p0, tap, fap, read_only, encmth, group_id, decsz),
      data_(p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(unsigned char id0,
                          unsigned char id1,
                          unsigned char id2,
                          unsigned char id3,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &encmth,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t> &decsz,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_3_frame(frame_id4(id0, id1, id2), tap, fap, read_only,
                            encmth, group_id, decsz, p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const char id[4],
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &encmth,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t> &decsz,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_3_frame(frame_id4(id), tap, fap, read_only, encmth,
                            group_id, decsz, p0, p1)
    { }

  public:

    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_3_frame.

  class UFID: public id3v2_3_frame, public unique_file_id {

  public:
    template <typename forward_input_iterator>
    UFID(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
    id3v2_3_frame("UFID", p1 - p0, tap, fap, read_only,
                  encmth, group_id, decsz),
      unique_file_id(p0, p1)
    { }

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

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

  }; // End class UFID.

  class ENCR: public id3v2_3_frame, public encryption_method {

  public:
    template <typename forward_input_iterator>
    ENCR(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
      id3v2_3_frame("ENCR", p1-p0, tap, fap, read_only, encmth,
                    group_id, decsz),
      encryption_method(p0, p1)
    { }

  public:

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

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

  };

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
    id3v2_3_text_frame(const frame_id4 &id,
                       forward_input_iterator p0,
                       forward_input_iterator p1,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t> &decsz):
      id3v2_3_frame(id, p1 - p0, tap,
                    fap, read_only,
                    encmth, group_id,
                    decsz)
    {
      unicode_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }

    template <typename forward_input_iterator>
    id3v2_3_text_frame(const char id[4],
                       forward_input_iterator p0,
                       forward_input_iterator p1,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t> &decsz):
      id3v2_3_text_frame(frame_id4(id), p0, p1, tap,
                         fap, read_only,
                         encmth, group_id,
                         decsz)
    { }

    template <typename forward_input_iterator>
    id3v2_3_text_frame(unsigned char id0,
                       unsigned char id1,
                       unsigned char id2,
                       unsigned char id3,
                       forward_input_iterator p0,
                       forward_input_iterator p1,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only read_only,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t> &decsz):
      id3v2_3_text_frame(frame_id4(id0, id1, id2, id3), p0, p1,
                         tap, fap,
                         read_only, encmth, group_id,
                         decsz)
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
    id3v2_3_text_frame(const frame_id4 &id,
                       const string_type &text,
                       encoding srcenc,
                       bool ucs2,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only ro,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &grid,
                       const boost::optional<std::size_t> &decsz):
      id3v2_3_text_frame(id, ucs2, convert_encoding(text, srcenc, ucs2 ? encoding::UCS_2LE : 
                                                    encoding::ISO_8859_1, true),
                         tap, fap, ro, encmth, grid, decsz)
    { }

  private:
    id3v2_3_text_frame(const frame_id4 &id,
                       bool unicode,
                       const std::vector<unsigned char> &text,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only ro,
                       const boost::optional<unsigned char> &encmth,
                       const boost::optional<unsigned char> &grid,
                       const boost::optional<std::size_t> &decsz):
      id3v2_3_frame(id, text.size(), tap, fap, ro, encmth, grid, decsz),
      unicode_(unicode),
      text_(text)
    { }

  public:

    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

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

    unsigned char unicode() const {
      return unicode_;
    }

    static std::unique_ptr<id3v2_3_text_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tap,
           file_alter_preservation               fap,
           read_only                             read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t>   &decsz);

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
    TXXX(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
      id3v2_3_frame("TXXX", p1 - p0, tap, fap, read_only,
                    encmth, group_id, decsz),
      user_defined_text(3, p0, p1)
    { }

  public:

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

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
      user_defined_text::descriptionb(back_inserter(buf));
      return id3v2_3_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

  }; // End class TXXX.

  // Comments
  class COMM: public id3v2_3_frame, public comments {
  public:
    template <typename forward_input_iterator>
    COMM(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
      id3v2_3_frame("COMM", p1 - p0, tap, fap, read_only,
                    encmth, group_id, decsz),
      comments(3, p0, p1)
    { }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

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
  }; // End class COMM.

  // play count
  class PCNT: public id3v2_3_frame, public play_count {
  public:
    template <typename forward_input_iterator>
    PCNT(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
      id3v2_3_frame("PCNT", p1 - p0, tap,
                    fap, read_only,
                    encmth, group_id,
                    decsz),
      play_count(p0, p1)
    { }

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

  }; // End class PCNT.

  /// Popularimeter; cf. sec 4.18
  class POPM: public id3v2_3_frame, public popularimeter {
  public:
    template <typename forward_input_iterator>
    POPM(forward_input_iterator p0,
         forward_input_iterator p1,
         tag_alter_preservation tap,
         file_alter_preservation fap,
         read_only read_only,
         const boost::optional<unsigned char> &encmth,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t> &decsz):
      id3v2_3_frame("POPM", p1 - p0, tap, fap, read_only,
                    encmth, group_id, decsz),
      popularimeter(p0, p1)
    { }

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

    static
    std::unique_ptr<id3v2_3_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only read_only,
           const boost::optional<unsigned char> &encmth,
           const boost::optional<unsigned char> &group_id,
           const boost::optional<std::size_t> &decsz);

  }; // End class POPM.

} // End namespace scribbu.

#endif // not FRAMESV23_HH_INCLUDED
