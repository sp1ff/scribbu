#ifndef FRAMESV23_HH_INCLUDED
#define FRAMESV23_HH_INCLUDED 1

#include <scribbu/framesv2.hh>

#include <algorithm>
#include <memory>

#include <boost/optional.hpp>

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

     Frame ID: [A-Z0-0]{4}-- Identifiers beginning with "X", "Y" and "Z"
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
    id3v2_3_plus_frame(const frame_id4                      &id,
                       std::size_t                           size,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id):
      id3v2_frame             (size, id.experimental() ),
      id_                     (id                      ),
      tag_alter_preservation_ (tag_alter_preservation  ),
      file_alter_preservation_(file_alter_preservation ),
      read_only_              (read_only               ),
      encryption_method_      (encryption_method       ),
      group_id_               (group_id                )
    { }
    id3v2_3_plus_frame(const char                            id[4],
                       std::size_t                           size,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id):
      id3v2_3_plus_frame(frame_id4(id), size, tag_alter_preservation,
                         file_alter_preservation, read_only,
                         encryption_method, group_id)
    { }
    id3v2_3_plus_frame(unsigned char                         id0,
                       unsigned char                         id1,
                       unsigned char                         id2,
                       unsigned char                         id3,
                       std::size_t                           size,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id):
      id3v2_3_plus_frame(frame_id4(id0, id1, id2, id3), size,
                         tag_alter_preservation,
                         file_alter_preservation, read_only,
                         encryption_method, group_id)
    { }

  public:
    frame_id4 id() const {
      return id_;
    }

  private:
    frame_id4                      id_;
    tag_alter_preservation         tag_alter_preservation_;
    file_alter_preservation        file_alter_preservation_;
    read_only                      read_only_;
    boost::optional<unsigned char> encryption_method_;
    boost::optional<unsigned char> group_id_;

  }; // End class id3v2_3_plus_frame.

  /// Interface shared by all ID3v2.3 frames
  class id3v2_3_frame: public id3v2_3_plus_frame {

  public:
    id3v2_3_frame(const frame_id4                      &id,
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_plus_frame(id, size, tag_alter_preservation, file_alter_preservation,
                       read_only, encryption_method, group_id),
      decompressed_size_(decompressed_size)
    { }
    id3v2_3_frame(const char                            id[4],
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame(frame_id4(id), size, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id, decompressed_size)
    { }
    id3v2_3_frame(unsigned char                         id0,
                  unsigned char                         id1,
                  unsigned char                         id2,
                  unsigned char                         id3,
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame(frame_id4(id0, id1, id2, id3), size,
                    tag_alter_preservation, file_alter_preservation,
                    read_only, encryption_method, group_id, decompressed_size)
    { }

  private:
    boost::optional<std::size_t> decompressed_size_;

  }; // End class id3v2_3_frame.

  class unknown_id3v2_3_frame: public id3v2_3_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const frame_id4                      &id,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      id3v2_3_frame(id, p1 - p0, tag_alter_preservation, file_alter_preservation,
                    read_only, encryption_method, group_id, decompressed_size),
      data_(p0, p1)
    { }
    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(unsigned char                         id0,
                          unsigned char                         id1,
                          unsigned char                         id2,
                          unsigned char                         id3,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      unknown_id3v2_3_frame(frame_id4(id0, id1, id2), tag_alter_preservation,
                            file_alter_preservation, read_only, encryption_method,
                            group_id, decompressed_size, p0, p1)
    { }
    template <typename forward_input_iterator>
    unknown_id3v2_3_frame(const char                            id[4],
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      unknown_id3v2_3_frame(frame_id4(id), tag_alter_preservation,
                            file_alter_preservation, read_only, encryption_method,
                            group_id, decompressed_size, p0, p1)
    { }

  public:
    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_3_frame.

  class UFID: public id3v2_3_frame {

  public:
    template <typename forward_input_iterator>
    UFID(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
    id3v2_3_frame("UFID", p1 - p0, tag_alter_preservation,
                  file_alter_preservation, read_only,
                  encryption_method, group_id,
                  decompressed_size),
      unique_file_id_(p0, p1)
    { }

    unique_file_id file_id() const {
      return unique_file_id_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);
  private:
    unique_file_id unique_file_id_;

  }; // End class UFID.

  class ENCR: public id3v2_3_frame {

  public:
    template <typename forward_input_iterator>
    ENCR(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame("ENCR", p1-p0, tag_alter_preservation,
                    file_alter_preservation, read_only, encryption_method,
                    group_id, decompressed_size),
      encryption_method_(p0, p1)
    { }

  public:

    encryption_method get_method() const {
      return encryption_method_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);
  private:
    encryption_method encryption_method_;

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
   * 2.3.0, Sec. 3.3. "ID3v2 frame overview"] TODO: Update per the changes to
   * Unicode since the spec was written.
   *
   *
   */

  class id3v2_3_text_frame: public id3v2_3_frame {

  public:

    template <typename forward_input_iterator>
    id3v2_3_text_frame(const frame_id4                      &id,
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame(id, p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    decompressed_size)
    {
      unicode_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }
    template <typename forward_input_iterator>
    id3v2_3_text_frame(const char                            id[4],
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_text_frame(frame_id4(id), p0, p1, tag_alter_preservation,
                         file_alter_preservation, read_only,
                         encryption_method, group_id,
                         decompressed_size)
    { }
    template <typename forward_input_iterator>
    id3v2_3_text_frame(unsigned char                         id0,
                       unsigned char                         id1,
                       unsigned char                         id2,
                       unsigned char                         id3,
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id,
                       const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_text_frame(frame_id4(id0, id1, id2, id3), p0, p1,
                         tag_alter_preservation, file_alter_preservation,
                         read_only, encryption_method, group_id,
                         decompressed_size)
    { }

  public:
    std::string as_utf8() const;
    unsigned char unicode() const {
      return unicode_;
    }
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);
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

  class TXXX: public id3v2_3_frame {

  public:
    template <typename forward_input_iterator>
    TXXX(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame("TXXX", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    decompressed_size),
      user_defined_text_(p0, p1)
    { }

  public:

    user_defined_text udt() const {
      return user_defined_text_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);

  private:
    user_defined_text user_defined_text_;

  }; // End class TXXX.

  // Comments
  class COMM: public id3v2_3_frame {
  public:
    template <typename forward_input_iterator>
    COMM(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame("COMM", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    decompressed_size),
      comments_(p0, p1)
    { }

  public:
    comments data() const {
      return comments_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);

  private:
    comments comments_;

  }; // End class COMM.

  // play count
  class PCNT: public id3v2_3_frame {
  public:
    template <typename forward_input_iterator>
    PCNT(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame("PCNT", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    decompressed_size),
      count_(p0, p1)
    { }

  public:
    play_count count() const {
      return count_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);

  private:
    play_count count_;

  }; // End class PCNT.

  /// Popularimeter; cf. sec 4.18
  class POPM: public id3v2_3_frame {
  public:
    template <typename forward_input_iterator>
    POPM(forward_input_iterator                p0,
         forward_input_iterator                p1,
         tag_alter_preservation                tag_alter_preservation,
         file_alter_preservation               file_alter_preservation,
         read_only                             read_only,
         const boost::optional<unsigned char> &encryption_method,
         const boost::optional<unsigned char> &group_id,
         const boost::optional<std::size_t>   &decompressed_size):
      id3v2_3_frame("PCNT", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    decompressed_size),
      popularimeter_(p0, p1)
    { }

  public:
    popularimeter pop() const {
      return popularimeter_;
    }

    static std::unique_ptr<id3v2_3_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 const boost::optional<std::size_t>   &decompressed_size);

  private:
    popularimeter popularimeter_;

  }; // End class POPM.

} // End namespace scribbu.

#endif // not FRAMESV23_HH_INCLUDED
