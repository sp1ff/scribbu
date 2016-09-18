#ifndef FRAMESV24_HH_INCLUDED
#define FRAMESV24_HH_INCLUDED 1

#include <scribbu/scribbu.hh>
#include <scribbu/framesv2.hh>
#include <scribbu/framesv23.hh>

#include <algorithm>
#include <memory>

#include <boost/optional.hpp>

namespace scribbu {

  /**
   * \class id3v2_4_frame
   *
   * \brief Interface shared by all ID3v2.4 frames
   *
   *
   * After the header, and possibly after the extended header, ID3v2.3 frames
   * consist of one more tags, each satisfying the following layout:
   *
   \code

     +--------+-------------------+
     |Frame ID|   $xx xx xx xx    |
     +--------+-------------------+
     |  Size  |   4 * %0xxxxxxx   |
     +--------+-------------------+
     |  Flags |%0abc0000 %0h00kmnp|
     +--------+-------------------+

     Frame ID: [A-Z0-0]{4}-- Identifiers beginning with "X", "Y" and "Z"
     are for experimental use

     Size: A sync-safe integer giving the size of the frame, in bytes, after
     compression, encryption, and unsynchronisation, not including the header
     (i.e. total frame size - 10)

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
   *
   */

  class id3v2_4_frame: public id3v2_3_plus_frame {

  public:
    /// Starting with rev. 2.4, frames can be unsync'd on an individual basis
    enum class unsynchronised {
      clear,
      set
    };

  public:
    id3v2_4_frame(const frame_id4                      &id,
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  bool                                  compressed,
                  bool                                  unsynchronised,
                  const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_3_plus_frame(id, size, tag_alter_preservation,
                         file_alter_preservation, read_only,
                         encryption_method, group_id),
      compressed_(compressed),
      unsynchronised_(unsynchronised),
      data_len_indicator_(data_len_indicator)
    { }
    id3v2_4_frame(const char                            id[4],
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  bool                                  compressed,
                  bool                                  unsynchronised,
                  const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame(frame_id4(id), size, tag_alter_preservation,
                    file_alter_preservation, read_only, encryption_method,
                    group_id, compressed, unsynchronised,
                    data_len_indicator)
    { }
    id3v2_4_frame(unsigned char                         id0,
                  unsigned char                         id1,
                  unsigned char                         id2,
                  unsigned char                         id3,
                  std::size_t                           size,
                  tag_alter_preservation                tag_alter_preservation,
                  file_alter_preservation               file_alter_preservation,
                  read_only                             read_only,
                  const boost::optional<unsigned char> &encryption_method,
                  const boost::optional<unsigned char> &group_id,
                  bool                                  compressed,
                  bool                                  unsynchronised,
                  const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_3_plus_frame(frame_id4(id0, id1, id2, id3), size,
                         tag_alter_preservation, file_alter_preservation,
                         read_only, encryption_method, group_id)
    { }

  private:
    bool                         compressed_;
    bool                         unsynchronised_;
    boost::optional<std::size_t> data_len_indicator_;

  }; // End class id3v2_4_frame.

  class unknown_id3v2_4_frame: public id3v2_4_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(const frame_id4                      &id,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      id3v2_4_frame(id, p1 - p0, tag_alter_preservation, file_alter_preservation,
                    read_only, encryption_method, group_id, compressed,
                    unsynchronised, data_len_indicator),
      data_(p0, p1)
    { }
    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(unsigned char                         id0,
                          unsigned char                         id1,
                          unsigned char                         id2,
                          unsigned char                         id3,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      unknown_id3v2_4_frame(frame_id4(id0, id1, id2), tag_alter_preservation,
                            file_alter_preservation, read_only, encryption_method,
                            group_id, compressed, unsynchronised,
                            data_len_indicator, p0, p1)
    { }
    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(const char                            id[4],
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator,
                          forward_input_iterator                p0,
                          forward_input_iterator                p1):
      unknown_id3v2_4_frame(frame_id4(id), tag_alter_preservation,
                            file_alter_preservation, read_only, encryption_method,
                            group_id, compressed, unsynchronised,
                            data_len_indicator, p0, p1)
    { }

  public:
    template <typename forward_output_iterator>
    forward_output_iterator data(forward_output_iterator p) const {
      return std::copy(data_.begin(), data_.end(), p);
    }

  private:
    std::vector<unsigned char> data_;

  }; // End class unknown_id3v2_4_frame.

  class UFID_2_4: public id3v2_4_frame {
  public:
    template <typename forward_input_iterator>
    UFID_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("UFID", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only, encryption_method, group_id,
                    compressed, unsynchronised, data_len_indicator),
      unique_file_id_(p0, p1)
    { }

    unique_file_id file_id() const {
      return unique_file_id_;
    }

    static std::unique_ptr<id3v2_4_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 bool                                  compressed,
                                                 bool                                  unsynchronised,
                                                 const boost::optional<std::size_t>   &data_len_indicator);

  private:
    unique_file_id unique_file_id_;

  }; // End class UFID_2_4.

  class ENCR_2_4: public id3v2_4_frame {

  public:
    template <typename forward_input_iterator>
    ENCR_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("ENCR", p1 - p0, tag_alter_preservation,
                  file_alter_preservation, read_only,
                  encryption_method, group_id, compressed,
                  unsynchronised, data_len_indicator),
      encryption_method_(p0, p1)
    { }

    encryption_method method() const {
      return encryption_method_;
    }

    static std::unique_ptr<id3v2_4_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 bool                                  compressed,
                                                 bool                                  unsynchronised,
                                                 const boost::optional<std::size_t>   &data_len_indicator);

  private:
    encryption_method encryption_method_;

  }; // End class ENCR_2_4.

  /**
   * \class id3v2_4_text_frame
   *
   * \brief ID3 v2.4 text information frame
   *
   *
   * "The text information frames are the most important frames, containing
   * information like artist, album and more. There may only be one text
   * information frame of its kind in an tag. If the textstring is followed by
   * a termination ($00 (00)) all the following information should be ignored
   * and not be displayed. All text frame identifiers begin with "T". Only text
   * frame identifiers begin with "T", with the exception of the "TXXX"
   * frame. All the text information frames have the following format:" [ID3
   * tag version 2.4.0 - Native Frames Sec. 4.2. :Text information frames"]
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
   * Possible encodings:
   *
   *      $00   ISO-8859-1 [ISO-8859-1]. Terminated with $00.
   *      $01   UTF-16 [UTF-16] encoded Unicode [UNICODE] with BOM. All
   *            strings in the same frame SHALL have the same byteorder.
   *            Terminated with $00 00.
   *      $02   UTF-16BE [UTF-16] encoded Unicode [UNICODE] without BOM.
   *            Terminated with $00 00.
   *      $03   UTF-8 [UTF-8] encoded Unicode [UNICODE]. Terminated with $00.
   *
   * [ID3 tag version 2.4.0 - Native Frames, Sec. 4.2. "Text information
   * frames"]
   *
   *
   */

  class id3v2_4_text_frame: public id3v2_4_frame {

  public:

    template <typename forward_input_iterator>
    id3v2_4_text_frame(const frame_id4                      &id,
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id,
                       bool                                  compressed,
                       bool                                  unsynchronised,
                       const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame(id, p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    compressed, unsynchronised, data_len_indicator)
    {
      encoding_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }
    template <typename forward_input_iterator>
    id3v2_4_text_frame(const char                            id[4],
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tag_alter_preservation,
                       file_alter_preservation               file_alter_preservation,
                       read_only                             read_only,
                       const boost::optional<unsigned char> &encryption_method,
                       const boost::optional<unsigned char> &group_id,
                       bool                                  compressed,
                       bool                                  unsynchronised,
                       const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_text_frame(frame_id4(id), p0, p1, tag_alter_preservation,
                         file_alter_preservation, read_only,
                         encryption_method, group_id,
                         compressed, unsynchronised, data_len_indicator)
    { }
    template <typename forward_input_iterator>
    id3v2_4_text_frame(unsigned char                         id0,
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
                       bool                                  compressed,
                       bool                                  unsynchronised,
                       const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_text_frame(frame_id4(id0, id1, id2, id3), p0, p1,
                         tag_alter_preservation, file_alter_preservation,
                         read_only, encryption_method, group_id,
                         compressed, unsynchronised, data_len_indicator)
    { }

  public:
    std::string as_utf8() const;
    unsigned char encoding() const {
      return encoding_;
    }
    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const {
      return std::copy(text_.begin(), text_.end(), p);
    }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tag_alter_preservation,
           file_alter_preservation               file_alter_preservation,
           read_only                             read_only,
           const boost::optional<unsigned char> &encryption_method,
           const boost::optional<unsigned char> &group_id,
           bool                                  compressed,
           bool                                  unsynchronised,
           const boost::optional<std::size_t>   &data_len_indicator);

  private:
    unsigned char encoding_;
    std::vector<unsigned char> text_;

  };


  /// User-defined text information
  class TXXX_2_4: public id3v2_4_frame {
  public:
    template <typename forward_input_iterator>
    TXXX_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("TXXX", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only, encryption_method, group_id,
                    compressed, unsynchronised, data_len_indicator),
      user_defined_text_(p0, p1)
    { }

    user_defined_text udt() const {
      return user_defined_text_;
    }

    static std::unique_ptr<id3v2_4_frame> create(const frame_id4                      &id,
                                                 const unsigned char                  *p,
                                                 std::size_t                           cb,
                                                 tag_alter_preservation                tag_alter_preservation,
                                                 file_alter_preservation               file_alter_preservation,
                                                 read_only                             read_only,
                                                 const boost::optional<unsigned char> &encryption_method,
                                                 const boost::optional<unsigned char> &group_id,
                                                 bool                                  compressed,
                                                 bool                                  unsynchronised,
                                                 const boost::optional<std::size_t>   &data_len_indicator);

  private:
    user_defined_text user_defined_text_;

  }; // End class TXXX_2_4.

  // Comments
  class COMM_2_4: public id3v2_4_frame {
  public:
    template <typename forward_input_iterator>
    COMM_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("COMM", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    compressed, unsynchronised,
                    data_len_indicator),
      comments_(p0, p1)
    { }

  public:
    comments data() const {
      return comments_;
    }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tag_alter_preservation,
           file_alter_preservation               file_alter_preservation,
           read_only                             read_only,
           const boost::optional<unsigned char> &encryption_method,
           const boost::optional<unsigned char> &group_id,
           bool                                  compressed,
           bool                                  unsynchronised,
           const boost::optional<std::size_t>   &data_len_indicator);

  private:
    comments comments_;

  };

  // play count
  class PCNT_2_4: public id3v2_4_frame {
  public:
    template <typename forward_input_iterator>
    PCNT_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("PCNT", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    compressed, unsynchronised,
                    data_len_indicator),
      count_(p0, p1)
    { }

  public:
    play_count count() const {
      return count_;
    }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tag_alter_preservation,
           file_alter_preservation               file_alter_preservation,
           read_only                             read_only,
           const boost::optional<unsigned char> &encryption_method,
           const boost::optional<unsigned char> &group_id,
           bool                                  compressed,
           bool                                  unsynchronised,
           const boost::optional<std::size_t>   &data_len_indicator);

  private:
    play_count count_;

  };

  // Popularimeter
  class POPM_2_4: public id3v2_4_frame {
  public:
    template <typename forward_input_iterator>
    POPM_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tag_alter_preservation,
             file_alter_preservation               file_alter_preservation,
             read_only                             read_only,
             const boost::optional<unsigned char> &encryption_method,
             const boost::optional<unsigned char> &group_id,
             bool                                  compressed,
             bool                                  unsynchronised,
             const boost::optional<std::size_t>   &data_len_indicator):
      id3v2_4_frame("PCNT", p1 - p0, tag_alter_preservation,
                    file_alter_preservation, read_only,
                    encryption_method, group_id,
                    compressed, unsynchronised,
                    data_len_indicator),
      popularimeter_(p0, p1)
    { }

  public:
    popularimeter pop() const {
      return popularimeter_;
    }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tag_alter_preservation,
           file_alter_preservation               file_alter_preservation,
           read_only                             read_only,
           const boost::optional<unsigned char> &encryption_method,
           const boost::optional<unsigned char> &group_id,
           bool                                  compressed,
           bool                                  unsynchronised,
           const boost::optional<std::size_t>   &data_len_indicator);

  private:
    popularimeter popularimeter_;

  }; // End class POPM_2_4.

} // End namespace scribbu.

#endif // not FRAMESV24_HH_INCLUDED
