#ifndef FRAMESV24_HH_INCLUDED
#define FRAMESV24_HH_INCLUDED 1
#include <algorithm>
#include <memory>

#include <boost/optional.hpp>

#include <scribbu/framesv2.hh>
#include <scribbu/framesv23.hh>

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

    typedef boost::optional<size_t> dli_type;

  public:
    id3v2_4_frame(const frame_id4        &id,
                  std::size_t             size,
                  tag_alter_preservation  tap,
                  file_alter_preservation fap,
                  read_only               ro,
                  bool                    cmp,
                  const id_type          &enc,
                  const id_type          &gid,
                  bool                    unsynch,
                  const dli_type         &dli):
      id3v2_3_plus_frame(id, size, tap, fap, ro, cmp, enc, gid),
      compressed_(cmp),
      unsynchronised_(unsynch),
      data_len_indicator_(dli)
    { }
    id3v2_4_frame(const char              id[4],
                  std::size_t             size,
                  tag_alter_preservation  tap,
                  file_alter_preservation fap,
                  read_only               ro,
                  const id_type          &enc,
                  const id_type          &gid,
                  bool                    cmp,
                  bool                    unsynch,
                  const dli_type         &dli):
      id3v2_4_frame(frame_id4(id), size, tap, fap, ro, cmp, enc, gid, unsynch, dli)
    { }
    id3v2_4_frame(unsigned char id0,
                  unsigned char id1,
                  unsigned char id2,
                  unsigned char id3,
                  std::size_t size,
                  tag_alter_preservation tap,
                  file_alter_preservation fap,
                  read_only ro,
                  const boost::optional<unsigned char> &enc,
                  const boost::optional<unsigned char> &gid,
                  bool cmprsd,
                  bool unsynch,
                  const boost::optional<std::size_t> &dli):
      id3v2_3_plus_frame(frame_id4(id0, id1, id2, id3), size,
                         tap, fap, ro, cmprsd, enc, gid)
    { }
    virtual id3v2_4_frame* clone() const = 0;

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

  private:
    bool compressed_;
    bool unsynchronised_;
    boost::optional<std::size_t> data_len_indicator_;

  }; // End class id3v2_4_frame.

  class unknown_id3v2_4_frame: public id3v2_4_frame {

  public:

    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(const frame_id4 &id,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only ro,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool cmprsd,
                          bool unsynch,
                          const boost::optional<std::size_t> &dli,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      id3v2_4_frame(id, p1 - p0, tap, fap, ro, cmprsd, enc, gid, 
                    unsynch, dli),
      data_(p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(unsigned char id0,
                          unsigned char id1,
                          unsigned char id2,
                          unsigned char id3,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only ro,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool cmprsd,
                          bool unsynch,
                          const boost::optional<std::size_t> &dli,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_4_frame(frame_id4(id0, id1, id2), tap, fap, ro, enc,
                            gid, cmprsd, unsynch, dli, p0, p1)
    { }

    template <typename forward_input_iterator>
    unknown_id3v2_4_frame(const char id[4],
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only ro,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool cmprsd,
                          bool unsynch,
                          const boost::optional<std::size_t> &dli,
                          forward_input_iterator p0,
                          forward_input_iterator p1):
      unknown_id3v2_4_frame(frame_id4(id), tap, fap, ro, enc,
                            gid, cmprsd, unsynch, dli, p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new unknown_id3v2_4_frame(*this); }

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

  }; // End class unknown_id3v2_4_frame.

  class UFID_2_4: public id3v2_4_frame, public unique_file_id {
  public:
    template <typename forward_input_iterator>
    UFID_2_4(forward_input_iterator p0,
             forward_input_iterator p1,
             tag_alter_preservation tap,
             file_alter_preservation fap,
             read_only ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool cmprsd,
             bool unsynch,
             const boost::optional<std::size_t> &dli):
      id3v2_4_frame("UFID", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      unique_file_id(p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new UFID_2_4(*this); }

    static
    std::unique_ptr<id3v2_4_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool cmprsd,
           bool unsynch,
           const boost::optional<std::size_t> &dli);

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


  }; // End class UFID_2_4.

  class ENCR_2_4: public id3v2_4_frame, public encryption_method {

  public:
    template <typename forward_input_iterator>
    ENCR_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tap,
             file_alter_preservation               fap,
             read_only                             ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool                                  cmprsd,
             bool                                  unsynch,
             const boost::optional<std::size_t>   &dli):
      id3v2_4_frame("ENCR", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      encryption_method(p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new ENCR_2_4(*this); }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool cmprsd,
           bool unsynch,
           const boost::optional<std::size_t> &dli);

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
   *      $00 ISO-8859-1 [ISO-8859-1]. Terminated with $00.
   *      $01 UTF-16 [UTF-16] encoded Unicode [UNICODE] with BOM. All
   *            strings in the same frame SHALL have the same byteorder.
   *            Terminated with $00 00.
   *      $02 UTF-16BE [UTF-16] encoded Unicode [UNICODE] without BOM.
   *            Terminated with $00 00.
   *      $03 UTF-8 [UTF-8] encoded Unicode [UNICODE]. Terminated with $00.
   *
   * [ID3 tag version 2.4.0 - Native Frames, Sec. 4.2. "Text information
   * frames"]
   *
   *
   */

  class id3v2_4_text_frame: public id3v2_4_frame {

  public:

    // ID3v2.4 text frames can be encoded in one of four ways (see above); on
    // read, I represent the encoding as a single byte, but on write I
    // would like to restrict the callers's choices a bit more
    enum class frame_encoding {
      ISO_8859_1, UTF_16_BOM, UTF_16_BE, UTF_8
    };

  public:
    // frame_id4
    template <typename forward_input_iterator>
    id3v2_4_text_frame(const frame_id4                      &id,
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tap,
                       file_alter_preservation               fap,
                       read_only                             ro,
                       const boost::optional<unsigned char> &enc,
                       const boost::optional<unsigned char> &gid,
                       bool                                  cmprsd,
                       bool                                  unsynch,
                       const boost::optional<std::size_t>   &dli):
      id3v2_4_frame(id, p1 - p0, tap, fap, ro, cmprsd, enc, gid, unsynch, dli)
    {
      unicode_ = *p0++;
      std::copy(p0, p1, std::back_inserter(text_));
    }
    // char[4]
    template <typename forward_input_iterator>
    id3v2_4_text_frame(const char                            id[4],
                       forward_input_iterator                p0,
                       forward_input_iterator                p1,
                       tag_alter_preservation                tap,
                       file_alter_preservation               fap,
                       read_only                             ro,
                       const boost::optional<unsigned char> &enc,
                       const boost::optional<unsigned char> &gid,
                       bool                                  cmprsd,
                       bool                                  unsynch,
                       const boost::optional<std::size_t>   &dli):
      id3v2_4_text_frame(frame_id4(id), p0, p1, tap, fap, ro,
                         enc, gid, cmprsd, unsynch, dli)
    { }
    // four cahrs
    template <typename forward_input_iterator>
    id3v2_4_text_frame(unsigned char id0,
                       unsigned char id1,
                       unsigned char id2,
                       unsigned char id3,
                       forward_input_iterator p0,
                       forward_input_iterator p1,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only ro,
                       const boost::optional<unsigned char> &enc,
                       const boost::optional<unsigned char> &gid,
                       bool cmprsd,
                       bool unsynch,
                       const boost::optional<std::size_t> &dli):
      id3v2_4_text_frame(frame_id4(id0, id1, id2, id3), p0, p1,
                         tap, fap, ro, enc, gid, cmprsd, unsynch, dli)
    { }

    template <typename string_type>
    id3v2_4_text_frame(const frame_id4 &id,
                       const string_type &text,
                       encoding src,
                       frame_encoding dst,
                       bool add_bom = false,
                       on_no_encoding rsp = on_no_encoding::fail,
                       tag_alter_preservation tap = tag_alter_preservation::preserve,
                       file_alter_preservation fap = file_alter_preservation::preserve,
                       read_only ro = read_only::clear,
                       const boost::optional<unsigned char> &enc = boost::none,
                       const boost::optional<unsigned char> &gid = boost::none,
                       bool cmp = false,
                       bool unsync = false,
                       const boost::optional<std::size_t> &dli = boost::none):
      id3v2_4_text_frame(id, dst,
                         convert_encoding(text, src, encshim(dst), add_bom, rsp),
                         tap, fap, ro, enc, gid, cmp, unsync, dli)
    { }

    virtual id3v2_4_frame* clone() const
    { return new id3v2_4_text_frame(*this); }

    static std::unique_ptr<id3v2_4_text_frame>
    create(const frame_id4 &id,
           const unsigned char *p,
           std::size_t cb,
           tag_alter_preservation tap,
           file_alter_preservation fap,
           read_only ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool compressed,
           bool unsynch,
           const boost::optional<std::size_t> &dli);

  private:
    static encoding encshim(frame_encoding x);
    static unsigned char encshim2(frame_encoding x);
    id3v2_4_text_frame(const frame_id4 &id,
                       frame_encoding dstenc,
                       const std::vector<unsigned char> &text,
                       tag_alter_preservation tap,
                       file_alter_preservation fap,
                       read_only ro,
                       const boost::optional<unsigned char> &enc,
                       const boost::optional<unsigned char> &gid,
                       bool cmp,
                       bool unsync,
                       const boost::optional<std::size_t> &dli):
      id3v2_4_frame(id, text.size() + 1, tap, fap, ro, cmp, enc, gid, unsync, dli),
      unicode_(encshim2(dstenc)),
      text_(text)
    { }

  public:

    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    void set(const std::string &text,
             encoding src = encoding::UTF_8,
             frame_encoding dst = frame_encoding::UTF_8,
             bool add_bom = false,
             on_no_encoding rsp = on_no_encoding::fail);

    unsigned char unicode() const
    { return unicode_; }

    template <typename forward_output_iterator>
    forward_output_iterator text(forward_output_iterator p) const
    { return std::copy(text_.begin(), text_.end(), p); }

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
      return id3v2_4_frame::as_str<string_type>(&(text_[0]), text_.size(),
                                                unicode(), dst, rsp, src);
    }

  protected:

    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  private:
    unsigned char unicode_;
    std::vector<unsigned char> text_;

  }; // End class id3v2_4_text_frame.


  /// User-defined text information
  class TXXX_2_4: public id3v2_4_frame, public user_defined_text {
  public:
    template <typename forward_input_iterator>
    TXXX_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tap,
             file_alter_preservation               fap,
             read_only                             ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool                                  cmprsd,
             bool                                  unsynch,
             const boost::optional<std::size_t>   &dli):
      id3v2_4_frame("TXXX", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      user_defined_text(4, p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new TXXX_2_4(*this); }

    static
    std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tap,
           file_alter_preservation               fap,
           read_only                             ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool                                  cmprsd,
           bool                                  unsynch,
           const boost::optional<std::size_t>   &dli);

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
      return id3v2_4_frame::as_str<string_type>(&(buf[0]), buf.size(),
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
      return id3v2_4_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

  protected:

    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class TXXX_2_4.

  // Comments
  class COMM_2_4: public id3v2_4_frame, public comments {
  public:
    template <typename forward_input_iterator>
    COMM_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tap,
             file_alter_preservation               fap,
             read_only                             ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool                                  cmprsd,
             bool                                  unsynch,
             const boost::optional<std::size_t>   &dli):
      id3v2_4_frame("COMM", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      comments(4, p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new COMM_2_4(*this); }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tap,
           file_alter_preservation               fap,
           read_only                             ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool                                  cmprsd,
           bool                                  unsynch,
           const boost::optional<std::size_t>   &dli);

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
      comments::descriptionb(back_inserter(buf));
      return id3v2_4_frame::as_str<string_type>(&(buf[0]), buf.size(),
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
      return id3v2_4_frame::as_str<string_type>(&(buf[0]), buf.size(),
                                                unicode(), dst, rsp, src);
    }

  protected:

    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class COMM_2_4.

  // play count
  class PCNT_2_4: public id3v2_4_frame, public play_count {
  public:
    template <typename forward_input_iterator>
    PCNT_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tap,
             file_alter_preservation               fap,
             read_only                             ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool                                  cmprsd,
             bool                                  unsynch,
             const boost::optional<std::size_t>   &dli):
      id3v2_4_frame("PCNT", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      play_count(p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new PCNT_2_4(*this); }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tap,
           file_alter_preservation               fap,
           read_only                             ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool                                  cmprsd,
           bool                                  unsynch,
           const boost::optional<std::size_t>   &dli);

  public:
    
    /// Return the size, in bytes, of the frame, prior to desynchronisation,
    /// compression, and/or encryption exclusive of the header
    virtual std::size_t size() const;

    std::size_t count() const
    { return play_count::count(); }

  protected:

    /// Serialize this frame to \a os, exclusive of any compression, encryption
    /// or unsynchronisation; return the number of bytes written
    virtual std::size_t serialize(std::ostream &os) const;

  }; // End class PCNT_2_4.

  // Popularimeter
  class POPM_2_4: public id3v2_4_frame, public popularimeter {
  public:
    template <typename forward_input_iterator>
    POPM_2_4(forward_input_iterator                p0,
             forward_input_iterator                p1,
             tag_alter_preservation                tap,
             file_alter_preservation               fap,
             read_only                             ro,
             const boost::optional<unsigned char> &enc,
             const boost::optional<unsigned char> &gid,
             bool                                  cmprsd,
             bool                                  unsynch,
             const boost::optional<std::size_t>   &dli):
      id3v2_4_frame("POPM", p1 - p0, tap, fap, ro, enc, gid,
                    cmprsd, unsynch, dli),
      popularimeter(p0, p1)
    { }

    virtual id3v2_4_frame* clone() const
    { return new POPM_2_4(*this); }

    static std::unique_ptr<id3v2_4_frame>
    create(const frame_id4                      &id,
           const unsigned char                  *p,
           std::size_t                           cb,
           tag_alter_preservation                tap,
           file_alter_preservation               fap,
           read_only                             ro,
           const boost::optional<unsigned char> &enc,
           const boost::optional<unsigned char> &gid,
           bool                                  cmprsd,
           bool                                  unsynch,
           const boost::optional<std::size_t>   &dli);

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

  }; // End class POPM_2_4.

} // End namespace scribbu.

#endif // not FRAMESV24_HH_INCLUDED
