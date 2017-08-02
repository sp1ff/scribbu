#ifndef ID3V2_HH_INCLUDED
#define ID3V2_HH_INCLUDED 1

#include <exception>
#include <iostream>
#include <memory>
#include <mutex>
#include <unordered_map>

#include <boost/exception/all.hpp>
#include <boost/optional/optional.hpp>
#include <boost/shared_array.hpp>

#include <scribbu/charsets.hh>
#include <scribbu/errors.hh>
#include <scribbu/framesv2.hh>

/**
 * \page scribbu_id3v2 ID3v2 Tags
 *
 * \section scribbu_id3v2_intro Introduction
 *
 * The \ref scribbu_id3v1 "IDv1"  tag had obvious limitations, leading
 * to the introduction of ID3v2  by Martin Nilsson & Michael Mutschler
 * in 1998 \ref  scribbu_id3v2_refs_2 "[2]". Despite the  name, it has
 * almost nothing  in common with  ID3v1: "ID3v2 tags are  of variable
 * size, and usually occur at the  start of the file, to aid streaming
 * media. They consist of a number of frames, each of which contains a
 * piece of metadata. For example,  the TIT2 frame contains the title,
 * and the WOAR frame contains the URL of the artist's website. Frames
 * can be  up to 16MB  in length, while total  tag size is  limited to
 * 256MB. The internationalization problem  was solved by allowing the
 * encoding of strings  not only in ISO-8859-1, but  also in Unicode."
 * \ref scribbu_id3v2_refs_3 "[3]".
 *
 * "There are three versions of ID3v2:
 *
 * - ID3v2.2  was the  first public  version of  ID3v2. It  used three
 *   character frame identifiers  rather than four (TT2  for the title
 *   instead of  TIT2). Most of the  common v2.3 and v2.4  frames have
 *   direct  analogues  in  v2.2.  Now  this  standard  is  considered
 *   obsolete.
 *
 * - ID3v2.3  expanded the  frame identifier  to four  characters, and
 *   added a  number of frames.  A frame can contain  multiple values,
 *   separated with a null byte. This  is the most widely used version
 *   of ID3v2 tags.
 *
 * - ID3v2.4  is  the  latest  version published,  dated  November  1,
 *   2000. Notably,  it allows  textual data to  be encoded  in UTF-8,
 *   which  was  a  common  practice  in  earlier  tags  (despite  the
 *   standard, since it was not  supported yet) because it has several
 *   noticeable advantages over UTF-16. Another new feature allows the
 *   addition of a tag to the end  of the file before other tags (like
 *   ID3v1).
 *
 * Windows  Explorer and  Windows Media  Player cannot  handle ID3v2.4
 * tags in any version, up to and including Windows 10 / Windows Media
 * Player 12. Windows can understand ID3v2 up to and including version
 * 2.3." \ref scribbu_id3v2_refs_3 "[3]".
 *
 * \section scribbu_id3v2_discuss Discussion
 *
 * \subsection scribbu_id3v2_discuss_header Header
 *
 * ID3v2 tags of any version begin with a ten byte header:
 *
 \code

   +--------------------+-----------+---+
   |ID3/file identifier |   "ID3"   | 3 |
   +--------------------+-----------+---+
   |ID3 version/revision|   $xx yy  | 2 |
   +--------------------+-----------+---+
   |ID3 flags           | %xxxxxxxx | 1 |
   +--------------------+-----------+---+
   |ID3 size            |4*%0xxxxxxx| 4 |
   +--------------------+-----------+---+

 \endcode
 *
 * The version/revision values corresponding  to different versions of
 * the specification are documented below.
 *
 * The flags are version-specific, and documented below.
 *
 * "The  ID3  tag  size  is  the   size  of  the  complete  tag  after
 * unsychronisation,  including padding,  excluding the  header (total
 * tag  size -  10). The  reason to  use 28  bits (representing  up to
 * 256MB) for  size description is  that we don't  want to run  out of
 * space here." \ref scribbu_id3v2_refs_2 "[2]"
 *
 *
 * \section scribbu_id3v2_unsync Unsynchorisation
 *
 * MPEG decoding software uses a two-byte sentinal value in the input
 * stream to detect the beginning of the audio. MPEG decoding software
 * that is not ID3-aware could mistakenly interpret that value as the
 * beginning of the audio should it happen to occur in the ID3v2
 * header. Unsynchronisation is an optional encoding scheme for the
 * ID3v2 header to prevent that. "Unsynchronisation may only be made
 * with MPEG 2 layer I, II and III and MPEG 2.5 files."
 * \ref scribbu_id3v2_refs_1 "[1]"
 *
 * More specifically, whenever a two byte combination of the form:
 *
 *   %11111111 111xxxxx (or $FF $Ex or $FF Fx)
 *
 * is encountered in an ID3v2 tag to be written to disk, it is
 * replaced with:
 *
 *   %11111111 00000000 111xxxxx
 *
 * and the 'unsynchronisation' flag will be set.
 *
 * This leaves us with an ambiguous situation on read: if we encounter
 * a bit pattern
 *
 *   %11111111 00000000 111xxxxx
 *
 * when reading a tag with the unsynchronisation flag set, we have no
 * way to know whether that was a false sync that was unsynchronised
 * (and so the three bytes should be interpreted as %11111111
 * 111xxxxx) or whether those three bytes had occurred naturally in
 * the tag when it was written. To resolve this, on encoding with
 * unsynchronisation all two-byte sequences of the form $FF 00 should
 * also be written as $FF 00 00.
 *
 * In ID3v2.2, this is clear. The ten byte header is "sync-safe" by
 * definition, so it can be read without any additional
 * interpretation, and if the unsynchronisation flag is set, all
 * two-byte sequences of the form $FF 00 should be interpreted as just
 * $FF.
 *
 * ID2v2.3 introduced an extended header, which is *not* sync-safe,
 * and padding, which *is* (padding bytes must always be $00). In
 * section 3.2 \ref scribbu_id3v2_refs_4 "[4]" the specification
 * explicitly notes that the extended header is subject to
 * unsynchronisation.
 *
 * ID3v2.4 introduced a different extended header and a footer, both
 * of which *are* sync-safe, and frames can be unsynhronised
 * individually. The unsynchronisation flag in the header being set
 * indicates that all frames are unsynchronised; unset in the header
 * means that at least one frame is *not* unsynchronised.
 *
 *
 * \section scribbu_id3v2_refs References
 *
 * - \anchor scribbu_id3v2_refs_1 Martin Nilsson, cited 2015: ID3 tag
 *   version 2 [Availbale online at http://id3.org/id3v2-00]
 *
 * - \anchor scribbu_id3v2_refs_2 Martin Nilsson, cited 2015:
 *   Contributors [Available online at http://id3.org/Contributors]
 *
 * - \anchor scribbu_id3v2_refs_3 Unknown, cited 2015:
 *   ID3v2 [Available online at https://en.wikipedia.org/wiki/ID3#ID3v2]
 *
 * - \anchor scribbu_id3v2_refs_4 Martin Nilsson, cited 2015: ID3 tag
 *   version 2.4.0 - Main Structure [Availbale online at
 *   http://id3.org/id3v2.4.0-structure]
 *
 *
 */

namespace scribbu {

  namespace detail {

    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2);
    std::uint32_t uint32_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3);
    std::uint32_t uint32_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3,
                                        unsigned char b4);
    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3);
    std::uint32_t uint32_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2);
    std::size_t unsigned_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2);
    std::uint32_t uint32_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2,
                                            unsigned char b3);
    std::size_t unsigned_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2,
                                            unsigned char b3);
    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3,
                                        unsigned char b4);
    void sync_safe_from_unsigned(std::size_t x, unsigned char b[]);
  }

  class zlib_error: public virtual boost::exception,
                    public virtual std::runtime_error
  {
  public:
    zlib_error(int status);
  };

  const std::size_t ID3V2_HEADER_SIZE = 10;

  /// Structure tying together all the information returned from
  /// looking_at_id3v2
  struct id3v2_info {
    /// If an ID3v2 tag is present, this will be set to true; if not,
    /// it will be set to false and all other fields are undefined.
    bool          present_;
    unsigned char version_;
    unsigned char revision_;
    unsigned char flags_;
    std::uint32_t size_;
  };

  /**
   * \brief Determine  whether an  input stream  is positioned  at the
   * start of an ID3v2  tag; if it is, return data  found in the ID3v2
   * header
   *
   * \param is [in] the input stream to be probed for an ID3v2 tag
   *
   * \param restore_get_if_found  [in,opt] If the caller  sets this to
   * true (the  default), and an  ID3v2 tag is  present in \a  is, the
   * input  streams get  pointer  will  be reset  to  its position  on
   * entry. If  the caller sets it  to false, the get  pointer will be
   * left positioned ten  bytes further than it was on  entry (i.e. it
   * will be positioned to the first  byte after the ID3v2 header). If
   * no ID3v2  tag is present, the  get pointer is always  restored to
   * where it was on entry to this function.
   *
   * \retrun an id3v2_info struct containing the results
   *
   *
   * This is intended to be used as a peek operation where the
   * caller can avoid a full parse of the tag; it does the following:
   *
   * - read the first ten bytes
   * - if it's not an ID3v2 tag, restore the get ptr & so indicate to
   *   the caller
   * - if it *is* so indicate, along with the version (so that the
   *   caller can construct the corresponding type) and the size (so
   *   the caller can skip the tag, if desired)
   * - optionally restore the get ptr
   *
   *
   */

  id3v2_info
  looking_at_id3v2(std::istream &is, bool restore_get_if_found=true);

  /**
   * \brief Restore false sync signals in a buffer
   *
   *
   * \param p [in] Address of the buffer to be resynchronized; note that the
   * buffer will be resynchronized in-place
   *
   * \param cb [in] The number of bytes in the buffer
   *
   * \return The number of bytes in the resynchronized buffer
   *
   *
   * "Unsynchronisation"   refers   to   the    process   of   removing   false
   * synchronisations when  writing an ID3v2  tag; cf.  sec.  5 of  the ID3v2.3
   * specification, or  \ref scribbu_id3v2_unsync  "here" for  background.  For
   * purposes of  this discussion, resynchronisation  refers to the  process of
   * replacing each two-byte sequence 0xff, 0x00 with just 0xff in the provided
   * buffer.
   *
   * This  implementation   will  perform   the  resynchronisation   in  place;
   * i.e. given an input buffer like so:
   *
   \code

     [0x00, 0x01, 0xff, 0x00, 0x02, 0x03, 0xff, 0x00, 0x04, 0x05, 0xff, 0x00, 0x06, 0x07]

   \endcode
   *
   * this function will alter the buffer to this:
   *
   \code

     [0x00, 0x01, 0xff, 0x02, 0x03, 0xff, 0x04, 0x05, 0xff, 0x06, 0x07, 0x00, 0x06, 0x07]

   \endcode
   *
   * and return a value of eleven.
   *
   * I chose this interface for two reasons,  the first being that it avoids an
   * allocation  &  the  second  being  that the  copies  follow  a  particular
   * pattern. In this example, we copy the following ranges:
   *
   * - [4,7) -> [3,6)
   * - [8,11) -> [6,9)
   * - [12,14) -> [9,11)
   *
   * Note  that for  each successive  false sync  being restored,  we can  copy
   * successively  larger  & larger  chunks  at  each  time, leaving  open  the
   * possibility for optimization through loop unrolling, SSE, &c.
   *
   *
   */

  std::size_t resynchronise(unsigned char *p, std::size_t cb);

  /**
   * \class id3v2_tag
   *
   * \brief Core ID3v2 functionality
   *
   *
   * Class id3v2_tag handles some functionality common to all ID3v2 tags, such
   * as version/revision information & the unsynch attribute, but it mostly
   * defines an interface to which all concrete ID3v2 tags can conform,
   * enabling client code to work in a version-independent way (e.g. a client
   * should be able to ask for the "artist" without having to worry about
   * whether the tag is ID3v2.2, 2.3 or 2.4).
   *
   *
   */

  class id3v2_tag {

  public:

    /// Some frames are constrained by the standard to be unique
    class duplicate_frame_error: public error
    {
    public:
      duplicate_frame_error(const frame_id3 &id, std::size_t n):
        id3_(id), n_(n)
      { }
      duplicate_frame_error(const frame_id4 &id, std::size_t n):
        id4_(id), n_(n)
      { }

    public:
      virtual const char * what() const noexcept(true);

    private:
      frame_id3 id3_;
      frame_id4 id4_;
      std::size_t n_;
      mutable std::shared_ptr<std::string> pwhat_;

    };

    class unknown_frame_error: public error
    {
    public:
      unknown_frame_error(const frame_id3 &id): id3_(id)
      { }
      unknown_frame_error(const frame_id4 &id): id4_(id)
      { }

    public:
      virtual const char * what() const noexcept(true);

    private:
      frame_id3 id3_;
      frame_id4 id4_;
      mutable std::shared_ptr<std::string> pwhat_;

    };

    class reserved_frame_error: public error
    {
    public:
      reserved_frame_error(const frame_id3 &id): id3_(id)
      { }
      reserved_frame_error(const frame_id4 &id): id4_(id)
      { }

    public:
      virtual const char * what() const noexcept(true);

    private:
      frame_id3 id3_;
      frame_id4 id4_;
      mutable std::shared_ptr<std::string> pwhat_;

    };

    class invalid_tag: public error
    {
    public:
      invalid_tag()
      { }

    public:
      virtual const char * what() const noexcept;

    };

    class no_tag: public error
    {
    public:
      no_tag()
      { }

    public:
      virtual const char * what() const noexcept;

    };

    class unknown_version: public error
    {
    public:
      unknown_version(unsigned char version):
        version_(version)
      { }

    public:
      unsigned char get_version() const {
        return version_;
      }
      virtual const char * what() const noexcept;

    private:
      unsigned char version_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    typedef scribbu::encoding encoding;

  public:
    /// Initialize from the first five bytes of \a is
    id3v2_tag(std::istream &is);
    /// Initialize from an id3v2_info
    id3v2_tag(const id3v2_info &H);

  public:

    /////////////////////////////////////////////////////////////////////////////
    //                      Common ID3v2 Attributes                            //
    /////////////////////////////////////////////////////////////////////////////

    unsigned char version() const {
      return version_;
    }
    unsigned char revision() const {
      return revision_;
    }
    bool unsynchronised() const {
      return unsync_;
    }
    void unsynchronised(bool f) {
      unsync_ = f;
    }

    /////////////////////////////////////////////////////////////////////////////
    //                          ID3v2 Serialization                            //
    /////////////////////////////////////////////////////////////////////////////

    virtual unsigned char flags() const = 0;
    virtual std::size_t size(bool unsync = true) const = 0;
    virtual bool needs_unsynchronisation() const = 0;
    virtual std::size_t write(std::ostream &os, bool unsync = true) const = 0;

    /////////////////////////////////////////////////////////////////////////////
    //                    Frames Common to all ID3v2 Tags                      //
    /////////////////////////////////////////////////////////////////////////////

    virtual
    std::string
    album(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src =
           boost::none) const = 0;
    virtual
    std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src =
                   boost::none) const = 0;
    virtual
    std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::size_t play_count() const = 0;
    virtual
    std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const = 0;

    virtual std::size_t has_album() const = 0;
    virtual std::size_t has_artist() const = 0;
    virtual std::size_t has_content_type() const = 0;
    virtual std::size_t has_encoded_by() const = 0;
    virtual std::size_t has_languages() const = 0;
    virtual std::size_t has_play_count() const = 0;
    virtual std::size_t has_title() const = 0;
    virtual std::size_t has_track() const = 0;
    virtual std::size_t has_year() const = 0;

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
           bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail) = 0;

  protected:
    std::pair<unsigned char, std::size_t>
    parse_flags_and_size(std::istream &is);

  private:
    unsigned char version_;
    unsigned char revision_;
    bool unsync_;

  }; // End class id3v2_tag.

} // End namespace scribbu.

#endif // not ID3V2_HH_INCLUDED
