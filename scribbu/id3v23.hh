/**
 * \file id3v23.hh
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

#ifndef ID3V23_HH_INCLUDED
#define ID3V23_HH_INCLUDED 1

#include <cstddef>

#include <scribbu/errors.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/framesv23.hh>

namespace scribbu {

  /**
   * \class id3v2_3_tag
   *
   * \brief Represents an ID3v2.3 tag
   *
   * \sa id3v2_tag
   *
   *
   * Schematically, and ID3v2.3 tag can be represented as follows:
   *
   \code

     +-----------------------------+
     |      Header (10 bytes)      |
     +-----------------------------+
     |       Extended Header       |
     | (variable length, OPTIONAL) |
     +-----------------------------+
     |   Frames (variable length)  |
     +-----------------------------+
     |           Padding           |
     | (variable length, OPTIONAL) |
     +-----------------------------+

   \endcode
   *
   * In greater detail:
   *
   \code

   | field                | representation     | bytes |                     |
   |----------------------+--------------------+-------+---------------------|
   | ID3/file identifier  | "ID3"              |     3 | ID3v2 header        |
   | ID3 version/revision | $03 00             |     2 |                     |
   | ID3 flags            | %xyz00000          |     1 |                     |
   | ID3 size             | 4*%0xxxxxxx        |     4 |                     |
   |----------------------+--------------------+-------+---------------------|
   | Extended header size | $xx xx xx xx       |     4 | ID3v2.3 Ext. header |
   | Extended Flags       | %x0000000 00000000 |     2 |                     |
   | Size of padding      | $xx xx xx xx       |     4 |                     |
   | CRC Checksum         | $xx xx xx xx       |     4 | (optional)          |
   |----------------------+--------------------+-------+---------------------|
   | Frame ID             | $xx xx xx xx       |     4 |                     |
   | Size                 | $xx xx xx xx       |     4 |                     |
   | Flags                | $xx xx             |     2 |                     |
   | Decompressed Size    | $xx xx xx xx       |     4 | (optional)          |
   | Encryption Method    | $xx                |     1 | (optional)          |
   | Group Identifier     | $xx                |     1 | (optional)          |
   | Frame data           |                    |       |                     |
   |----------------------+--------------------+-------+---------------------|
   | padding              | 00                 |     * |                     |

   \endcode
   *
   * Flags:
   *
   *   - x (bit 7): unsynchornisation was applied (see \ref scribbu_id3v2_unsync "here")
   *   - y (bit 6): extended header is present (on which more below)
   *   - z (bit 5): experimental ("This flag should always be set when
   *     the tag is in an experimental stage.")
   *
   * The Optional Extended Header:
   *
   \code

     | field              |   representation | bytes |
     |--------------------+------------------+-------|
     |Extended header size|     $xx xx xx xx |     4 |
     |--------------------+------------------+-------|
     |Extended Flags      |%x0000000 00000000|     2 |
     |--------------------+------------------+-------|
     |Size of padding     |     $xx xx xx xx |     4 |
     |--------------------+------------------+-------|

   \endcode

     Flags:

       - x (bit 15): CRC data present

   \endcode
   *
   * Note that the extended header size & size of padding are \em not
   * sync-safe-- section 3.2 \ref scribbu_id3v2_refs_4 "[4]" of the
   * specification explicitly notes that the extended header is subject to
   * unsynchronisation.
   *
   * CRC data is a four byte CRC32 checksum appended to the extended header;
   * the checksum is calculated before unsynchronization on the data between
   * the extended header & the padding (i.e. just the frames).
   *
   *
   * \todo Implement insert overloads for id3v2_3_frame&&,
   * initializer_list<id3v2_3_frame> & ranges (i.e. two iterators).
   *
   *
   */

  class id3v2_3_tag: public id3v2_tag {

  public:

    ///////////////////////////////////////////////////////////////////////////////
    //                             Nested Types                                  //
    ///////////////////////////////////////////////////////////////////////////////

    class invalid_ext_header: public error
    {
    public:
      virtual const char * what() const noexcept;
    };

    //// ID3v2.3 extended header
    class ext_header {

    private:

      static const std::size_t SERIALIZED = 0;
      static const std::size_t SERIALIZED_WITH_UNSYNC = 1;

    public:
      /// Construct from a (resynchronised) serialization
      ext_header(const unsigned char *p0, const unsigned char *p1);
      /// Construct a fresh, new extended header; if a CRC is desired, pass the tag
      /// whose frames are to be checksummed
      ext_header(std::size_t cbpad, const id3v2_3_tag *ptag_for_crc = nullptr);

    public:
      /// Compute the size of this extended header
      std::size_t size() const
      { return has_crc() ? 10 : 6; }
      /// Compute the size on disk
      std::size_t serialized_size(bool unsync = false) const;
      /// True if the serialized form would contain false syncs
      std::size_t needs_unsynchronisation() const;
      /// Determine whether this header has a CRC
      bool has_crc() const
      { return (bool) crc_; }
      /// Update/compute the CRC checksum for this header
      std::uint32_t crc(const id3v2_3_tag &tag) const;
      /// Retrieve the CRC-- will throw if we have no CRC
      std::uint32_t crc() const
      { return *crc_; }
      std::size_t padding_size() const
      { return cbpad_; }
      /// Write this header to an ostream without unsynchronisation; if this header
      /// is configured to include a CRC, it will be updated based on \a tag
      std::size_t write(std::ostream &os, bool unsync = false) const;

    private:

      void ensure_cached_data_is_fresh() const;

    private:
      std::size_t cbpad_;
      mutable boost::optional<std::uint32_t> crc_;
      mutable bool dirty_;
      mutable std::size_t num_false_syncs_;
      mutable std::vector<unsigned char> cache_[2];

    };

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             Construction                              //
    ///////////////////////////////////////////////////////////////////////////

    /// Read an ID3v2.3 tag from \a is
    id3v2_3_tag(std::istream &is);
    /// Read an ID3v2.3 tag once it's header has already been read into \a H
    id3v2_3_tag(std::istream &is, const id3v2_info &H);

    enum class want_extended_header { none, present, with_crc };

    id3v2_3_tag(std::size_t cbpad = 0, bool fexp = false,
                want_extended_header ext = want_extended_header::none);
    id3v2_3_tag(const id3v2_3_tag &that);
    virtual id3v2_tag* clone() const
    { return new id3v2_3_tag(*this); }
    id3v2_3_tag& operator=(const id3v2_3_tag &that);

  public:

    /// Retrieve this tag's ID3v2 flags; the exact meaning of each bit will
    /// depend on the particular ID3v2 version
    virtual unsigned char flags() const;

    /////////////////////////////////////////////////////////////////////////////
    //                          ID3v2 Serialization                            //
    /////////////////////////////////////////////////////////////////////////////

    /// Compute the size of this tag (in bytes) exclusive of the ID3v2 header
    /// (i.e. return the total tag size, in bytes, less ten)
    virtual std::size_t size(bool unsync = false) const;
    /// Return true if this the serialization of this tag would contain false
    /// syncs if serialized in its present state
    virtual bool needs_unsynchronisation() const;
    /// Serialize this tag to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised"
    /// will be updated accordingly)
    virtual std::size_t write(std::ostream &os, bool unsync = false) const;

  public:

    /////////////////////////////////////////////////////////////////////////////
    //                  attributes common to all ID3v2 tags                    //
    /////////////////////////////////////////////////////////////////////////////

    virtual std::size_t num_frames() const
    { return frames_.size(); }
    virtual std::size_t padding() const
    { return padding_; }
    virtual void padding(std::size_t padding)
    { padding_ = padding; }

    /////////////////////////////////////////////////////////////////////////////
    //                    Frames Common to all ID3v2 Tags                      //
    /////////////////////////////////////////////////////////////////////////////

    virtual std::string
    album(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TALB", dst, rsp, src); }
    virtual std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TPE1", dst, rsp, src); }
    virtual std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TCON", dst, rsp, src); }
    virtual std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TENC", dst, rsp, src); }
    virtual std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TLAN", dst, rsp, src); }
    virtual
    std::size_t play_count() const;
    virtual std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TIT2", dst, rsp, src); }
    virtual std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TRCK", dst, rsp, src); }
    virtual std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TYER", dst, rsp, src); }

    virtual std::size_t has_album() const
    { return frame_map_.count("TALB"); }
    virtual std::size_t has_artist() const
    { return frame_map_.count("TPE1"); }
    virtual std::size_t has_content_type() const
    { return frame_map_.count("TCON"); }
    virtual std::size_t has_encoded_by() const
    { return frame_map_.count("TENC"); }
    virtual std::size_t has_languages() const
    { return frame_map_.count("TLAN"); }
    virtual std::size_t has_play_count() const
    { return frame_map_.count("PCNT"); }
    virtual std::size_t has_title() const
    { return frame_map_.count("TIT2"); }
    virtual std::size_t has_track() const
    { return frame_map_.count("TRCK"); }
    virtual std::size_t has_year() const
    { return frame_map_.count("TYER"); }

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TALB", text, src, add_bom, rsp); }
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
           bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TPE1", text, src, add_bom, rsp); }
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TCON", text, src, add_bom, rsp); }
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TENC", text, src, add_bom, rsp); }
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TLAN", text, src, add_bom, rsp); }
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TIT2", text, src, add_bom, rsp); }
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TRCK", text, src, add_bom, rsp); }
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TYER", text, src, add_bom, rsp); }

    virtual void
    add_comment(const std::string &text,
                language lang = language::from_locale,
                encoding src = encoding::UTF_8,
                use_unicode unicode = use_unicode::no,
                const std::string &description = std::string(),
                on_no_encoding rsp = on_no_encoding::fail);

    virtual void
    add_user_defined_text(const std::string &text,
                          encoding src = encoding::UTF_8,
                          use_unicode unicode = use_unicode::no,
                          const std::string &dsc = std::string(),
                          on_no_encoding rsp = on_no_encoding::fail);

    /// Retrieve the contents of an arbitrary text frame
    virtual
    std::string
    text(id3v2_text_frames id,
         encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const;
    /// Set the contents of an arbitrary text frame; will add the relevant
    /// frame to this tag if it doesn't already exist
    virtual
    void
    text(id3v2_text_frames id,
         const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail);
    /// Delete an arbitrary text frame
    virtual
    void
    delete_frame(id3v2_text_frames id);

    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

    bool experimental() const
    { return experimental_; }
    bool has_extended_header() const
    { return (bool) pext_header_; }
    ext_header extended_header() const
    { return *pext_header_; }
    std::size_t has_frame(const frame_id4 &id) const
    { return frame_map_.count(id); }

    const id3v2_3_frame& get_frame(const frame_id4 &id) const
    {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const
    {
      using namespace std;
      return transform(comms_.begin(), comms_.end(), p,
                       [](const pair<const COMM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const
    {
      using namespace std;
      return transform(pcnts_.begin(), pcnts_.end(), p,
                       [](const pair<const PCNT*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const
    {
      using namespace std;
      return transform(popms_.begin(), popms_.end(), p,
                       [](const pair<const POPM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    ///////////////////////////////////////////////////////////////////////////
    //                           tag as container                            //
    ///////////////////////////////////////////////////////////////////////////

  private:

    typedef
    std::vector<std::unique_ptr<id3v2_3_frame>> frames_type;
    friend class mutable_frame_proxy;

  public:

    /**
     * \class mutable_frame_proxy
     *
     * \brief Proxy for an id3v2_3_frame returned when a mutable frame iterator
     * is dereferenced
     *
     *
     * \todo Implement functions on id3v2_3_tag::mutable_frame_proxy forwarding
     * to id3v_3_frame public methods
     *
     *
     */

    class mutable_frame_proxy
    {
    public:
      /// Construct with the owning tag & index
      mutable_frame_proxy(id3v2_3_tag *p, std::size_t idx): p_(p), idx_(idx)
      { }
      // Deleted until neded
      mutable_frame_proxy(const mutable_frame_proxy &) = delete;
      /// Used when we dereference mutable_frame_iterators (i.e. operator*, ->, [])
      mutable_frame_proxy(const mutable_frame_proxy &&that):
        p_(that.p_), idx_(that.idx_)
      { }
      // Deleted until needed
      mutable_frame_proxy& operator=(const mutable_frame_proxy &) = delete;
      // Used when algorithms move assign one iterator to another (remove, remove_if)
      mutable_frame_proxy& operator=(mutable_frame_proxy &&that);

      // We overload assignment to keep the tag's ancillary datastructures up-to-date
      mutable_frame_proxy& operator=(const id3v2_3_frame &frame);
      mutable_frame_proxy& operator=(const id3v2_3_text_frame &frame);
      mutable_frame_proxy& operator=(const PCNT &frame);
      mutable_frame_proxy& operator=(const COMM &frame);
      mutable_frame_proxy& operator=(const POPM &frame);

      /// This is needed to enable expressions like p->something when p is a
      /// mutable_iterator
      id3v2_3_frame* operator->() const {
        return p_->frames_[idx_].get();
      }
      /// Needed to enable expressions like (*p).something...
      operator id3v2_3_frame&() const {
        return *(p_->frames_[idx_]);
      }

    private:
      id3v2_3_tag *p_;
      std::size_t idx_;

    }; // End class mutable_frame_proxy

    typedef id3v2_tag::frame_iterator_base<id3v2_3_tag::frames_type::iterator> iterator_base;
    friend id3v2_tag::frame_iterator_base<id3v2_3_tag::frames_type::iterator>;

    typedef
    id3v2_tag::frame_iterator<id3v2_3_tag, id3v2_3_frame, mutable_frame_proxy,
                              id3v2_3_tag::frames_type::iterator>
    iterator;

    friend
    id3v2_tag::frame_iterator<id3v2_3_tag, id3v2_3_frame, mutable_frame_proxy,
                              id3v2_3_tag::frames_type::iterator>;

    typedef
    id3v2_tag::const_frame_iterator<id3v2_3_frame, id3v2_3_tag::frames_type::iterator>
    const_iterator;

    friend
    id3v2_tag::const_frame_iterator<id3v2_3_frame, id3v2_3_tag::frames_type::iterator>;


    iterator begin()
    { return iterator(this, frames_.begin()); }
    iterator end()
    { return iterator(this, frames_.end()); }

    const_iterator begin() const {
      id3v2_3_tag &me = const_cast<id3v2_3_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }

    const_iterator end() const {
      id3v2_3_tag &me = const_cast<id3v2_3_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    const_iterator cbegin() const {
      id3v2_3_tag &me = const_cast<id3v2_3_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }
    const_iterator cend() const {
      id3v2_3_tag &me = const_cast<id3v2_3_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    iterator
    insert(const_iterator p, const id3v2_3_frame &frame);

    iterator
    insert(const_iterator p, const id3v2_3_text_frame &frame);
    iterator
    insert(const_iterator p, const PCNT &frame);
    iterator
    insert(const_iterator p, const COMM &frame);
    iterator
    insert(const_iterator p, const POPM &frame);

    void
    push_back(const id3v2_3_frame &frame);
    void
    push_back(const id3v2_3_text_frame &frame);
    void
    push_back(const PCNT &frame);
    void
    push_back(const COMM &frame);
    void
    push_back(const POPM &frame);

    /// Remove the frame at the given position; return a mutable frame iterator
    /// pointing to the next element (or end())
    iterator
    erase(const_iterator p);

    /// Remove the frames in the range [p0, p1); return a mutable frame
    /// iterator pointing to the next element (or end())
    iterator
    erase(const_iterator p0, const_iterator p1);

    /// Write an ID3v2.3 header
    std::ostream& write_header(std::ostream &os, unsigned char flags, std::size_t cb) const;

    /// Write an ID3v2.3 extended header
    std::ostream& write_extended_header(std::ostream &os) const;

  private:

    /// Remove an arbitrary frame from our ancillary datastructures; on return,
    /// it will remain in the frame vector
    void remove_frame_from_lookups(const frame_id4 &id, std::size_t idx);
    void add_frame_to_lookups(const id3v2_3_frame &frame, std::size_t idx);
    void add_frame_to_lookups(id3v2_3_text_frame &frame, std::size_t idx);
    void add_frame_to_lookups(PCNT &frame, std::size_t idx);
    void add_frame_to_lookups(COMM &frame, std::size_t idx);
    void add_frame_to_lookups(POPM &frame, std::size_t idx);

  public:
    ///////////////////////////////////////////////////////////////////////////
    //                             frame parsing                             //
    ///////////////////////////////////////////////////////////////////////////

    typedef id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only
    read_only;

    /// Convenience typedef for a functor taking an ID3v2.3 frame ID and a
    /// buffer producing & an id3v2_3_frame.
    typedef std::function<std::unique_ptr<id3v2_3_frame>
                          (const frame_id4&,
                           const unsigned char*,
                           std::size_t,
                           tag_alter_preservation,
                           file_alter_preservation,
                           read_only,
                           const boost::optional<unsigned char>&,
                           const boost::optional<unsigned char>&,
                           const boost::optional<std::size_t>&)>
    generic_frame_parser;

    /// Convenience typedef for a functor taking an ID3v2.3 frame ID and a
    /// buffer producing & an id3v2_3_text_frame.
    typedef std::function<std::unique_ptr<id3v2_3_text_frame>
                          (const frame_id4&,
                           const unsigned char*,
                           std::size_t,
                           tag_alter_preservation,
                           file_alter_preservation,
                           read_only,
                           const boost::optional<unsigned char>&,
                           const boost::optional<unsigned char>&,
                           const boost::optional<std::size_t>&)>
    text_frame_parser;

    /// Retrieve a copy of the default set of generic frame parsers--
    /// thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator
    get_default_generic_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_generic_parsers_.begin(),
                       default_generic_parsers_.end(),
                       p);
    }

    /// Retrieve a copy of the default set of text frame parsers--
    /// thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator
    get_default_text_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_text_parsers_.begin(),
                       default_text_parsers_.end(),
                       p);
    }

    /// true => F is a new generic frame parser, false => another was replaced
    static bool
    register_default_frame_parser(const frame_id4 &id,
                                  const generic_frame_parser &F);

    /// true => F is a new text frame parser, false => another was replaced
    static bool
    register_default_frame_parser(const frame_id4 &id,
                                  const text_frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator
    get_generic_frame_parsers(forward_output_iterator p) {
      return std::copy(generic_parsers_.begin(), generic_parsers_.end(), p);
    }

    /// Not thread-safe
    bool
    register_generic_frame_parser(const frame_id4 &id,
                                  const generic_frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator
    get_text_frame_parsers(forward_output_iterator p) {
      return std::copy(text_parsers_.begin(), text_parsers_.end(), p);
    }

    bool
    register_text_frame_parser(const frame_id4 &id,
                               const text_frame_parser &F);

    // Give every translation unit a static initializer; see below.
    struct static_initializer {
      static_initializer();
      ~static_initializer();
    };
    friend struct static_initializer;

  private:

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decompress(const unsigned char *p,
               std::size_t cb, std::size_t
               uncompressed_size) const;

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decrypt(const unsigned char *p,
            std::size_t cb,
            unsigned char method) const;

    bool encryption_method_regd(unsigned char method) const {
      return 0 != encryption_methods_.count(method);
    }

    /// Returns true if the parser for the given frame ID may not be
    /// replaced
    static bool parsing_is_reserved(const frame_id4 &id);

    /// Parse an ID3v2.3 tag after the standard ten-byte header from an input
    /// stream
    void parse(std::istream &is, std::size_t size, bool extended);

    void
    parse_frame(const frame_id4 &id,
                id3v2_3_frame::tag_alter_preservation tap,
                id3v2_3_frame::file_alter_preservation fap,
                id3v2_3_frame::read_only ro,
                const boost::optional<unsigned char> &enc,
                const boost::optional<unsigned char> &gid,
                const boost::optional<std::size_t> &desz,
                const unsigned char * p0,
                const unsigned char * p1);

    void register_encryption_method(const ENCR &encr);
    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string
    text_frame_as_str(
      const frame_id4 &id,
      encoding dst = encoding::UTF_8,
      on_no_encoding rsp = on_no_encoding::fail,
      const boost::optional<encoding> &src = boost::none) const;
    /// Replace a text frame if it exists, append it otherwise
    void set_text_frame(
      const frame_id4 &id,
      const std::string &text,
      encoding src = encoding::UTF_8,
      bool add_bom = false,
      on_no_encoding rsp = on_no_encoding::fail);

  private:

    // Nifty Counter Idiom...
    static std::mutex& mutex_;
    static std::unordered_map<frame_id4, generic_frame_parser>
    &default_generic_parsers_;
    static std::unordered_map<frame_id4, text_frame_parser>
    &default_text_parsers_;

    typedef
    std::unordered_map<frame_id4, generic_frame_parser>
    generic_parser_map_type;

    typedef
    std::unordered_map<frame_id4, text_frame_parser>
    text_parser_map_type;

    typedef
    std::unordered_multimap<frame_id4, std::ptrdiff_t>
    frame_lookup_type;

    typedef
    std::unordered_multimap<frame_id4, id3v2_3_text_frame*>
    text_frame_lookup_type;

    typedef
    std::vector<std::pair<const COMM*, std::size_t>>
    comm_frame_lookup_type;

    typedef
    std::vector<std::pair<const PCNT*, std::size_t>>
    pcnt_frame_lookup_type;

    typedef
    std::vector<std::pair<const POPM*, std::size_t>>
    popm_frame_lookup_type;

    bool experimental_;
    generic_parser_map_type generic_parsers_;
    text_parser_map_type text_parsers_;
    std::shared_ptr<ext_header> pext_header_;
    std::size_t padding_;
    comm_frame_lookup_type comms_;
    pcnt_frame_lookup_type pcnts_;
    popm_frame_lookup_type popms_;
    std::unordered_map<unsigned char, ENCR> encryption_methods_;
    frames_type frames_;
    frame_lookup_type frame_map_;
    /// index: frame id to text frame (spec guarantees only one per id)
    text_frame_lookup_type text_map_;

  }; // End class id3v2_3_tag.

  static id3v2_3_tag::static_initializer id3v2_3_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V23_HH_INCLUDED
