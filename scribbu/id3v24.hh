/**
 * \file id3v24.hh
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

#ifndef ID3V24_HH_INCLUDED
#define ID3V24_HH_INCLUDED 1

#include <scribbu/id3v2.hh>
#include <scribbu/framesv24.hh>

namespace scribbu {

  /**
   * \class id3v2_4_tag
   *
   * \brief Represents an ID3v2.4 tag
   *
   * \sa id3v2_tag
   *
   * Schematically, an ID3 v2.4 can be represented as follows:
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
     | Footer (10 bytes, OPTIONAL) |
     +-----------------------------+

   \endcode
   *
   * Header:
   *
   \code

     | field              | representation | bytes |
     |--------------------+----------------+-------|
     |ID3/file identifier |          "ID3" |     3 |
     |--------------------+----------------+-------|
     |ID3 version/revision|        $04 00  |     2 |
     |--------------------+----------------+-------|
     |ID3 flags           |      %abcd0000 |     1 |
     |--------------------+----------------+-------|
     |ID3 size            |     4*%0xxxxxxx|     4 |
     |--------------------+----------------+-------|

   \endcode
   *
   * Flags:
   *
   *   - a (bit 7): unsynchronisation
   *   - b (bit 6): extended header present
   *   - c (bit 5): experimental indicator ("This flag SHALL always be set when the tag is
   *     in an experimental stage."
   *   - d (bit 4): footer present
   *
   *
   * ID3v2.4 Optional Extended Header:
   *
   \code

     | field              | representation | bytes |
     |--------------------+----------------+-------|
     |Extended header size|    4*%0xxxxxxx |     4 |
     |--------------------+----------------+-------|
     |Number of flag bytes|        $01     |     1 |
     |--------------------+----------------+-------|
     |Extended Flags      |     %0bcd0000  |     1 |
     |--------------------+----------------+-------|

   \endcode
   *
   * Extended Flags:
   *
   *  - b (bit 6): Tag is an update of a tag found earlier in the present file or stream
   *  - c (bit 5): CRC data present (on which more below)
   *  - d (bit 4): Tag restrictions are in effect
   *
   * "Each flag that is set in the extended header has data attached, which
   *  comes in the order in which the flags are encountered (i.e. the data for
   *  flag 'b' comes before the data for flag 'c').  Unset flags cannot have
   *  any attached data."  \ref scribbu_id3v2_refs_4 "[4]"
   *
   *   - "Tag is an update" flag data length: 0
   *   - "CRC data present" flag data length: $05 (5 * %0xxxxxxx)
   *   - "Tag restrictions" flag data length: $01
   *
   * Footer:
   *
   \code

     | field                | representation | bytes |
     |----------------------+----------------+-------|
     |    ID3v2 identifier  |          "3DI" |     3 |
     |----------------------+----------------+-------|
     |ID3v2 version/revision|      $04 00    |     2 |
     |----------------------+----------------+-------|
     |          ID3v2 flags |      %abcd0000 |     1 |
     |----------------------+----------------+-------|
     |           ID3v2 size |    4*%0xxxxxxx |     4 |
     |----------------------+----------------+-------|

   \endcode
   *
   * If the "CRC data present" flag is set, a CRC-32 [ISO-3309] data
   * is included in the extended header. The CRC is calculated on all
   * the data between the header and footer as indicated by the
   * header's tag length field, minus the extended header. Note that
   * this includes the padding (if there is any), but excludes the
   * footer. The CRC-32 is stored as an 35 bit synchsafe integer,
   * leaving the upper four bits always zeroed.
   *
   *
   * \todo Implement insert overloads for id3v2_4_frame&&,
   * initializer_list<id3v2_4_frame> & range (i.e. two iterators)
   *
   *
   */

  class id3v2_4_tag: public id3v2_tag {

  public:

    ///////////////////////////////////////////////////////////////////////////////
    //                             Nested Types                                  //
    ///////////////////////////////////////////////////////////////////////////////

    class invalid_ext_header: public error
    {
    public:
      virtual const char * what() const noexcept;
    };

    typedef id3v2_3_plus_frame::tag_alter_preservation tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    enum class tag_size {
      restricted,          // No more than 128 frames and 1 MB total tag
                           // size.
      more_restricted,     // No more than 64 frames and 128 KB total
                           // tag size.
      very_restricted,     // No more than 32 frames and 40 KB total tag
                           // size.
      extremely_restricted // No more than 32 frames and 4 KB total
                           // tag size.
    };

    enum class text_size {
      unrestricted,     // No restrictions
      restricted,       // No string is longer than 1024 characters.
      more_restricted,  // No string is longer than 128 characters.
      very_restricted   // No string is longer than 30 characters.
    };

    enum class image_size {
      unrestricted,    // No restrictions
      restricted,      // All images are 256x256 pixels or smaller.
      more_restricted, // All images are 64x64 pixels or smaller.
      very_restricted  // All images are exactly 64x64 pixels,
                       // unless required otherwise.
    };

    /// ID3v2.4 extended header
    class ext_header {

    public:
      /// Construct from a (resynchronised) serialization
      ext_header(const unsigned char *p0, const unsigned char *p1);

    public:
      //// Retrieve the size of this extended header-- will throw if the size
      //// calculation is dirty
      std::size_t size() const;
      bool is_update() const
      { return is_update_; }
      bool has_crc() const
      { return fcrc_; }
      std::uint32_t crc() const;
      std::uint32_t crc(const id3v2_4_tag &tag) const;
      bool restricted() const
      { return restricted_; }
      std::tuple<tag_size, bool, text_size, bool, image_size>
      get_restrictions() const;
      std::size_t write(std::ostream &os, const id3v2_4_tag &tag) const;

    private:
      mutable bool size_dirty_;
      mutable std::size_t size_;
      bool is_update_;
      bool fcrc_;
      mutable bool crc_dirty_;
      mutable std::uint32_t crc_;
      bool restricted_;
      tag_size tag_size_restriction_;
      bool text_enc_restriction_;
      text_size text_size_restriction_;
      bool image_enc_restriction_;
      image_size image_sz_restriction_;

    };

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             Construction                              //
    ///////////////////////////////////////////////////////////////////////////

    /// Read an ID3v2.3 tag from \a is
    id3v2_4_tag(std::istream &is);
    /// Read an ID3v2.3 tag once it's header has already been read into \a H
    id3v2_4_tag(std::istream &is, const id3v2_info &H);
    /// Instantiate an ID3v2.4 tag "from scratch"
    id3v2_4_tag(std::size_t cbpad = 0, bool exp = false,
                bool footer = false);
    id3v2_4_tag(std::size_t cbpad = 0, bool exp = false, bool footer = false,
                bool update = false, bool crc = false, bool restricted = false,
                tag_size tsr = tag_size::restricted, bool ter = false,
                text_size tzr = text_size::unrestricted,
                bool ier = false, image_size izr = image_size::unrestricted);

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
    /// Return true if all frames would contain false syncs if serialized in
    /// their present state
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

  public:

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
    { return frame_map_.count("CNT"); }
    virtual std::size_t has_title() const
    { return frame_map_.count("TIT2"); }
    virtual std::size_t has_track() const
    { return frame_map_.count("TRCK"); }
    virtual std::size_t has_year() const
    { return frame_map_.count("TYER"); }

    const id3v2_4_text_frame::frame_encoding DST =
      id3v2_4_text_frame::frame_encoding::UTF_8;

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TALB", text, src, DST, add_bom, rsp); }
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
           bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TPE1", text, src, DST, add_bom, rsp); }
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TCON", text, src, DST, add_bom, rsp); }
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TENC", text, src, DST, add_bom, rsp); }
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TLAN", text, src, DST, add_bom, rsp); }
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TIT2", text, src, DST, add_bom, rsp); }
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TRCK", text, src, DST, add_bom, rsp); }
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TYER", text, src, DST, add_bom, rsp); }

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

    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

    bool experimental() const
    { return experimental_; }
    bool has_extended_header() const
    { return (bool) pext_header_; }
    ext_header extended_header() const
    { return *pext_header_; }
    bool has_footer() const
    { return footer_; }
    std::size_t has_frame(const frame_id4 &id) const
    { return frame_map_.count(id); }

    const id3v2_4_frame& get_frame(const frame_id4 &id) const
    {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const
    {
      using namespace std;
      return transform(comms_.begin(), comms_.end(), p,
                       [](const pair<const COMM_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const
    {
      using namespace std;
      return transform(pcnts_.begin(), pcnts_.end(), p,
                       [](const pair<const PCNT_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const
    {
      using namespace std;
      return transform(popms_.begin(), popms_.end(), p,
                       [](const pair<const POPM_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    ///////////////////////////////////////////////////////////////////////////
    //                           tag as container                            //
    ///////////////////////////////////////////////////////////////////////////

  private:

    typedef std::vector<std::unique_ptr<id3v2_4_frame>> frames_type;
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
      mutable_frame_proxy(id3v2_4_tag *p, std::size_t idx): p_(p), idx_(idx)
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
      mutable_frame_proxy& operator=(const id3v2_4_frame &frame);
      mutable_frame_proxy& operator=(const id3v2_4_text_frame &frame);
      mutable_frame_proxy& operator=(const PCNT_2_4 &frame);
      mutable_frame_proxy& operator=(const COMM_2_4 &frame);
      mutable_frame_proxy& operator=(const POPM_2_4 &frame);

      /// This is needed to enable expressions like p->something when p is a
      /// mutable_iterator
      id3v2_4_frame* operator->() const {
        return p_->frames_[idx_].get();
      }
      /// Needed to enable expressions like (*p).something...
      operator id3v2_4_frame&() const {
        return *(p_->frames_[idx_]);
      }

    private:
      id3v2_4_tag *p_;
      std::size_t idx_;

    }; // End class mutable_frame_proxy

    typedef id3v2_tag::frame_iterator_base<id3v2_4_tag::frames_type::iterator> iterator_base;
    friend id3v2_tag::frame_iterator_base<id3v2_4_tag::frames_type::iterator>;

    typedef
    id3v2_tag::frame_iterator<id3v2_4_tag, id3v2_4_frame, mutable_frame_proxy,
                              id3v2_4_tag::frames_type::iterator>
    iterator;

    friend
    id3v2_tag::frame_iterator<id3v2_4_tag, id3v2_4_frame, mutable_frame_proxy,
                              id3v2_4_tag::frames_type::iterator>;

    typedef
    id3v2_tag::const_frame_iterator<id3v2_4_frame, id3v2_4_tag::frames_type::iterator>
    const_iterator;

    friend
    id3v2_tag::const_frame_iterator<id3v2_4_frame, id3v2_4_tag::frames_type::iterator>;


    iterator begin()
    { return iterator(this, frames_.begin()); }
    iterator end()
    { return iterator(this, frames_.end()); }

    const_iterator begin() const {
      id3v2_4_tag &me = const_cast<id3v2_4_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }

    const_iterator end() const {
      id3v2_4_tag &me = const_cast<id3v2_4_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    const_iterator cbegin() const {
      id3v2_4_tag &me = const_cast<id3v2_4_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }

    const_iterator cend() const {
      id3v2_4_tag &me = const_cast<id3v2_4_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    iterator
    insert(const_iterator p, const id3v2_4_frame &frame);

    iterator
    insert(const_iterator p, const id3v2_4_text_frame &frame);
    iterator
    insert(const_iterator p, const PCNT_2_4 &frame);
    iterator
    insert(const_iterator p, const COMM_2_4 &frame);
    iterator
    insert(const_iterator p, const POPM_2_4 &frame);

    void
    push_back(const id3v2_4_frame &frame);
    void
    push_back(const id3v2_4_text_frame &frame);
    void
    push_back(const PCNT_2_4 &frame);
    void
    push_back(const COMM_2_4 &frame);
    void
    push_back(const POPM_2_4 &frame);

    /// Remove the frame at the given position; return a mutable frame iterator
    /// pointing to the next element (or end())
    iterator
    erase(const_iterator p);

    /// Remove the frames in the range [p0, p1); return a mutable frame
    /// iterator pointing to the next element (or end())
    iterator
    erase(const_iterator p0, const_iterator p1);

    /// Write an ID3v2.4 header
    std::ostream& write_header(std::ostream &os, unsigned char flags, std::size_t cb) const;
    /// Write an ID3v2.4 footer
    std::ostream& write_footer(std::ostream &os, unsigned char flags, std::size_t cb) const;

  private:

    /// Remove an arbitrary frame from our ancillary datastructures; on return,
    /// it will remain in the frame vector
    void remove_frame_from_lookups(const frame_id4 &id, std::size_t idx);
    void add_frame_to_lookups(const id3v2_4_frame &frame, std::size_t idx);
    void add_frame_to_lookups(id3v2_4_text_frame &frame, std::size_t idx);
    void add_frame_to_lookups(PCNT_2_4 &frame, std::size_t idx);
    void add_frame_to_lookups(COMM_2_4 &frame, std::size_t idx);
    void add_frame_to_lookups(POPM_2_4 &frame, std::size_t idx);

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             frame parsing                             //
    ///////////////////////////////////////////////////////////////////////////

    /// Convenience typedef for a functor taking an ID3v2.4 frame ID and a
    /// buffer & producing an id3v2_4_frame.
    typedef
    std::function<std::unique_ptr<id3v2_4_frame>
                  (const frame_id4&,
                   const unsigned char*,
                   std::size_t,
                   tag_alter_preservation,
                   file_alter_preservation,
                   read_only,
                   const boost::optional<unsigned char>&,
                   const boost::optional<unsigned char>&,
                   bool,
                   bool,
                   const boost::optional<std::size_t>&)>
    generic_frame_parser;

    /// Convenience typedef for a functor taking an ID3v2.4 frame ID and a
    /// buffer & producing an id3v2_4_text_frame.
    typedef
    std::function<std::unique_ptr<id3v2_4_text_frame>
                  (const frame_id4&,
                   const unsigned char*,
                   std::size_t,
                   tag_alter_preservation,
                   file_alter_preservation,
                   read_only,
                   const boost::optional<unsigned char>&,
                   const boost::optional<unsigned char>&,
                   bool,
                   bool,
                   const boost::optional<std::size_t>&)>
    text_frame_parser;

    typedef
    std::pair<const frame_id4, generic_frame_parser>
    generic_frame_parser_registration;

    typedef
    std::pair<const frame_id4, text_frame_parser>
    text_frame_parser_registration;

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

    static bool
    register_default_generic_frame_parser(const frame_id4 &id,
                                          const generic_frame_parser &F);

    static bool
    register_default_text_frame_parser(const frame_id4 &id,
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
                                  const generic_frame_parser &F) {
      if (parsing_is_reserved(id)) {
         throw reserved_frame_error(id);
      }
      return generic_parsers_.insert(std::make_pair(id, F)).second;
    }

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator
    get_text_frame_parsers(forward_output_iterator p) {
      return std::copy(text_parsers_.begin(), text_parsers_.end(), p);
    }

    /// Not thread-safe
    bool
    register_text_frame_parser(const frame_id4 &id,
                                  const text_frame_parser &F) {
      if (parsing_is_reserved(id)) {
        throw reserved_frame_error(id);
      }
      return text_parsers_.insert(std::make_pair(id, F)).second;
    }

    // Give every translation unit a static initializer; see below.
    struct static_initializer {
      static_initializer();
      ~static_initializer();
    };
    friend struct static_initializer;

  private:

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decompress(const unsigned char *p,
               std::size_t cb,
               std::size_t uncompressed_size) const;

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
                std::size_t size,
                id3v2_4_frame::tag_alter_preservation tag_alter_preservation,
                id3v2_4_frame::file_alter_preservation file_alter_preservation,
                id3v2_4_frame::read_only read_only,
                const boost::optional<unsigned char> &encryption_method,
                const boost::optional<unsigned char> &group_id,
                bool compressed,
                bool unsynchronisation,
                const boost::optional<std::size_t> &data_len_ind,
                const unsigned char *p0,
                const unsigned char *p1);

    void register_encryption_method(const ENCR_2_4 &encr);
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
      id3v2_4_text_frame::frame_encoding dst =id3v2_4_text_frame::frame_encoding::UTF_8,
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
    std::unordered_multimap<frame_id4, id3v2_4_text_frame*>
    text_frame_lookup_type;

    typedef
    std::vector<std::pair<COMM_2_4*, std::size_t>>
    comm_frame_lookup_type;

    typedef
    std::vector<std::pair<PCNT_2_4*, std::size_t>>
    pcnt_frame_lookup_type;

    typedef
    std::vector<std::pair<POPM_2_4*, std::size_t>>
    popm_frame_lookup_type;

    bool experimental_;
    bool footer_;
    generic_parser_map_type generic_parsers_;
    text_parser_map_type text_parsers_;
    std::shared_ptr<ext_header> pext_header_;
    std::size_t padding_;
    comm_frame_lookup_type comms_;
    pcnt_frame_lookup_type pcnts_;
    popm_frame_lookup_type popms_;
    std::unordered_map<unsigned char, ENCR_2_4> encryption_methods_;
    frames_type frames_;
    frame_lookup_type frame_map_;
    /// index: frame id to text frame (spec guarantees only one per id)
    text_frame_lookup_type text_map_;

  }; // End class id3v2_4_tag.

  static id3v2_4_tag::static_initializer id3v2_4_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V24_HH_INCLUDED
