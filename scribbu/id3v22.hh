/**
 * \file id3v22.hh
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

#ifndef ID3V22_HH_INCLUDED
#define ID3V22_HH_INCLUDED 1

#include <scribbu/id3v2.hh>
#include <scribbu/framesv2.hh>
#include <scribbu/framesv22.hh>

#include <boost/iterator/iterator_facade.hpp>

namespace scribbu {

  /**
   * \class id3v2_2_tag
   *
   * \brief Represents an ID3v2.2 tag
   *
   * \sa id3v2_tag
   *
   *
   * Schematically, an ID3v2.2 tag can be represented:
   *
   \code

     +-----------------------------+
     |      Header (10 bytes)      |
     +-----------------------------+
     |   Frames (variable length)  |
     +-----------------------------+
     |           Padding           |
     | (variable length, OPTIONAL) |
     +-----------------------------+

  \endcode
  *
  * The header in detail:
  *
  \code

     |                field | representation | bytes |
     |----------------------+----------------+-------|
     |  ID3/file identifier |          "ID3" |     3 |
     |----------------------+----------------+-------|
     | ID3 version/revision |         $02 00 |     2 |
     |----------------------+----------------+-------|
     |            ID3 flags |      %xy000000 |     1 |
     |----------------------+----------------+-------|
     |             ID3 size |    4*%0xxxxxxx |     4 |
     |----------------------+----------------+-------|

   \endcode
   *
   * Flags:
   *
   *   - x (bit 7): unsynchronisation was applied
   *   - y (bit 6); compression in use; according to the standard, the tag
   *     shall be ignored if the compression bit is set, since there's no
   *     scheme defined.
   *
   *
   * \todo implement insert overloads for id3v2_2_frame&&,
   * initializer_list<id3v2_2_frame>, and range (i.e. two iterators)
   *
   *
   */

  class id3v2_2_tag: public id3v2_tag {

  public:

    /////////////////////////////////////////////////////////////////////////////
    //                             Construction                                //
    /////////////////////////////////////////////////////////////////////////////

    /// Read an ID3v2.2 tag from \a is
    id3v2_2_tag(std::istream &is);
    /// Read an ID3v2.2 tag once it's header has already been read into \a H
    id3v2_2_tag(std::istream &is, const id3v2_info &H);
    /// Initialize an ID3v2.2 tag "from scratch"
    id3v2_2_tag(std::size_t cbpad = 0, bool fexp = false): id3v2_tag(2, 0)
    { }
    id3v2_2_tag(const id3v2_2_tag &that);
    virtual id3v2_tag* clone() const
    { return new id3v2_2_tag(*this); }
    id3v2_2_tag& operator=(const id3v2_2_tag &that);

  public:

    /// Retrieve this tag's ID3v2.2 flags; the exact meaning of each bit shall
    /// be interpreted according to the ID3v2.2 spec
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
    { return text_frame_as_str("TAL", dst, rsp, src); }

    virtual std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TP1", dst, rsp, src);}

    virtual std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TCO", dst, rsp, src); }

    virtual std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TEN", dst, rsp, src); }

    virtual std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TLA", dst, rsp, src); }

    virtual
    std::size_t play_count() const;
    virtual std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TT2", dst, rsp, src); }

    virtual std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TRK", dst, rsp, src); }

    virtual std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const
    { return text_frame_as_str("TYE", dst, rsp, src); }

    virtual std::size_t has_album() const
    { return frame_map_.count("TAL"); }
    virtual std::size_t has_artist() const
    { return frame_map_.count("TP1"); }
    virtual std::size_t has_content_type() const
    { return frame_map_.count("TCO"); }
    virtual std::size_t has_encoded_by() const
    { return frame_map_.count("TEN"); }
    virtual std::size_t has_languages() const
    { return frame_map_.count("TLA"); }
    virtual std::size_t has_play_count() const
    { return frame_map_.count("CNT"); }
    virtual std::size_t has_title() const
    { return frame_map_.count("TT2"); }
    virtual std::size_t has_track() const
    { return frame_map_.count("TRK"); }
    virtual std::size_t has_year() const
    { return frame_map_.count("TYE"); }

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TAL", text, src, add_bom, rsp); }
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
          bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TP1", text, src, add_bom, rsp); }
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TCO", text, src, add_bom, rsp); }
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TEN", text, src, add_bom, rsp); }
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TLA", text, src, add_bom, rsp); }
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TT2", text, src, add_bom, rsp); }
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TRK", text, src, add_bom, rsp); }
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail)
    { set_text_frame("TYE", text, src, add_bom, rsp); }

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

    bool compression() const {
      return compression_;
    }

    std::size_t has_frame(const frame_id3 &id) const
    { return frame_map_.count(id); }

    const id3v2_2_frame& get_frame(const frame_id3 &id) const {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const
    {
      using namespace std;
      return transform(coms_.begin(), coms_.end(), p,
                       [](const pair<const COM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const
    {
      using namespace std;
      return transform(cnts_.begin(), cnts_.end(), p,
                       [](const pair<const CNT*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const
    {
      using namespace std;
      return transform(pops_.begin(), pops_.end(), p,
                       [](const pair<const POP*, size_t> &x) {
                         return *(x.first);
                       });
    }

    ///////////////////////////////////////////////////////////////////////////
    //                           tag as container                            //
    ///////////////////////////////////////////////////////////////////////////

  private:

    typedef std::vector<std::unique_ptr<id3v2_2_frame>> frames_type;
    friend class mutable_frame_proxy;


  public:

    /**
     * \class mutable_frame_proxy
     *
     * \brief Proxy for an id3v2_2_frame returned when a mutable frame iterator
     * is dereferenced
     *
     *
     * Mutable (i.e. non-const) iterators dereference to an instance of this
     * class. This class knows how to keep id3v2_2_tag data structures in-sync
     * when changes are made to the frame wrapped by its instances. Cf.
     * \ref scribbu_impl_notes_iterators_proxies "here" for detailes on
     * proxied containers.
     *
     * One shortcoming in this implementation currently is the lack of any
     * forwarding mechanism; e.g. the following code will fail to compile with
     * "error: ‘class scribbu::id3v2_2_tag::mutable_frame_proxy’ has no member
     * named ‘id’":
     *
     \code

       auto p = tag.begin();
       p[1].id();

     \endcode
     *
     * This is, I believe, because implicit conversion is \em not considered in
     * method invocation
     * (cf. http://en.cppreference.com/w/cpp/language/implicit_conversion). From
     * the same source:
     *
     * "Implicit conversion sequence consists of the following, in this order:
     *
     *   1) zero or one standard conversion sequence;
     *
     *   2) zero or one user-defined conversion;
     *
     *   3) zero or one standard conversion sequence."
     *
     * The only solution I can find is to implement forwarding functions on
     * this class for each public member function of class id3v_2_frame. This
     * seems tedious to me, so I'm delaying in the hopes of a better solution.
     *
     * \todo Implement functions on id3v2_2_tag::mutable_frame_proxy forwarding
     * to id3v_2_frame public methods
     *
     *
     */

    class mutable_frame_proxy
    {
    public:
      /// Construct with the owning tag & index
      mutable_frame_proxy(id3v2_2_tag *p, std::size_t idx): p_(p), idx_(idx)
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
      mutable_frame_proxy& operator=(const id3v2_2_frame &frame);
      mutable_frame_proxy& operator=(const id3v2_2_text_frame &frame);
      mutable_frame_proxy& operator=(const CNT &frame);
      mutable_frame_proxy& operator=(const COM &frame);
      mutable_frame_proxy& operator=(const POP &frame);

      /// This is needed to enable expressions like p->something when p is a
      /// mutable_iterator
      id3v2_2_frame* operator->() const {
        return p_->frames_[idx_].get();
      }
      /// Needed to enable expressions like (*p).something...
      operator id3v2_2_frame&() const {
        return *(p_->frames_[idx_]);
      }

    private:
      id3v2_2_tag *p_;
      std::size_t idx_;

    }; // End class mutable_frame_proxy

    friend id3v2_tag::frame_iterator<id3v2_2_tag, id3v2_2_frame, mutable_frame_proxy,
                                     id3v2_2_tag::frames_type::iterator>;
    friend id3v2_tag::const_frame_iterator<id3v2_2_frame, id3v2_2_tag::frames_type::iterator>;

  public:

    /// id3v2_2_tag iterator
    typedef id3v2_tag::frame_iterator<id3v2_2_tag, id3v2_2_frame, mutable_frame_proxy,
                                      id3v2_2_tag::frames_type::iterator> iterator;
    /// id3v2_2_tag const_iterator
    typedef id3v2_tag::const_frame_iterator<id3v2_2_frame, id3v2_2_tag::frames_type::iterator>
    const_iterator;


    iterator begin()
    { return iterator(this, frames_.begin()); }
    iterator end()
    { return iterator(this, frames_.end()); }

    const_iterator begin() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }

    const_iterator end() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    const_iterator cbegin() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return iterator(&me, me.frames_.begin());
    }

    const_iterator cend() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return iterator(&me, me.frames_.end());
    }

    /**
     * \brief Insert the given value into this tag's list of frames before the
     * specficied iterator
     *
     *
     * \param p [in] A valid constant frame iterator; \a frame shall be
     * inserted before the position to which \a p refers
     *
     * \param frame [in] A const reference to the new frame; a deep copy will
     * be performed
     *
     * \return An iterator that points to the newly inserted frame
     *
     *
     */

    iterator
    insert(const_iterator p, const id3v2_2_frame &frame);

    iterator
    insert(const_iterator p, const id3v2_2_text_frame &frame);
    iterator
    insert(const_iterator p, const CNT &frame);
    iterator
    insert(const_iterator p, const COM &frame);
    iterator
    insert(const_iterator p, const POP &frame);

    void
    push_back(const id3v2_2_frame &frame);
    void
    push_back(const id3v2_2_text_frame &frame);
    void
    push_back(const CNT &frame);
    void
    push_back(const COM &frame);
    void
    push_back(const POP &frame);

    /// Remove the frame at the given position; return a mutable frame iterator
    /// pointing to the next element (or end())
    iterator
    erase(const_iterator p);

    /// Remove the frames in the range [p0, p1); return a mutable frame
    /// iterator pointing to the next element (or end())
    iterator
    erase(const_iterator p0, const_iterator p1);

    /// Write an ID3v2.2 header
    std::ostream& write_header(std::ostream &os, unsigned char flags, std::size_t cb) const;

  private:

    /// Remove an arbitrary frame from our ancillary datastructures; on return,
    /// it will remain in the frame vector
    void remove_frame_from_lookups(const frame_id3 &id, std::size_t idx);
    void add_frame_to_lookups(const id3v2_2_frame &frame, std::size_t idx);
    void add_frame_to_lookups(id3v2_2_text_frame &frame, std::size_t idx);
    void add_frame_to_lookups(CNT &frame, std::size_t idx);
    void add_frame_to_lookups(COM &frame, std::size_t idx);
    void add_frame_to_lookups(POP &frame, std::size_t idx);

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             frame parsing                             //
    ///////////////////////////////////////////////////////////////////////////

    /// Convenience typedef for a functor taking an ID3v2.2 frame ID and a
    /// buffer & producing an id3v2_2_frame.
    typedef
    std::function<std::unique_ptr<id3v2_2_frame>
                  (const frame_id3&,
                   const unsigned char*,
                   std::size_t)>
      generic_frame_parser;

    /// Convenience typedef for a functor taking an ID3v2.2 text frame ID and a
    /// buffer & producing an id3v2_2_text_frame.
    typedef
    std::function<std::unique_ptr<id3v2_2_text_frame>
                  (const frame_id3&,
                   const unsigned char*,
                   std::size_t)>
      text_frame_parser;

    /// Retrieve a copy of the default set of generic frame parsers--
    /// thread-safe
    template <typename forward_output_iterator>
    static
    forward_output_iterator
    get_default_generic_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_generic_parsers_.begin(),
                       default_generic_parsers_.end(),
                       p);
    }

    /// Retrieve a copy of the default set of textual frame parsers--
    /// thread-safe
    template <typename forward_output_iterator>
    static
    forward_output_iterator
    get_default_text_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_text_parsers_.begin(),
                       default_text_parsers_.end(),
                       p);
    }

    /// true => F is a new generic frame parser, false => another was replaced
    static bool
    register_default_generic_frame_parser(const frame_id3 &id,
                                          const generic_frame_parser &F);

    /// true => F is a new textual frame parser, false => another was replaced
    static bool
    register_default_text_frame_parser(const frame_id3 &id,
                                       const text_frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator
    get_generic_frame_parsers(forward_output_iterator p) {
      return std::copy(generic_parsers_.begin(), generic_parsers_.end(), p);
    }
    /// Not thread-safe
    bool
    register_generic_frame_parser(const frame_id3 &id,
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
    register_text_frame_parser(const frame_id3 &id,
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

    // Nifty Counter Idiom...
    static std::mutex& mutex_;
    static std::unordered_map<frame_id3, generic_frame_parser>
    &default_generic_parsers_;
    static std::unordered_map<frame_id3, text_frame_parser>
    &default_text_parsers_;

    /// Returns true if the parser for the given frame ID may not be
    /// replaced
    static bool parsing_is_reserved(const frame_id3 &id);

    /// Parse an ID3v2.2 tag after the standard ten-byte header from an input
    /// stream
    void parse(std::istream &is, std::size_t size);

    /// Parse the frame with identifier {id0,id1,id2} from [p0,p1)
    void
    parse_frame(const frame_id3     &id,
                const unsigned char *p0,
                const unsigned char *p1);

    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string
    text_frame_as_str(
      const frame_id3 &id,
      encoding dst = encoding::UTF_8,
      on_no_encoding rsp = on_no_encoding::fail,
      const boost::optional<encoding> &src = boost::none) const;
    /// Replace a text frame if it exists, append it otherwise
    void set_text_frame(
      const frame_id3 &id,
      const std::string &text,
      encoding src = encoding::UTF_8,
      bool add_bom = false,
      on_no_encoding rsp = on_no_encoding::fail);

  private:

    typedef
    std::unordered_map<frame_id3, generic_frame_parser>
    generic_parser_map_type;

    typedef
    std::unordered_map<frame_id3, text_frame_parser>
    text_parser_map_type;

    typedef
    std::unordered_multimap<frame_id3, std::ptrdiff_t>
    frame_lookup_type;

    typedef
    std::unordered_multimap<frame_id3, id3v2_2_text_frame*>
    text_frame_lookup_type;

    typedef
    std::vector<std::pair<COM*, std::size_t>>
    com_frame_lookup_type;

    typedef
    std::vector<std::pair<CNT*, std::size_t>>
    cnt_frame_lookup_type;

    typedef
    std::vector<std::pair<POP*, std::size_t>>
    pop_frame_lookup_type;

    /// lookup table mapping frame identifier to text frame parser
    text_parser_map_type text_parsers_;
    /// lookup table mapping frame identifier to generic frame parser
    generic_parser_map_type generic_parsers_;
    /// true => compression in use
    bool compression_;
    /// # of padding bytes
    std::size_t padding_;
    /// vector of addresses of COM frames together with their index
    /// in frames_
    com_frame_lookup_type coms_;
    /// vector of addresses of CNT frames together with their index
    /// in frames_
    cnt_frame_lookup_type cnts_;
    /// vector of addresses of POP frames together with their index
    /// in frames_
    pop_frame_lookup_type pops_;
    /// polymorphic vector of frames
    frames_type frames_;
    /// index: frame id to location in frames_
    frame_lookup_type frame_map_;
    /// index: frame id to text frame (spec guarantees only one per id)
    text_frame_lookup_type text_map_;

  };

  static id3v2_2_tag::static_initializer id3v2_2_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V22_HH_INCLUDED
