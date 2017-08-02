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
   *
   * Schematically:
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

     Header:

       +--------------------+-----------+---+
       |ID3/file identifier |   "ID3"   | 3 |
       +--------------------+-----------+---+
       |ID3 version/revision|   $02 00  | 2 |
       +--------------------+-----------+---+
       |ID3 flags           | %xx000000 | 1 |
       +--------------------+-----------+---+
       |ID3 size            |4*%0xxxxxxx| 4 |
       +--------------------+-----------+---+

       flags:

         bit 7: unsync
             6: compression

   \endcode
   *
   * According to the standard, the tag shall be ignored if the compression bit
   * is set, since there's no scheme defined.
   *
   *
   */

  class id3v2_2_tag: public id3v2_tag {

  public:
    id3v2_2_tag(std::istream &is);
    id3v2_2_tag(std::istream &is, const id3v2_info &H);

  public:

    /////////////////////////////////////////////////////////////////////////////
    //                          ID3v2 Serialization                            //
    /////////////////////////////////////////////////////////////////////////////

    virtual unsigned char flags() const;
    virtual std::size_t size(bool unsync = true) const; 
    virtual bool needs_unsynchronisation() const;
    virtual std::size_t write(std::ostream &os, bool unsync = true) const;

    /////////////////////////////////////////////////////////////////////////////
    //                    Frames Common to all ID3v2 Tags                      //
    /////////////////////////////////////////////////////////////////////////////

    virtual std::string
    album(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TAL", dst, rsp, src);
    }

    virtual std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TP1", dst, rsp, src);
    }

    virtual std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TCO", dst, rsp, src);
    }

    virtual std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TEN", dst, rsp, src);
    }

    virtual std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TLA", dst, rsp, src);
    }

    virtual
    std::size_t play_count() const;
    virtual std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TT2", dst, rsp, src);
    }

    virtual std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TRK", dst, rsp, src);
    }

    virtual std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TYE", dst, rsp, src);
    }

    virtual
    std::size_t has_album() const {
      return frame_map_.count("TAL");
    }
    virtual std::size_t has_artist() const {
      return frame_map_.count("TP1");
    }
    virtual std::size_t has_content_type() const {
      return frame_map_.count("TCO");
    }
    virtual std::size_t has_encoded_by() const {
      return frame_map_.count("TEN");
    }
    virtual std::size_t has_languages() const {
      return frame_map_.count("TLA");
    }
    virtual std::size_t has_play_count() const {
      return frame_map_.count("CNT");
    }
    virtual std::size_t has_title() const {
      return frame_map_.count("TT2");
    }
    virtual std::size_t has_track() const {
      return frame_map_.count("TRK");
    }
    virtual std::size_t has_year() const {
      return frame_map_.count("TYE");
    }

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

    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

    std::size_t has_frame(const frame_id3 &id) const {
      return frame_map_.count(id);
    }

    const id3v2_2_frame& get_frame(const frame_id3 &id) const {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const {
      using namespace std;
      return transform(coms_.begin(), coms_.end(), p,
                       [](const pair<const COM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const {
      using namespace std;
      return transform(cnts_.begin(), cnts_.end(), p,
                       [](const pair<const CNT*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const {
      using namespace std;
      return transform(pops_.begin(), pops_.end(), p,
                       [](const pair<const POP*, size_t> &x) {
                         return *(x.first);
                       });
    }

    std::size_t num_frames() const {
      return frames_.size();
    }

    bool compression() const {
      return compression_;
    }

    std::size_t padding() const {
      return padding_;
    }

    void padding(std::size_t padding) {
      padding_ = padding;
    }

    ///////////////////////////////////////////////////////////////////////////
    //                           tag as container                            //
    ///////////////////////////////////////////////////////////////////////////

  private:

    typedef
    std::vector<std::unique_ptr<id3v2_2_frame>>
    frames_type;

    friend class mutable_frame_proxy;

  public:

    /***
     * TOOD(sp1ff): Clean all this documentation up.
     *
     * 
     * I originally conceived of & implemented these tags & frames as immutable
     * copies of whatever was read off disk on construction. As the project
     * developed, it became clear that this was not going to be sufficient and
     * that the library would need to provide some way of editing frames, as
     * well.
     *
     * This is a first step toward moving my ID3v2 tag abstractions away from
     * being immutable copies of what we read off disk & providing mutability.
     * I've tried to make this tag behave like a standard-compliant container
     * for frames... except that the frames are polymorphic. There are also a
     * lot of ancillary datastructures that need to be updated when the
     * collection of frames changes.
     *
     *
     */

    /**
     * \class mutable_frame_proxy
     *
     * \brief Proxy for an id3v2_2_frame returned when a mutable frame iterator
     * is dereferenced
     *
     *
     * TODO(sp1ff): Implement the following:
     *
     * - assignment from another mutable_frame_proxy
     * 
     * - operator==
     *
     * - operator<
     *
     * - move semantics?
     *
     * TODO(sp1ff): Re-implement the id3v2_2_frame methods, so I can write
     * expressions like iterator[i].id()...
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

      // This is needed to enable expressions like p->something when p is a
      // mutable_iterator
      id3v2_2_frame* operator->() const {
        return p_->frames_[idx_].get();
      }
      // Needed to enable expressions like (*p).something...
      operator id3v2_2_frame&() const {
        return *(p_->frames_[idx_]);
      }

    private:
      id3v2_2_tag *p_;
      std::size_t idx_;
    };

    friend class frame_iterator_base;

    // TOOD(sp1ff): Document frame_iterator_base
    // - factored out as much logic as I could
    // - implemented in terms of a non-const iterator; the const subclass takes
    // - care of enforcing RO semantics
    class frame_iterator_base {

    protected:

      typedef id3v2_2_tag::frames_type::iterator impl_type;

    public:
      /// std iterator category-- all frame iterators are random-access
      typedef std::random_access_iterator_tag iterator_category;
      /// result of subtracting two frame iterators
      typedef std::ptrdiff_t difference_type;
      /// constructs a "one-past-the-end" iterator
      frame_iterator_base()
      { }
      frame_iterator_base(const impl_type &p0, const impl_type &p): p0_(p0), p_(p)
      { }
      virtual ~frame_iterator_base()
      { }

      /// retrieve this iterator's idnex
      std::size_t index() const
      { return p_ - p0_; }

      friend difference_type operator-(const frame_iterator_base &lhs,
                                       const frame_iterator_base &rhs)
      { return lhs.p_ - rhs.p_; }

      friend bool operator==(const frame_iterator_base &lhs,
                             const frame_iterator_base &rhs)
      { return lhs.p_ == rhs.p_; }

      friend bool operator!=(const frame_iterator_base &lhs,
                             const frame_iterator_base &rhs)
      { return ! (lhs == rhs); }

      friend bool operator<(const frame_iterator_base &lhs,
                            const frame_iterator_base &rhs)
      { return lhs.p_ < rhs.p_; }

    protected:
      /// increment this iterator by n; return a referene to this iterator
      frame_iterator_base& incr(std::ptrdiff_t n = 1)
      { p_ += n; return *this; }
      /// decrement this iterator by n; return a referene to this iterator
      frame_iterator_base& decr(std::ptrdiff_t n = 1)
      { p_ -= n; return *this; }

    protected:
      impl_type p0_, p_;
    };

    class mutable_frame_iterator:
      public frame_iterator_base {

    public:
      /// The type "pointed to" by this iterator
      typedef id3v2_2_frame value_type;
      typedef id3v2_2_frame *pointer;
      typedef id3v2_2_frame &reference;

      mutable_frame_iterator()
      { }
      
      explicit mutable_frame_iterator(id3v2_2_tag *pown,
                                      const impl_type &p):
        frame_iterator_base(pown->frames_.begin(), p), pown_(pown)
      { }

      mutable_frame_iterator& operator++()
      { incr(); return *this; }

      mutable_frame_iterator operator++(int)
      {
        mutable_frame_iterator tmp(*this);
        incr();
        return tmp;
      }

      mutable_frame_iterator& operator--()
      { decr(); return *this; }

      mutable_frame_iterator operator--(int)
      {
        mutable_frame_iterator tmp(*this);
        decr();
        return tmp;
      }

      mutable_frame_iterator& operator+=(difference_type i)
      { incr(i); return *this; }

      mutable_frame_iterator& operator-=(difference_type i)
      { decr(i); return *this; }

      mutable_frame_iterator operator+(difference_type i)
      {
        mutable_frame_iterator tmp(*this);
        tmp += i;
        return tmp;
      }

      mutable_frame_iterator operator-(difference_type i)
      {
        mutable_frame_iterator tmp(*this);
        tmp -= i;
        return tmp;
      }

      mutable_frame_proxy operator*() const
      { return mutable_frame_proxy(pown_, index()); }

      mutable_frame_proxy operator->() const
      { return mutable_frame_proxy(pown_, index()); }

      mutable_frame_proxy operator[](difference_type i)
      { return *(*this + i); }

    private:
      id3v2_2_tag *pown_;

    };

    class const_iterator:
      public frame_iterator_base {
      
    public:
      /// The type "pointed to" by this iterator
      typedef id3v2_2_frame value_type;
      typedef const id3v2_2_frame *pointer;
      typedef const id3v2_2_frame &reference;

      const_iterator()
      { }
      
      explicit const_iterator(const impl_type &p0,
                                    const impl_type &p):
        frame_iterator_base(p0, p)
      { }

      const_iterator(const mutable_frame_iterator &p): frame_iterator_base(p)
      { }

      const_iterator& operator++()
      { incr(); return *this; }

      const_iterator operator++(int)
      {
        const_iterator tmp(*this);
        incr();
        return tmp;
      }

      const_iterator& operator--()
      { decr(); return *this; }

      const_iterator operator--(int)
      {
        const_iterator tmp(*this);
        decr();
        return tmp;
      }

      const_iterator& operator+=(difference_type i)
      { incr(i); return *this; }

      const_iterator& operator-=(difference_type i)
      { decr(i); return *this; }

      const_iterator operator+(difference_type i)
      {
        const_iterator tmp(*this);
        tmp += i;
        return tmp;
      }

      const_iterator operator-(difference_type i)
      {
        const_iterator tmp(*this);
        tmp -= i;
        return tmp;
      }
      reference operator*() const
      { return *(p_->get()); }

      pointer operator->() const
      { return p_->get(); }

    };

    mutable_frame_iterator begin() {
      return mutable_frame_iterator(this, frames_.begin());
    }
    mutable_frame_iterator end() {
      return mutable_frame_iterator(this, frames_.end());
    }

    const_iterator begin() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return mutable_frame_iterator(&me, me.frames_.begin());
    }

    const_iterator end() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return mutable_frame_iterator(&me, me.frames_.end());
    }

    const_iterator cbegin() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return mutable_frame_iterator(&me, me.frames_.begin());
    }
    const_iterator cend() const {
      id3v2_2_tag &me = const_cast<id3v2_2_tag&>(*this);
      return mutable_frame_iterator(&me, me.frames_.end());
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

    mutable_frame_iterator
    insert(const_iterator p, const id3v2_2_frame &frame);

    // TODO(sp1ff): Implement overloads for
    //   - id3v2_2_frame&&
    //   - initializer_list<id3v2_2_frame>
    //   - range (i.e. two iterators)

    mutable_frame_iterator
    insert(const_iterator p, const id3v2_2_text_frame &frame);
    mutable_frame_iterator
    insert(const_iterator p, const CNT &frame);
    mutable_frame_iterator
    insert(const_iterator p, const COM &frame);
    mutable_frame_iterator
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
    mutable_frame_iterator
    erase(const_iterator p);

    /// Remove the frames in the range [p0, p1); return a mutable frame
    /// iterator pointing to the next element (or end())
    mutable_frame_iterator
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
      generic_parsers_.insert(std::make_pair(id, F)).first;
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
      text_parsers_.insert(std::make_pair(id, F)).first;
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
    // std::unordered_multimap<frame_id3, const id3v2_2_text_frame*>
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
