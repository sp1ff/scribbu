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

    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

    bool compression() const {
      return compression_;
    }

    std::size_t padding() const {
      return padding_;
    }

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

    class frame_iterator:
      public boost::iterator_facade<
        frame_iterator,
        id3v2_2_frame const,
        boost::random_access_traversal_tag> {

    public:

      typedef
      std::vector<std::unique_ptr<id3v2_2_frame>>::const_iterator impl_type;

      explicit frame_iterator(impl_type p): p_(p)
      { }

    private:
      friend class boost::iterator_core_access;

      void increment() { ++p_; }

      bool equal(const frame_iterator &other) const {
        return p_ == other.p_;
      }

      const id3v2_2_frame& dereference() const {
        return *(p_->get());
      }

    private:

      impl_type p_;


    }; // End class frame_iterator.

    frame_iterator begin() const {
      return frame_iterator(frames_.begin());
    }

    frame_iterator end() const {
      return frame_iterator(frames_.end());
    }

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
        // TODO: Throw a custom exception in this case
        throw std::invalid_argument("frame " + id.as_string() +
                                    " is reserved for parsing");
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
        // TODO: Throw a custom exception in this case
        throw std::invalid_argument("frame " + id.as_string() +
                                    " is reserved for parsing");
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
    void parse(std::istream &is);

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
    std::unordered_map<frame_id3, const id3v2_2_text_frame*>
    text_frame_lookup_type;

    typedef
    std::vector<std::pair<const COM*, std::size_t>>
    com_frame_lookup_type;

    typedef
    std::vector<std::pair<const CNT*, std::size_t>>
    cnt_frame_lookup_type;

    typedef
    std::vector<std::unique_ptr<id3v2_2_frame>>
    frames_type;

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
