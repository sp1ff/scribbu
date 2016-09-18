#ifndef ID3V22_HH_INCLUDED
#define ID3V22_HH_INCLUDED 1

#include <scribbu/id3v2.hh>
#include <scribbu/framesv2.hh>
#include <scribbu/framesv22.hh>

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
    virtual std::string print() const
    { return std::string("Hello from ID3v2.2!"); }

    bool compression() const {
      return compression_;
    }

    virtual std::string album() const {
      return text_frame_as_utf8("TAL");
    }
    virtual std::string artist() const {
      return text_frame_as_utf8("TP1");
    }
    virtual std::string content_type() const {
      return text_frame_as_utf8("TCO");
    }
    virtual std::string encoded_by() const {
      return text_frame_as_utf8("TEN");
    }
    virtual std::string languages() const {
      return text_frame_as_utf8("TLA");
    }
    virtual std::string title() const {
      return text_frame_as_utf8("TT2");
    }
    virtual std::string year() const {
      return text_frame_as_utf8("TYE");
    }
    virtual std::size_t has_album() const {
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
    virtual std::size_t has_title() const {
      return frame_map_.count("TT2");
    }
    virtual std::size_t has_year() const {
      return frame_map_.count("TYE");
    }

    virtual std::size_t all_comments(std::vector<scribbu::comments> &out) const;
    virtual std::size_t all_play_counts(std::vector<scribbu::play_count> &out) const;
    virtual std::size_t all_udts(std::vector<scribbu::user_defined_text> &out) const;
    virtual std::size_t all_ufids(std::vector<scribbu::unique_file_id> &out) const;

    /// Convenience typedef for a functor taking an ID3v3.3 frame ID and a
    /// buffer producing an id3v2_2_frame.
    typedef std::function<std::unique_ptr<id3v2_2_frame> (const frame_id3&, const unsigned char*, std::size_t)>
      frame_parser;

    typedef std::pair<const frame_id3, frame_parser> frame_parser_registration;

    /// Retrieve a copy of the default set of frame parsers-- thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator get_default_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_parsers_.begin(), default_parsers_.end(), p);
    }

    /// true => F is a new frame parser, false => another was replaced
    static bool register_default_frame_parser(const frame_id3 &id, const frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator get_frame_parsers(forward_output_iterator p) {
      return std::copy(parsers_.begin(), parsers_.end(), p);
    }
    /// Not thread-safe
    bool register_frame_parser(const frame_id3 &id, const frame_parser &F) {
      parsers_.insert(std::make_pair(id, F)).first;
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
    static std::unordered_map<frame_id3, frame_parser> &default_parsers_;

    /// Parse an ID3v2.2 tag after the standard ten-byte header from an input
    /// stream
    void parse(std::istream &is);
    /// Parse the frame with identifier {id0,id1,id2} from [p0,p1)
    std::unique_ptr<id3v2_2_frame> parse_frame(const frame_id3     &id,
                                               const unsigned char *p0,
                                               const unsigned char *p1) const;
    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string text_frame_as_utf8(const frame_id3 &id) const;

  private:

    /// frame id => frame parser lookup
    typedef std::unordered_map<frame_id3, frame_parser> parser_map_type;
    /// frame id => frame location lookup
    typedef std::unordered_multimap<frame_id3, std::ptrdiff_t> frame_lookup_type;
    /// polymorphic collection of frames
    typedef std::vector<std::unique_ptr<id3v2_2_frame>> frames_type;

    parser_map_type parsers_;
    bool compression_;
    std::size_t padding_;
    frames_type frames_;
    frame_lookup_type frame_map_;

  }; // End class id3v2_2_tag.

  static id3v2_2_tag::static_initializer id3v2_2_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V22_HH_INCLUDED
