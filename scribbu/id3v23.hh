#ifndef ID3V23_HH_INCLUDED
#define ID3V23_HH_INCLUDED 1

#include <scribbu/errors.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/framesv23.hh>

/**
 * \page scribbu_id3v23
 *
 * \section scribbu_id3v23_intro Introduction
 *
 * TODO: Write me!
 *
 *
 * \section scribbu_id3v23_discuss "Discussion"
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

   Header:

     +--------------------+-----------+---+
     |ID3/file identifier |   "ID3"   | 3 |
     +--------------------+-----------+---+
     |ID3 version/revision|   $03 00  | 2 |
     +--------------------+-----------+---+
     |ID3 flags           | %xxx00000 | 1 |
     +--------------------+-----------+---+
     |ID3 size            |4*%0xxxxxxx| 4 |
     +--------------------+-----------+---+

     Flags:

       bit 7: unsync
           6: extended header
           5: experimental indicator

    Extended header:

     +--------------------+------------------+---+
     |Extended header size|   $xx xx xx xx   | 4 |
     +--------------------+------------------+---+
     |Extended Flags      |%x0000000 00000000| 2 |
     +--------------------+------------------+---+
     |Size of padding     |   $xx xx xx xx   | 4 |
     +--------------------+------------------+---+

     Flags:

       bit 15: CRC data present

 \endcode
 *
 * Note that the extended header size & size of padding are not
 * sync-safe-- section 3.2 \ref scribbu_id3v2_refs_4 "[4]" of the
 * specification explicitly notes that the extended header is subject
 * to unsynchronisation.
 *
 * CRC data  is a four  byte CRC32  checksum appended to  the extended
 * header; the checksum is  calculated before unsynchronization on the
 * data  between the  extended header  &  the padding  (i.e. just  the
 * frames).
 *
 *
 */

namespace scribbu {

  /**
   * \class id3v2_3_tag
   *
   * \brief Represents an ID3v2.3 tag
   *
   *
   * TODO: Write me!
   *
   *
   */

  class id3v2_3_tag: public id3v2_tag {

  public:
    class invalid_ext_header: public error
    {
    public:
      virtual const char * what() const noexcept;
    };

  public:
    id3v2_3_tag(std::istream &is);
    id3v2_3_tag(std::istream &is, const id3v2_info &H);

  public:

    class ext_header {
    public:
      ext_header(const unsigned char *p0,
                 const unsigned char *p1);

    public:
      std::size_t size() const {
        return size_;
      }
      bool has_crc() const {
        return crc_present_;
      }
      std::uint32_t crc() const {
        return crc_;
      }
      std::size_t padding_size() const {
        return cb_padding_;
      }

    private:
      std::size_t size_;
      bool crc_present_;
      std::uint32_t crc_;
      std::size_t cb_padding_;
    };

  public:
    virtual std::string print() const
    { return std::string("Hello from ID3v2.3!"); }

    bool experimental() const {
      return experimental_;
    }
    bool has_extended_header() const {
      return (bool) pext_header_;
    }
    ext_header extended_header() const {
      return *pext_header_;
    }
    std::size_t padding() const {
      return padding_;
    }

    virtual std::string album() const {
      return text_frame_as_utf8("TALB");
    }
    virtual std::string artist() const {
      return text_frame_as_utf8("TPE1");
    }
    virtual std::string content_type() const {
      return text_frame_as_utf8("TCON");
    }
    virtual std::string encoded_by() const {
      return text_frame_as_utf8("TENC");
    }
    virtual std::string languages() const {
      return text_frame_as_utf8("TLAN");
    }
    virtual std::string title() const {
      return text_frame_as_utf8("TIT2");
    }
    virtual std::string year() const {
      return text_frame_as_utf8("TYER");
    }

    virtual std::size_t has_album() const {
      return frame_map_.count("TALB");
    }
    virtual std::size_t has_artist() const {
      return frame_map_.count("TPE1");
    }
    virtual std::size_t has_content_type() const {
      return frame_map_.count("TCON");
    }
    virtual std::size_t has_encoded_by() const {
      return frame_map_.count("TENC");
    }
    virtual std::size_t has_languages() const {
      return frame_map_.count("TLAN");
    }
    virtual std::size_t has_title() const {
      return frame_map_.count("TIT2");
    }
    virtual std::size_t has_year() const {
      return frame_map_.count("TYER");
    }

    virtual std::size_t all_comments(std::vector<scribbu::comments> &out) const;
    virtual std::size_t all_play_counts(std::vector<scribbu::play_count> &out) const;
    virtual std::size_t all_udts(std::vector<scribbu::user_defined_text> &out) const;
    virtual std::size_t all_ufids(std::vector<scribbu::unique_file_id> &out) const;

    typedef id3v2_3_plus_frame::tag_alter_preservation tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    /// Convenience typedef for a functor taking an ID3v2.3 frame ID and a
    /// buffer producing an id3v2_3_frame.
    typedef std::function<std::unique_ptr<id3v2_3_frame> (const frame_id4                      &id,
                                                          const unsigned char                  *p,
                                                          std::size_t                           cb,
                                                          tag_alter_preservation                tag_alter_preservation,
                                                          file_alter_preservation               file_alter_preservation,
                                                          read_only                             read_only,
                                                          const boost::optional<unsigned char> &encryption_method,
                                                          const boost::optional<unsigned char> &group_id,
                                                          const boost::optional<std::size_t>   &decompressed_size)>
      frame_parser;

    typedef std::pair<const frame_id4, frame_parser> frame_parser_registration;

    /// Retrieve a copy of the default set of frame parsers-- thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator get_default_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_parsers_.begin(), default_parsers_.end(), p);
    }

    static bool register_default_frame_parser(const frame_id4 &id, const frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator get_frame_parsers(forward_output_iterator p) {
      return std::copy(parsers_.begin(), parsers_.end(), p);
    }
    /// Not thread-safe
    bool register_frame_parser(const frame_id4 &id, const frame_parser &F) {
      parsers_.insert(std::make_pair(id, F));
    }

    // Give every translation unit a static initializer; see below.
    struct static_initializer {
      static_initializer();
      ~static_initializer();
    };
    friend struct static_initializer;

  private:

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decompress(const unsigned char *p, std::size_t cb, std::size_t uncompressed_size) const;

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decrypt(const unsigned char *p, std::size_t cb, unsigned char method) const;

    bool encryption_method_regd(unsigned char method) const {
      return 0 != encryption_methods_.count(method);
    }

    void parse(std::istream &is, bool extended);

    std::unique_ptr<id3v2_3_frame>
    parse_frame(const frame_id4                       &id,
                id3v2_3_frame::tag_alter_preservation  tag_alter_preservation,
                id3v2_3_frame::file_alter_preservation file_alter_preservation,
                id3v2_3_frame::read_only               read_only,
                const boost::optional<unsigned char>  &encryption_method,
                const boost::optional<unsigned char>  &group_id,
                const boost::optional<std::size_t>    &decompressed_size,
                const unsigned char *                  p0,
                const unsigned char *                  p1) const;

    void register_encryption_method(const ENCR &encr);
    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string text_frame_as_utf8(const frame_id4 &id) const;

  private:

    // Nifty Counter Idiom...
    static std::mutex& mutex_;
    static std::unordered_map<frame_id4, frame_parser> &default_parsers_;

    /// frame id => frame parser lookup
    typedef std::unordered_map<frame_id4, frame_parser> parser_map_type;
    /// frame id => frame location lookup
    typedef std::unordered_multimap<frame_id4, std::ptrdiff_t> frame_lookup_type;
    /// polymorphic collection of frames
    typedef std::vector<std::unique_ptr<id3v2_3_frame>> frames_type;

    bool experimental_;
    parser_map_type parsers_;
    std::shared_ptr<ext_header> pext_header_;
    std::size_t padding_;
    std::unordered_map<unsigned char, ENCR> encryption_methods_;
    frames_type frames_;
    frame_lookup_type frame_map_;

  }; // End class id3v2_3_tag.

  static id3v2_3_tag::static_initializer id3v2_3_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V23_HH_INCLUDED
