#ifndef ID3V23_HH_INCLUDED
#define ID3V23_HH_INCLUDED 1

#include <scribbu/errors.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/framesv23.hh>

/**
 * \page scribbu_id3v23 ID3v2.3 Tags
 *
 * \section scribbu_id3v23_intro Introduction
 *
 * ID3 version 2.3.
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

    enum class want_extended_header { none, present, with_crc };

    id3v2_3_tag(std::size_t cbpad = 0, bool fexp = false,
                want_extended_header ext = want_extended_header::none);

  public:

    class ext_header {
    public:
      ext_header(const unsigned char *p0,
                 const unsigned char *p1);
      ext_header(std::size_t cbpad, bool fcrc = false):
        size_(fcrc ? 10 : 6),
        crc_present_(fcrc),
        cb_padding_(cbpad)
      { }

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
      return text_frame_as_str("TALB", dst, rsp, src);
    }
    virtual std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TPE1", dst, rsp, src);
    }
    virtual std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TCON", dst, rsp, src);
    }
    virtual std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TENC", dst, rsp, src);
    }
    virtual std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TLAN", dst, rsp, src);
    }
    virtual
    std::size_t play_count() const;
    virtual std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TIT2", dst, rsp, src);
    }
    virtual std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TRCK", dst, rsp, src);
    }
    virtual std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const {
      return text_frame_as_str("TYER", dst, rsp, src);
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
    virtual std::size_t has_play_count() const {
      return frame_map_.count("PCNT");
    }
    virtual std::size_t has_title() const {
      return frame_map_.count("TIT2");
    }
    virtual std::size_t has_track() const {
      return frame_map_.count("TRCK");
    }
    virtual std::size_t has_year() const {
      return frame_map_.count("TYER");
    }

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TALB", text, src, add_bom, rsp);
    }
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
           bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TPE1", text, src, add_bom, rsp);
    }
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TCON", text, src, add_bom, rsp);
    }
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TENC", text, src, add_bom, rsp);
    }
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TLAN", text, src, add_bom, rsp);
    }
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TIT2", text, src, add_bom, rsp);
    }
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TRCK", text, src, add_bom, rsp);
    }
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail) {
      set_text_frame("TYER", text, src, add_bom, rsp);
    }


    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

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

    std::size_t has_frame(const frame_id4 &id) const {
      return frame_map_.count(id);
    }

    const id3v2_3_frame& get_frame(const frame_id4 &id) const {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const {
      using namespace std;
      return transform(comms_.begin(), comms_.end(), p,
                       [](const pair<const COMM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const {
      using namespace std;
      return transform(pcnts_.begin(), pcnts_.end(), p,
                       [](const pair<const PCNT*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const {
      using namespace std;
      return transform(popms_.begin(), popms_.end(), p,
                       [](const pair<const POPM*, size_t> &x) {
                         return *(x.first);
                       });
    }

    class frame_iterator:
      public boost::iterator_facade<
        frame_iterator,
        id3v2_3_frame const,
        boost::random_access_traversal_tag> {

    public:

      typedef
      std::vector<std::unique_ptr<id3v2_3_frame>>::const_iterator impl_type;

      explicit frame_iterator(impl_type p): p_(p)
      { }

    private:
      friend class boost::iterator_core_access;

      void increment() { ++p_; }

      bool equal(const frame_iterator &other) const {
        return p_ == other.p_;
      }

      const id3v2_3_frame& dereference() const {
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

    void parse(std::istream &is, bool extended);

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

    /// Returns true if the parser for the given frame ID may not be
    /// replaced
    static bool parsing_is_reserved(const frame_id4 &id);

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

    typedef
    std::vector<std::unique_ptr<id3v2_3_frame>> frames_type;

    bool experimental_;
    std::size_t size_;
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
