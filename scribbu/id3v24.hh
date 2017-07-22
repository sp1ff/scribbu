#ifndef ID3V24_HH_INCLUDED
#define ID3V24_HH_INCLUDED 1

#include <scribbu/id3v2.hh>
#include <scribbu/framesv24.hh>

/**
 * \page scribbu_id3v24 ID3v2.4 tags
 *
 * \section scribbu_id3v24_intro Introduction
 *
 * ID3 v2.4.
 *
 *
 * \section scribbu_id3v24_discuss "Discussion"
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
   | Footer (10 bytes, OPTIONAL) |
   +-----------------------------+

   Header:

     +--------------------+-----------+---+
     |ID3/file identifier |   "ID3"   | 3 |
     +--------------------+-----------+---+
     |ID3 version/revision|   $04 00  | 2 |
     +--------------------+-----------+---+
     |ID3 flags           | %xxx00000 | 1 |
     +--------------------+-----------+---+
     |ID3 size            |4*%0xxxxxxx| 4 |
     +--------------------+-----------+---+

     Flags:

       bit 7: Unsynchronisation
       bit 6: Extended header
       bit 5: Experimental indicator
       bit 4: Footer present

    Extended header:

     +--------------------+-------------+---+
     |Extended header size| 4*%0xxxxxxx | 4 |
     +--------------------+-------------+---+
     |Number of flag bytes|     $01     | 1 |
     +--------------------+-------------+---+
     |Extended Flags      |  %0bcd0000  | 1 |
     +--------------------+------------------+---+

     Extended Flags:

       bit 6: Tag is an update
       bit 5: CRC data present
       bit 4: Tag restrictions

     "Each flag that is set in the extended header has data attached,
     which comes in the order in which the flags are encountered
     (i.e. the data for flag 'b' comes before the data for flag 'c').
     Unset flags cannot have any attached data."
     \ref scribbu_id3v2_refs_4 "[4]"

       "Tag is an update" flag data length: 0
       "CRC data present" flag data length: $05 (5 * %0xxxxxxx)
       "Tag restrictions" flag data length: $01


     Footer:

       +----------------------+-----------+---+
       |ID3v2 identifier      | "3DI"     | 3 |
       +----------------------+-----------+---+
       |ID3v2 version/revision| $04 00    | 2 |
       +----------------------+-----------+---+
       |ID3v2 flags           | %abcd0000 | 1 |
       +----------------------+-----------+---+
       |ID3v2 size            |4*%0xxxxxxx| 4 |
       +----------------------+-----------+---+

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
 */

namespace scribbu {

  /**
   * \class id3v2_4_tag
   *
   * \brief Represents an ID3v2.4 tag
   *
   *
   */

  class id3v2_4_tag: public id3v2_tag {

  public:
    class invalid_ext_header: public error
    {
    public:
      virtual const char * what() const noexcept;
    };

  public:
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

  public:

    class ext_header {

    public:
      ext_header(const unsigned char *p0,
                 const unsigned char *p1);

    public:
      std::size_t size() const {
        return size_;
      }
      bool is_update() const {
        return is_update_;
      }
      bool has_crc() const {
        return has_crc_;
      }
      std::uint32_t crc() const;
      bool restricted() const {
        return restricted_;
      }
      std::tuple<tag_size, bool, text_size, bool, image_size>
      get_restrictions() const;

    private:
      std::size_t size_;
      bool is_update_;
      bool has_crc_;
      std::uint32_t crc_;
      bool restricted_;
      tag_size tag_size_restriction_;
      bool text_enc_restriction_;
      text_size text_size_restriction_;
      bool image_enc_restriction_;
      image_size image_sz_restriction_;

    };

  public:
    id3v2_4_tag(std::istream &is);
    id3v2_4_tag(std::istream &is, const id3v2_info &H);

  public:

    /////////////////////////////////////////////////////////////////////////////
    //                          ID3v2 Serialization                            //
    /////////////////////////////////////////////////////////////////////////////

    virtual unsigned char flags() const;
    virtual std::size_t size() const;
    virtual bool needs_unsynchronisation() const;
    virtual std::size_t write(std::istream &) const;

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
      return frame_map_.count("CNT");
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

    ///////////////////////////////////////////////////////////////////////////
    //                           public accessors                            //
    ///////////////////////////////////////////////////////////////////////////

    bool has_extended_header() const {
      return (bool) pext_header_;
    }
    bool experimental() const {
      return experimental_;
    }
    bool has_footer() const {
      return footer_;
    }
    std::size_t padding() const {
      return padding_;
    }

    std::size_t has_frame(const frame_id4 &id) const {
      return frame_map_.count(id);
    }

    const id3v2_4_frame& get_frame(const frame_id4 &id) const {
      frame_lookup_type::const_iterator p = frame_map_.find(id);
      return *frames_.at(p->second);
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_comments(forward_output_iterator p) const {
      using namespace std;
      return transform(comms_.begin(), comms_.end(), p,
                       [](const pair<const COMM_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_play_counts(forward_output_iterator p) const {
      using namespace std;
      return transform(pcnts_.begin(), pcnts_.end(), p,
                       [](const pair<const PCNT_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    template <typename forward_output_iterator>
    forward_output_iterator get_popularimeters(forward_output_iterator p) const {
      using namespace std;
      return transform(popms_.begin(), popms_.end(), p,
                       [](const pair<const POPM_2_4*, size_t> &x) {
                         return *(x.first);
                       });
    }

    class frame_iterator:
      public boost::iterator_facade<
        frame_iterator,
        id3v2_4_frame const,
        boost::random_access_traversal_tag> {

    public:

      typedef
      std::vector<std::unique_ptr<id3v2_4_frame>>::const_iterator impl_type;

      explicit frame_iterator(impl_type p): p_(p)
      { }

    private:
      friend class boost::iterator_core_access;

      void increment() { ++p_; }

      bool equal(const frame_iterator &other) const {
        return p_ == other.p_;
      }

      const id3v2_4_frame& dereference() const {
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
      generic_parsers_.insert(std::make_pair(id, F));
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
      text_parsers_.insert(std::make_pair(id, F));
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

    void parse(std::istream &is, bool extended);

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

    /// Returns true if the parser for the given frame ID may not be
    /// replaced
    static bool parsing_is_reserved(const frame_id4 &id);

    void register_encryption_method(const ENCR_2_4 &encr);
    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string
    text_frame_as_str(
      const frame_id4 &id,
      encoding dst = encoding::UTF_8,
      on_no_encoding rsp = on_no_encoding::fail,
      const boost::optional<encoding> &src = boost::none) const;

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
    std::unordered_multimap<frame_id4, const id3v2_4_text_frame*>
    text_frame_lookup_type;

    typedef
    std::vector<std::pair<const COMM_2_4*, std::size_t>>
    comm_frame_lookup_type;

    typedef
    std::vector<std::pair<const PCNT_2_4*, std::size_t>>
    pcnt_frame_lookup_type;

    typedef
    std::vector<std::pair<const POPM_2_4*, std::size_t>>
    popm_frame_lookup_type;

    typedef
    std::vector<std::unique_ptr<id3v2_4_frame>>
    frames_type;

    std::size_t size_;
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
