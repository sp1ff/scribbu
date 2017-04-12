#ifndef ID3V24_HH_INCLUDED
#define ID3V24_HH_INCLUDED 1

#include <scribbu/id3v2.hh>
#include <scribbu/framesv24.hh>

/**
 * \page scribbu_id3v24
 *
 * \section scribbu_id3v24_intro Introduction
 *
 * ID3v2.4 is the latest version published, dated November 1, 2000. Notably, it
 * allows textual data to  be encoded in UTF-8, which was  a common practice in
 * earlier tags (despite the standard, since  it was not supported yet) because
 * it has several noticeable advantages over UTF-16. Another new feature allows
 * the addition of a tag to the end of the file before other tags (like ID3v1).
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

  class id3v2_4_tag;

  struct id3v2_4_tag_printer: public id3v2_acyclic_visitor
  {
    virtual void print_on(std::ostream&, const id3v2_4_tag&) = 0;
  };

  // TODO: "appended tag"!?
  /**
   * \class id3v2_4_tag
   *
   * \brief Represents an ID3v2.4 tag
   *
   *
   * TODO: Write me!
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

    virtual void accept_for_print(id3v2_acyclic_visitor &V,
                                  std::ostream          &os) const;

    bool has_extended_header() const {
      return (bool) pext_header_;
    }
    bool experimental() const {
      return experimental_;
    }
    bool has_footer() const;

    ///////////////////////////////////////////////////////////////////////////
    //                       inherited from id3v2_tag                        //
    ///////////////////////////////////////////////////////////////////////////

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

    virtual std::size_t
    all_comments(std::vector<scribbu::comments> &out) const;
    virtual std::size_t
    all_play_counts(std::vector<scribbu::play_count> &out) const;
    virtual std::size_t
    all_udts(std::vector<scribbu::user_defined_text> &out) const;
    virtual std::size_t
    all_ufids(std::vector<scribbu::unique_file_id> &out) const;

    ///////////////////////////////////////////////////////////////////////////
    //                       frame parser registration                       //
    ///////////////////////////////////////////////////////////////////////////

    /// Convenience typedef for a functor taking an ID3v2.4 frame ID and a
    /// buffer producing an id3v2_4_frame.
    typedef
    std::function<
      std::unique_ptr<id3v2_4_frame>
      (const frame_id4                      &id,
       const unsigned char                  *p,
       std::size_t                           cb,
       tag_alter_preservation                tag_alter_preservation,
       file_alter_preservation               file_alter_preservation,
       read_only                             read_only,
       const boost::optional<unsigned char> &encryption_method,
       const boost::optional<unsigned char> &group_id,
       bool                                   compressed,
       bool                                   unsynchronisation,
       const boost::optional<std::size_t>    &data_len_ind)>
    frame_parser;

    /**
     * \brief Determine whether the framework has a parser for a given frame ID
     *
     *
     * \param id [in] Frame ID of interest
     *
     * \return bool indicating whether the framework has a (fixed & immutable)
     * parser for this frame ID
     *
     *
     * This class understands a set of ID3v2.2 frames. It permits callers to
     * register  parsers for  new frames,  as  well as  to replace  existing
     * parsers. It reservers, however, a small set of frame parsers on which
     * implementation code depends.
     *
     *
     */

    static bool has_framework_parser(const frame_id4 &x);

    typedef std::pair<const frame_id4, frame_parser> frame_parser_registration;

    /// Retrieve a copy of the default set of frame parsers-- thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator
    get_default_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(default_parsers_.begin(), default_parsers_.end(), p);
    }

    /// true => F is a new frame parser, false => another was replaced; will
    /// throw if \a id names a frame whose parser is reserved by the framework
    static void
    register_default_frame_parser(const frame_id4 &id, const frame_parser &F);

    /// Not thread-safe
    template <typename forward_output_iterator>
    forward_output_iterator get_frame_parsers(forward_output_iterator p) {
      return std::copy(parsers_.begin(), parsers_.end(), p);
    }
    /// Not thread-safe
    void register_frame_parser(const frame_id4 &id, const frame_parser &F) {
      parsers_.insert(std::make_pair(id, F));
    }

    // Give every translation unit a static initializer; see below.
    struct static_initializer {
      static_initializer();
      ~static_initializer();
    };
    friend struct static_initializer;

    /// Convenience typedef for a member function taking an ID3v2.4 frame ID, a
    /// position, and a buffer producing an id3v2_4_frame; these are reserved
    /// by the framework
    typedef std::unique_ptr<id3v2_4_frame>
    (id3v2_4_tag::*reserved_frame_parser)(
      const frame_id4                        &id,
      std::ptrdiff_t                          i,
      const unsigned char                    *pb,
      std::size_t                             cb,
      tag_alter_preservation                  tag_alter_preservation,
      file_alter_preservation                 file_alter_preservation,
      read_only                               read_only,
      const boost::optional<unsigned char>   &encryption_method,
      const boost::optional<unsigned char>   &group_id,
      bool                                   compressed,
      bool                                   unsynchronisation,
      const boost::optional<std::size_t>    &data_len_ind);

  private:

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decompress(const unsigned char *p,
               std::size_t          cb,
               std::size_t          uncompressed_size) const;

    std::tuple<boost::shared_array<unsigned char>, std::size_t>
    decrypt(const unsigned char *p,
            std::size_t          cb,
            unsigned char        method) const;

    bool encryption_method_regd(unsigned char method) const {
      return 0 != encryption_methods_.count(method);
    }

    void register_encryption_method(const ENCR_2_4 &encr);

    std::unique_ptr<scribbu::id3v2_4_frame>
    create_COMM(const frame_id4                    &/*id*/,
                std::ptrdiff_t                        i,
                const unsigned char                  *p,
                std::size_t                           cb,
                tag_alter_preservation                tag_alter_preservation,
                file_alter_preservation               file_alter_preservation,
                read_only                             read_only,
                const boost::optional<unsigned char> &encryption_method,
                const boost::optional<unsigned char> &group_id,
                bool                                   compressed,
                bool                                   unsynchronisation,
                const boost::optional<std::size_t>    &data_len_ind);
    std::unique_ptr<scribbu::id3v2_4_frame>
    create_PCNT(const frame_id4                    &/*id*/,
                std::ptrdiff_t                        i,
                const unsigned char                  *p,
                std::size_t                           cb,
                tag_alter_preservation                tag_alter_preservation,
                file_alter_preservation               file_alter_preservation,
                read_only                             read_only,
                const boost::optional<unsigned char> &encryption_method,
                const boost::optional<unsigned char> &group_id,
                bool                                   compressed,
                bool                                   unsynchronisation,
                const boost::optional<std::size_t>    &data_len_ind);
    std::unique_ptr<scribbu::id3v2_4_frame>
    create_UFID(const frame_id4                    &/*id*/,
                std::ptrdiff_t                        i,
                const unsigned char                  *p,
                std::size_t                           cb,
                tag_alter_preservation                tag_alter_preservation,
                file_alter_preservation               file_alter_preservation,
                read_only                             read_only,
                const boost::optional<unsigned char> &encryption_method,
                const boost::optional<unsigned char> &group_id,
                bool                                   compressed,
                bool                                   unsynchronisation,
                const boost::optional<std::size_t>    &data_len_ind);
    std::unique_ptr<scribbu::id3v2_4_frame>
    create_TXXX(const frame_id4                    &/*id*/,
                std::ptrdiff_t                        i,
                const unsigned char                  *p,
                std::size_t                           cb,
                tag_alter_preservation                tag_alter_preservation,
                file_alter_preservation               file_alter_preservation,
                read_only                             read_only,
                const boost::optional<unsigned char> &encryption_method,
                const boost::optional<unsigned char> &group_id,
                bool                                   compressed,
                bool                                   unsynchronisation,
                const boost::optional<std::size_t>    &data_len_ind);
    std::unique_ptr<scribbu::id3v2_4_frame>
    create_text_frame(
      const frame_id4                    &/*id*/,
      std::ptrdiff_t                        i,
      const unsigned char                  *p,
      std::size_t                           cb,
      tag_alter_preservation                tag_alter_preservation,
      file_alter_preservation               file_alter_preservation,
      read_only                             read_only,
      const boost::optional<unsigned char> &encryption_method,
      const boost::optional<unsigned char> &group_id,
      bool                                   compressed,
      bool                                   unsynchronisation,
      const boost::optional<std::size_t>    &data_len_ind);

    /// Retrieve a copy of the set of reserved frame parsers-- thread-safe
    template <typename forward_output_iterator>
    static forward_output_iterator
    get_reserved_frame_parsers(forward_output_iterator p) {
      std::lock_guard<std::mutex> guard(mutex_);
      return std::copy(reserved_parsers_.begin(),
                       reserved_parsers_.end(), p);
    }

    // Nifty Counter Idiom...
    static std::mutex& mutex_;
    static std::unordered_map<frame_id4, frame_parser> &default_parsers_;
    static std::unordered_map<frame_id4, reserved_frame_parser>
    &reserved_parsers_;

    void parse(std::istream &is, bool extended);

    std::unique_ptr<id3v2_4_frame>
    parse_frame(const frame_id4                       &id,
                std::size_t                            size,
                id3v2_4_frame::tag_alter_preservation  tag_alter_preservation,
                id3v2_4_frame::file_alter_preservation file_alter_preservation,
                id3v2_4_frame::read_only               read_only,
                const boost::optional<unsigned char>  &encryption_method,
                const boost::optional<unsigned char>  &group_id,
                bool                                   compressed,
                bool                                   unsynchronisation,
                const boost::optional<std::size_t>    &data_len_ind,
                const unsigned char *                  p0,
                const unsigned char *                  p1) const;

    /// Lookup a text frame, convert its data from its native encoding to
    /// UTF-8, return as a string
    std::string text_frame_as_utf8(const frame_id4 &id) const;

  private:

    /// frame id => frame parser lookup
    typedef std::unordered_map<frame_id4, frame_parser> parser_map_type;
    /// frame id => reserved frame parser lookup
    typedef std::unordered_map<frame_id3, reserved_frame_parser>
    reserved_parser_map_type;
    /// frame id => frame location lookup
    typedef std::unordered_multimap<frame_id4, std::ptrdiff_t>
    frame_lookup_type;
    /// polymorphic collection of frames
    typedef std::vector<std::unique_ptr<id3v2_4_frame>> frames_type;

    bool experimental_;
    bool footer_;
    parser_map_type parsers_;
    std::shared_ptr<ext_header> pext_header_;
    std::size_t padding_;
    std::unordered_map<unsigned char, ENCR_2_4> encryption_methods_;
    frames_type frames_;

    // Convenience data structures
    frame_lookup_type frame_map_;
    std::unordered_map<std::ptrdiff_t, COMM_2_4*> comments_;
    std::unordered_map<std::ptrdiff_t, PCNT_2_4*> play_counts_;
    std::unordered_map<std::ptrdiff_t, UFID_2_4*> ufids_;
    std::unordered_map<std::ptrdiff_t, TXXX_2_4*> udts_;
    std::unordered_map<std::ptrdiff_t, id3v2_4_text_frame*> texts_pos_;
    std::unordered_multimap<frame_id4, id3v2_4_text_frame*> text_;

  }; // End class id3v2_4_tag.

  static id3v2_4_tag::static_initializer id3v2_4_tag_static_initializer_;

} // End namespace scribbu.

#endif // not ID3V24_HH_INCLUDED
