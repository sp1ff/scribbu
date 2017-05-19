#include <id3v23.hh>

#include <id3v2.hh>

#include <zlib.h>


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_2_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef
std::unordered_map<scribbu::frame_id4,
                   scribbu::id3v2_3_tag::generic_frame_parser>
def_generic_reg_type;

typedef
std::unordered_map<scribbu::frame_id4,
                   scribbu::id3v2_3_tag::text_frame_parser>
def_text_reg_type;

static unsigned int nifty_counter_ = 0;

static typename
std::aligned_storage<sizeof(std::mutex), alignof(std::mutex)>::type
mutex_buf_;

static typename
std::aligned_storage< sizeof(def_generic_reg_type),
                      alignof(def_generic_reg_type)>::type
generic_map_buf_;

static typename
std::aligned_storage< sizeof(def_text_reg_type),
                      alignof(def_text_reg_type)>::type
text_map_buf_;

/*static*/ std::mutex&
scribbu::id3v2_3_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_generic_reg_type&
scribbu::id3v2_3_tag::default_generic_parsers_ =
  reinterpret_cast<def_generic_reg_type&>(generic_map_buf_);

/*static*/ def_text_reg_type&
scribbu::id3v2_3_tag::default_text_parsers_ =
  reinterpret_cast<def_text_reg_type&>(text_map_buf_);

scribbu::id3v2_3_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_3_tag::mutex_) std::mutex();
    new (&id3v2_3_tag::default_generic_parsers_) def_generic_reg_type();
    new (&id3v2_3_tag::default_text_parsers_) def_text_reg_type();

#   define REGG(id, tag)                           \
    id3v2_3_tag::default_generic_parsers_.insert(  \
    std::make_pair(frame_id4((id)), tag::create))  \

#   define REGT(id, tag)                           \
    id3v2_3_tag::default_text_parsers_.insert(  \
    std::make_pair(frame_id4((id)), tag::create))  \

    REGG("UFID", UFID);
    REGG("ENCR", ENCR);
    REGT("TALB", id3v2_3_text_frame);
    REGT("TBPM", id3v2_3_text_frame);
    REGT("TCOM", id3v2_3_text_frame);
    REGT("TCON", id3v2_3_text_frame);
    REGT("TCOP", id3v2_3_text_frame);
    REGT("TDAT", id3v2_3_text_frame);
    REGT("TDLY", id3v2_3_text_frame);
    REGT("TENC", id3v2_3_text_frame);
    REGT("TEXT", id3v2_3_text_frame);
    REGT("TFLT", id3v2_3_text_frame);
    REGT("TIME", id3v2_3_text_frame);
    REGT("TIT1", id3v2_3_text_frame);
    REGT("TIT2", id3v2_3_text_frame);
    REGT("TIT3", id3v2_3_text_frame);
    REGT("TKEY", id3v2_3_text_frame);
    REGT("TLAN", id3v2_3_text_frame);
    REGT("TLEN", id3v2_3_text_frame);
    REGT("TMED", id3v2_3_text_frame);
    REGT("TOAL", id3v2_3_text_frame);
    REGT("TOFN", id3v2_3_text_frame);
    REGT("TOLY", id3v2_3_text_frame);
    REGT("TOPE", id3v2_3_text_frame);
    REGT("TORY", id3v2_3_text_frame);
    REGT("TOWN", id3v2_3_text_frame);
    REGT("TPE1", id3v2_3_text_frame);
    REGT("TPE2", id3v2_3_text_frame);
    REGT("TPE3", id3v2_3_text_frame);
    REGT("TPE4", id3v2_3_text_frame);
    REGT("TPOS", id3v2_3_text_frame);
    REGT("TPUB", id3v2_3_text_frame);
    REGT("TRCK", id3v2_3_text_frame);
    REGT("TRDA", id3v2_3_text_frame);
    REGT("TRSN", id3v2_3_text_frame);
    REGT("TRSO", id3v2_3_text_frame);
    REGT("TSIZ", id3v2_3_text_frame);
    REGT("TSRC", id3v2_3_text_frame);
    REGT("TSSE", id3v2_3_text_frame);
    REGT("TYER", id3v2_3_text_frame);
    REGG("TXXX", TXXX);
    REGG("PCNT", PCNT);
    REGG("POPM", POPM);
    // NB "COMM" intentionally omitted

#   undef REGT
#   undef REGG
  }
}

scribbu::id3v2_3_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_3_tag::mutex_)->~mutex();
    (&id3v2_3_tag::default_generic_parsers_)->~unordered_map();
    (&id3v2_3_tag::default_text_parsers_)->~unordered_map();
  }
}


///////////////////////////////////////////////////////////////////////////////
//                             class id3v2_3_tag                             //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::id3v2_3_tag::invalid_ext_header::what() const noexcept
{
  return "invalid extended header";
}

scribbu::id3v2_3_tag::ext_header::ext_header(const unsigned char *p0,
                                             const unsigned char *p1)
{
  using scribbu::detail::uint32_from_non_sync_safe;
  using scribbu::detail::unsigned_from_non_sync_safe;

  if (p0 + 10 > p1) {
    throw invalid_ext_header();
  }
  size_ = unsigned_from_non_sync_safe(p0[0], p0[1], p0[2], p0[3]);
  crc_present_ = p0[4] & 0x80;
  cb_padding_ = unsigned_from_non_sync_safe(p0[6], p0[7], p0[8], p0[9]);
  if (crc_present_) {
    if (14 > p1 - p0) {
      throw invalid_ext_header();
    }
    crc_ = uint32_from_non_sync_safe(p0[10], p0[11], p0[12], p0[13]);
  }
}

scribbu::id3v2_3_tag::id3v2_3_tag(std::istream &is):
  id3v2_tag(is),
  experimental_(flags() & 0x20)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                               text_parsers_.begin()));
  parse(is, flags() & 0x40);
}

scribbu::id3v2_3_tag::id3v2_3_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  experimental_(flags() & 0x20)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                               text_parsers_.begin()));
  parse(is, flags() & 0x40);
}

/*virtual*/ std::size_t
scribbu::id3v2_3_tag::play_count() const {
  switch (has_play_count()) {
  case 1:
    return pcnts_.front().first->count();
  case 0:
    // TODO: Throw custom exception?
    throw std::runtime_error("no play counts");
  default:
    // TODO: Throw custom exception?
    throw std::runtime_error("multiple play counts");
  }
}


/// Not thread-safe
bool
scribbu::id3v2_3_tag::register_generic_frame_parser(
  const frame_id4 &id,
  const generic_frame_parser &F) {
  if (parsing_is_reserved(id)) {
    // TODO: Throw a custom exception in this case
    throw std::invalid_argument("frame " + id.as_string() +
                                " is reserved for parsing");
  }
  generic_parsers_.insert(std::make_pair(id, F)).first;
}

bool
scribbu::id3v2_3_tag::register_text_frame_parser(const frame_id4 &id,
                                                 const text_frame_parser &F) {
  if (parsing_is_reserved(id)) {
    // TODO: Throw a custom exception in this case
    throw std::invalid_argument("frame " + id.as_string() +
                                " is reserved for parsing");
  }
  text_parsers_.insert(std::make_pair(id, F)).first;
}


std::tuple<boost::shared_array<unsigned char>, std::size_t>
scribbu::id3v2_3_tag::decompress(const unsigned char *p,
                                 std::size_t          cb,
                                 std::size_t          uncompressed_size) const
{
  boost::shared_array<unsigned char> pd(new unsigned char[uncompressed_size]);
  int status = uncompress(pd.get(), &uncompressed_size, p, cb);
  if (Z_OK != status) {
    throw zlib_error(status);
  }
  return std::make_tuple(pd, uncompressed_size);
}

std::tuple<boost::shared_array<unsigned char>, std::size_t>
scribbu::id3v2_3_tag::decrypt(const unsigned char * /*p*/,
                              std::size_t           /*cb*/,
                              unsigned char         /*method*/) const
{
  // TODO: Implement id3v2_3_tag::decrypt!
  throw std::logic_error("unimplemented");
}

void scribbu::id3v2_3_tag::parse(std::istream &is, bool extended)
{
  using scribbu::detail::unsigned_from_non_sync_safe;
  using scribbu::detail::unsigned_from_sync_safe;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|
                std::ios_base::failbit|
                std::ios_base::badbit);
  // Also, save this so we can restore the stream to its original
  // state.
  std::istream::streampos here = is.tellg();

  try {

    // Size, in bytes, of the tag *after* the common header, *before*
    // resynchronisation
    std::size_t cb_tag = size();

    // std::array's size is fixed at compile time, which we can't do, and
    // std::vector is permitted to allocate additional memory to accomodate
    // later insertions.
    std::unique_ptr<unsigned char[]> pb(new unsigned char[cb_tag]);
    is.read((char*)pb.get(), cb_tag);

    // In ID3v2.3, the extended header and frames are *not* sync-safe, so we
    // resynchronise here (unlike, say, 2.4).
    if (unsynchronised()) {
      cb_tag = resynchronise(pb.get(), cb_tag);
    }

    const unsigned char *p0 = pb.get();
    const unsigned char *p1 = p0 + cb_tag;

    // Unlike ID3v2.2, we may have an extended header next.
    if (extended) {
      pext_header_.reset(new ext_header(p0, p1));
      p0 += pext_header_->size() + 4;
    }

    static const frame_id4 ENCR("ENCR");
    static const frame_id4 PADDING(0, 0, 0, 0);

    // Process the list of frames once, to grab all encryption methods; take
    // care to handle ENCR frames that are themselves encrypted!
    // TODO: First daft-- re-factor!
    std::size_t cb_frame = 0;
    bool found_encrypted = false;
    do {

      const unsigned char *pcurr = p0;

      for ( ; pcurr < p1; pcurr += cb_frame + 10) {

        // If we're here, there are three possibilities:

        // 1. There is another frame to be consumed, in which case we should
        // have at least eleven bytes ahead of us: "A frame must be at least 1
        // byte big, excluding the header."-- "ID3 tag version 2.3.0",
        // sec. 3.3.

        // 2. The tag contains padding after all the frames & we've reached
        // that; "It is permitted to include padding after all the final frame
        // (at the end of the ID3 tag), making the size of all the frames
        // together smaller than the size given in the head of the tag. A
        // possible purpose of this padding is to allow for adding a few
        // additional frames or enlarge existing frames within the tag without
        // having to rewrite the entire file. The value of the padding bytes
        // must be $00." -- "ID3 tag version 2.3.0", sec. 3.0.

        // 3. The tag is corrupt

        std::size_t left = p1 - pcurr;
        if ( (1 == left && 0 == pcurr[0]) ||
             (2 == left && 0 == pcurr[0] && 0 == pcurr[1]) ||
             (3 == left && 0 == pcurr[0] && 0 == pcurr[1] && 0 == pcurr[2]) ) {
          padding_ = left;
          break;
        }

        // left > 3
        frame_id4 id(pcurr[0], pcurr[1], pcurr[2], pcurr[3]);
        if (PADDING == id) {
          padding_ = left;
          break;
        }

        if (pcurr + 11 > p1) {
          throw invalid_tag();
        }

        cb_frame = unsigned_from_non_sync_safe(pcurr[4], pcurr[5], pcurr[6], pcurr[7]);

        if (ENCR != id) {
          continue;
        }

        unsigned char f0 = pcurr[8];

        id3v2_3_frame::tag_alter_preservation tap = (0 != f0 & 0x80) ?
          id3v2_3_frame::tag_alter_preservation::discard :
          id3v2_3_frame::tag_alter_preservation::preserve;
        id3v2_3_frame::file_alter_preservation fap = (0 != f0 & 0x40) ?
          id3v2_3_frame::file_alter_preservation::discard :
          id3v2_3_frame::file_alter_preservation::preserve;
        id3v2_3_frame::read_only ro = (0 != f0 & 0x40) ?
          id3v2_3_frame::read_only::set :
          id3v2_3_frame::read_only::clear;

        unsigned char f1 = pcurr[9];

        // If the frame is compressed, encrypted, or grouped, there will be
        // additional bytes added starting at pcurr + 10. Since pcurr is used
        // in the loop test clause, we need to capture the 'current read point'
        // in another variable:
        const unsigned char *pread = pcurr + 10;
        std::size_t cb_read = cb_frame;
        boost::optional<std::size_t> decompressed_size = boost::none;
        if (0 != (f1 & 0x80)) {
          decompressed_size = unsigned_from_non_sync_safe(pread[0], pread[1], pread[2], pread[3]);
          pread += 4;
          cb_read -= 4;
        }
        boost::optional<unsigned char> encryption_method = boost::none;
        if (0 != (f1 & 0x40)) {
          encryption_method = *pread++;
          cb_read -= 1;
        }
        boost::optional<unsigned char> group_id = boost::none;
        if (0 != (f1 & 0x20)) {
          group_id = *pread++;
          cb_read -= 1;
        }

        // If we decrypt and/or decompress, we need to make a copy-- keep the
        // copy here...
        boost::shared_array<unsigned char> pf;

        if (boost::none != encryption_method) {
          if (! encryption_method_regd(encryption_method.get())) {
            found_encrypted = true;
            continue;
          }
          std::tie(pf, cb_read) = decrypt(pread, cb_read, encryption_method.get());
          pread = pf.get();
        }

        if (boost::none != decompressed_size) {
          std::tie(pf, cb_read) = decompress(pread, cb_read, decompressed_size.get());
          pread = pf.get();
        }

        scribbu::ENCR encr(pread, pread + cb_read, tap, fap, ro,
                           encryption_method, group_id,
                           decompressed_size);

        register_encryption_method(encr);

      } // End iteration over all frames.

    } while (found_encrypted); // End while on encrypted ENCR frames.

    for ( ; p0 < p1; p0 += cb_frame + 10) {

      // If we're here, there are three possibilities:

      // 1. There is another frame to be consumed, in which case we should
      // have at least eleven bytes ahead of us: "A frame must be at least 1
      // byte big, excluding the header."-- "ID3 tag version 2.3.0",
      // sec. 3.3.

      // 2. The tag contains padding after all the frames & we've reached
      // that; "It is permitted to include padding after all the final frame
      // (at the end of the ID3 tag), making the size of all the frames
      // together smaller than the size given in the head of the tag. A
      // possible purpose of this padding is to allow for adding a few
      // additional frames or enlarge existing frames within the tag without
      // having to rewrite the entire file. The value of the padding bytes
      // must be $00." -- "ID3 tag version 2.3.0", sec. 3.0.

      // 3. The tag is corrupt

      std::size_t left = p1 - p0;
      if ( (1 == left && 0 == p0[0]) ||
           (2 == left && 0 == p0[0] && 0 == p0[1]) ||
           (3 == left && 0 == p0[0] && 0 == p0[1] && 0 == p0[2]) ) {
        // Padding
        break;
      }

      // left > 3
      frame_id4 id(p0[0], p0[1], p0[2], p0[3]);
      if (PADDING == id) {
        break;
      }

      if (p0 + 11 > p1) {
        throw invalid_tag();
      }

      cb_frame = unsigned_from_non_sync_safe(p0[4], p0[5], p0[6], p0[7]);

      unsigned char f0 = p0[8];

      id3v2_3_frame::tag_alter_preservation tap = (0 != f0 & 0x80) ?
        id3v2_3_frame::tag_alter_preservation::discard :
        id3v2_3_frame::tag_alter_preservation::preserve;
      id3v2_3_frame::file_alter_preservation fap = (0 != f0 & 0x40) ?
        id3v2_3_frame::file_alter_preservation::discard :
        id3v2_3_frame::file_alter_preservation::preserve;
      id3v2_3_frame::read_only ro = (0 != f0 & 0x20) ?
        id3v2_3_frame::read_only::set :
        id3v2_3_frame::read_only::clear;

      unsigned char f1 = p0[9];

      // If the frame is compressed, encrypted, or grouped, there will be
      // additional bytes added starting at pcurr + 10. Since pcurr is used
      // in the loop test clause, we need to capture the 'current read point'
      // in another variable:
      const unsigned char *pread = p0 + 10;
      std::size_t cb_read = cb_frame;
      boost::optional<std::size_t> decompressed_size = boost::none;
      if (0 != (f1 & 0x80)) {
        decompressed_size = unsigned_from_non_sync_safe(pread[0], pread[1], pread[2], pread[3]);
        pread += 4;
        cb_read -= 4;
      }
      boost::optional<unsigned char> encryption_method = boost::none;
      if (0 != (f1 & 0x40)) {
        encryption_method = *pread++;
        cb_read -= 1;
      }
      boost::optional<unsigned char> group_id = boost::none;
      if (0 != (f1 & 0x20)) {
        group_id = *pread++;
        cb_read -= 1;
      }

      // If we decrypt and/or decompress, we need to make a copy-- keep the
      // copy here...
      boost::shared_array<unsigned char> pf;

      if (boost::none != encryption_method) {
        std::tie(pf, cb_read) = decrypt(pread, cb_read, encryption_method.get());
        pread = pf.get();
      }

      if (boost::none != decompressed_size) {
        std::tie(pf, cb_read) = decompress(pread, cb_read, decompressed_size.get());
        pread = pf.get();
      }

      // OK-- parse the frame (parse_frame will update all our internal
      // datastructures).
      parse_frame(id, tap, fap, ro, encryption_method, group_id,
                  decompressed_size, pread, pread + cb_read);

    } // End iteration over frames.

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
  }
}

void
scribbu::id3v2_3_tag::parse_frame(
  const frame_id4 &id,
  id3v2_3_frame::tag_alter_preservation tap,
  id3v2_3_frame::file_alter_preservation fap,
  id3v2_3_frame::read_only read_only,
  const boost::optional<unsigned char> &encmth,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t> &dcsz,
  const unsigned char *p0,
  const unsigned char *p1)
{
  using namespace std;

  static const frame_id4 COMMID("COMM"), PCNTID("PCNT");

  // COMM is handled specially-- the frame parser for "COMM" may not
  // be replaced
  if (COMMID == id) {
    COMM *pcomm = new COMM(p0, p1, tap, fap, read_only, encmth,
                           group_id, dcsz);
    comms_.push_back(make_pair(pcomm, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_3_frame>(pcomm));
  }
  else if (PCNTID == id) {
    PCNT *pcnt = new PCNT(p0, p1, tap, fap, read_only, encmth,
                          group_id, dcsz);
    pcnts_.push_back(make_pair(pcnt, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_3_frame>(pcnt));
  }
  else {

    // check to see if we have a text frame parser registered for `id'...
    auto pfn0 = text_parsers_.find(id);
    if (text_parsers_.end() != pfn0) {

      // we do: treat this frame as a text frame. Create an
      // id3v2_2_text_frame...
      auto ptr = pfn0->second(id, p0, p1 - p0, tap,
                              fap, read_only,
                              encmth, group_id,
                              dcsz);
      // enter the address thereof into our text frame index...
      if (text_map_.count(id)) {
        throw duplicate_frame_error(id, p1 - p0);
      }
      text_map_[id] = ptr.get();
      // and move our ptr-to-id3v2_2_text_frame into our frame collection
      // as a ptr-to-id3v2_2_frame.
      frames_.push_back(unique_ptr<id3v2_3_frame>(std::move(ptr)));
    }
    else {

      // We do not-- check to see if we have a generic parser registered...
      auto pfn1 = generic_parsers_.find(id);
      if (generic_parsers_.end() == pfn1) {
        frames_.push_back(
          unique_ptr<id3v2_3_frame>(
             new unknown_id3v2_3_frame(id, tap,
                                       fap, read_only,
                                       encmth, group_id,
                                       dcsz, p0, p1)));
      }
      else {
        frames_.push_back(pfn1->second(id, p0, p1 - p0,
                                       tap,
                                       fap,
                                       read_only, encmth,
                                       group_id, dcsz));
      }
    }
  }

  // and finally update our id index.
  frame_map_.insert(std::make_pair(id, frames_.size() - 1));

}

/*static*/ bool
scribbu::id3v2_3_tag::parsing_is_reserved(const frame_id4 &id)
{
  using namespace std;

  vector<frame_id4> RSVD{ "COMM", "PCNT", "POPM" };

  return RSVD.end() != find(RSVD.begin(), RSVD.end(), id);
}

void scribbu::id3v2_3_tag::register_encryption_method(const ENCR &encr) {

  if (0 != encryption_methods_.count(encr.method_symbol())) {
    // TODO: Throw a custom exception, here
    throw std::runtime_error("duplicate encryption method");
  }

  encryption_methods_.insert(std::make_pair(encr.method_symbol(), encr));

}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string
scribbu::id3v2_3_tag::text_frame_as_str(
  const frame_id4 &id,
  encoding dst /*= encoding::UTF_8*/,
  on_no_encoding rsp /*= on_no_encoding::fail*/,
  const boost::optional<encoding> &src /*= boost::none*/) const
{
  return text_map_.at(id)->as_str<std::string>(dst, rsp, src);
}
