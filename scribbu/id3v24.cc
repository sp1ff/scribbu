#include "id3v24.hh"
#include "framesv24.hh"

#include <zlib.h>


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_4_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef
std::unordered_map<scribbu::frame_id4,
                   scribbu::id3v2_4_tag::generic_frame_parser>
def_generic_reg_type;

typedef
std::unordered_map<scribbu::frame_id4,
                   scribbu::id3v2_4_tag::text_frame_parser>
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
scribbu::id3v2_4_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_generic_reg_type&
scribbu::id3v2_4_tag::default_generic_parsers_ =
  reinterpret_cast<def_generic_reg_type&>(generic_map_buf_);

/*static*/ def_text_reg_type&
scribbu::id3v2_4_tag::default_text_parsers_ =
  reinterpret_cast<def_text_reg_type&>(text_map_buf_);

scribbu::id3v2_4_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_4_tag::mutex_) std::mutex();
    new (&id3v2_4_tag::default_generic_parsers_) def_generic_reg_type();
    new (&id3v2_4_tag::default_text_parsers_) def_text_reg_type();

#   define REGG(id, tag)                          \
    id3v2_4_tag::default_generic_parsers_.insert( \
    std::make_pair(frame_id4((id)), tag::create)) \

#   define REGT(id, tag)                          \
    id3v2_4_tag::default_text_parsers_.insert(    \
    std::make_pair(frame_id4((id)), tag::create)) \

    REGG("UFID", UFID_2_4);
    REGG("ENCR", ENCR_2_4);
    REGT("TALB", id3v2_4_text_frame);
    REGT("TBPM", id3v2_4_text_frame);
    REGT("TCOM", id3v2_4_text_frame);
    REGT("TCON", id3v2_4_text_frame);
    REGT("TCOP", id3v2_4_text_frame);
    REGT("TDAT", id3v2_4_text_frame);
    REGT("TDLY", id3v2_4_text_frame);
    REGT("TENC", id3v2_4_text_frame);
    REGT("TEXT", id3v2_4_text_frame);
    REGT("TFLT", id3v2_4_text_frame);
    REGT("TIME", id3v2_4_text_frame);
    REGT("TIT1", id3v2_4_text_frame);
    REGT("TIT2", id3v2_4_text_frame);
    REGT("TIT3", id3v2_4_text_frame);
    REGT("TKEY", id3v2_4_text_frame);
    REGT("TLAN", id3v2_4_text_frame);
    REGT("TLEN", id3v2_4_text_frame);
    REGT("TMED", id3v2_4_text_frame);
    REGT("TOAL", id3v2_4_text_frame);
    REGT("TOFN", id3v2_4_text_frame);
    REGT("TOLY", id3v2_4_text_frame);
    REGT("TOPE", id3v2_4_text_frame);
    REGT("TORY", id3v2_4_text_frame);
    REGT("TOWN", id3v2_4_text_frame);
    REGT("TPE1", id3v2_4_text_frame);
    REGT("TPE2", id3v2_4_text_frame);
    REGT("TPE3", id3v2_4_text_frame);
    REGT("TPE4", id3v2_4_text_frame);
    REGT("TPOS", id3v2_4_text_frame);
    REGT("TPUB", id3v2_4_text_frame);
    REGT("TRCK", id3v2_4_text_frame);
    REGT("TRDA", id3v2_4_text_frame);
    REGT("TRSN", id3v2_4_text_frame);
    REGT("TRSO", id3v2_4_text_frame);
    REGT("TSIZ", id3v2_4_text_frame);
    REGT("TSRC", id3v2_4_text_frame);
    REGT("TSSE", id3v2_4_text_frame);
    REGT("TYER", id3v2_4_text_frame);
    REGG("TXXX", TXXX_2_4);
    REGG("PCNT", PCNT_2_4);
    REGG("POPM", POPM_2_4);

    // NB COMM intentionally omitted--handled specially.

#   undef REGT
#   undef REGG
  }
}

scribbu::id3v2_4_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_4_tag::mutex_)->~mutex();
    (&id3v2_4_tag::default_generic_parsers_)->~unordered_map();
    (&id3v2_4_tag::default_text_parsers_)->~unordered_map();
  }
}


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_4 tag                              //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::id3v2_4_tag::invalid_ext_header::what() const noexcept
{
  return "invalid extended header";
}

scribbu::id3v2_4_tag::ext_header::ext_header(const unsigned char *p0,
                                             const unsigned char *p1)
{
  using scribbu::detail::unsigned_from_sync_safe;

  if (p0 + 6 > p1) {
    throw invalid_ext_header();
  }

  size_ = unsigned_from_sync_safe(p0[0], p0[1], p0[2], p0[3]);

  std::size_t num_flag_bytes = p0[4];

  if (!num_flag_bytes) {
    throw invalid_ext_header();
  }

  // We only know how to parse the first:
  bool is_update_  = p0[5] & 0x40;
  bool has_crc_    = p0[5] & 0x20;
  bool restricted_ = p0[5] & 0x10;

  // 00 01 02 03  04  05  06  07 08 09 0a 0b 0c
  //    size    | # | f | 00 |05  crc data      |
  p0 += 5 + num_flag_bytes;

  if (is_update_) {
    // No flag data
    if (p0 + 1 > p1 || 0x00 != p0[0]) {
      throw invalid_ext_header();
    }
    ++p0;
  }

  if (has_crc_) {
    if (p0 + 6 > p1 || 0x05 != p0[0]) {
      throw invalid_ext_header();
    }
    // The CRC data has the form
    // %0000xxxx %0xxxxxxxx %0xxxxxxxx %0xxxxxxxx %0xxxxxxxx
    if (0x0f < p0[1] || 0x7f < p0[2] || 0x7f < p0[3] ||
        0x7f < p0[4] || 0x7f < p0[5]) {
      throw invalid_ext_header();
    }

    crc_ = unsigned_from_sync_safe(p0[1], p0[2], p0[3], p0[4], p0[5]);

    p0 += 6;
  }

  if (restricted_) {

    if (p0 + 2 > p1 || 0x01 != p0[0]) {
      throw invalid_ext_header();
    }

    // Restrictions           %ppqrrstt
    // - p - Tag size restrictions
    // - q - Text encoding restrictions
    // - r - Text fields size restrictions
    // - s - Image encoding restrictions
    // - t - Image size restrictions

    // Tag size restriction
    //   00   No more than 128 frames and 1 MB total tag size.
    //   01   No more than 64 frames and 128 KB total tag size.
    //   10   No more than 32 frames and 40 KB total tag size.
    //   11   No more than 32 frames and 4 KB total tag size.
    tag_size_restriction_ = tag_size( (0xc0 & p0[1]) >> 6 );

    // q - Text encoding restrictions
    //   0    No restrictions
    //   1    Strings are only encoded with ISO-8859-1 [ISO-8859-1] or
    //        UTF-8 [UTF-8].
    text_enc_restriction_ = 0 != (0x20 & p0[1]);

    // r - Text fields size restrictions
    //   00   No restrictions
    //   01   No string is longer than 1024 characters.
    //   10   No string is longer than 128 characters.
    //   11   No string is longer than 30 characters.
    text_size_restriction_ = text_size( (0x18 & p0[1]) >> 3 );

    // s - Image encoding restrictions
    //   0   No restrictions
    //   1   Images are encoded only with PNG [PNG] or JPEG [JFIF].
    image_enc_restriction_ = 0 != (0x04 & p0[1]);

    // t - Image size restrictions
    //   00  No restrictions
    //   01  All images are 256x256 pixels or smaller.
    //   10  All images are 64x64 pixels or smaller.
    //   11  All images are exactly 64x64 pixels, unless required
    //       otherwise.
    image_sz_restriction_ = image_size(0x03 & p0[1]);

    p0 += 2;
  }

  if (p0 != p1) {
    throw invalid_ext_header();
  }

}

std::uint32_t scribbu::id3v2_4_tag::ext_header::crc() const
{
  if (!has_crc()) {
    throw invalid_ext_header();
  }
  return crc_;
}

std::tuple<scribbu::id3v2_4_tag::tag_size, bool, scribbu::id3v2_4_tag::text_size, bool, scribbu::id3v2_4_tag::image_size>
scribbu::id3v2_4_tag::ext_header::get_restrictions() const
{
  if (!restricted()) {
    throw invalid_ext_header();
  }
  return std::make_tuple(tag_size_restriction_, text_enc_restriction_,
                         text_size_restriction_, image_enc_restriction_,
                         image_sz_restriction_);
}

scribbu::id3v2_4_tag::id3v2_4_tag(std::istream &is):
  id3v2_tag(is),
  experimental_(flags() & 0x20),
  footer_(flags() & 0x10)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                               text_parsers_.begin()));
  parse(is, flags() & 0x40);
}

scribbu::id3v2_4_tag::id3v2_4_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  experimental_(flags() & 0x20),
  footer_(flags() & 0x10)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                                  text_parsers_.begin()));
  parse(is, flags() & 0x40);
}

/*virtual*/ std::size_t
scribbu::id3v2_4_tag::play_count() const {
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

std::tuple<boost::shared_array<unsigned char>, std::size_t>
scribbu::id3v2_4_tag::decompress(const unsigned char *p,
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
scribbu::id3v2_4_tag::decrypt(const unsigned char *p,
                              std::size_t          cb,
                              unsigned char        method) const
{
  throw std::logic_error("unimplemented");
}

void scribbu::id3v2_4_tag::parse(std::istream &is, bool extended)
{
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
    // resynchronisation. If a footer is present this equals to ('total size' -
    // 20) bytes, otherwise ('total size' - 10) bytes.
    std::size_t cb_tag = size();

    // std::array's size is fixed at compile time, which we can't do, and
    // std::vector is permitted to allocate additional memory to accomodate
    // later insertions.
    std::unique_ptr<unsigned char[]> pb(new unsigned char[cb_tag]);
    is.read((char*)pb.get(), cb_tag);

    // In ID3v2.4, the header, extended header, and footer are all
    // sync-safe. Therefore, unsynchronisation is applied on a per-frame basis.

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
    std::size_t cb_frame = 0;
    bool found_encrypted = false;
    do {

      const unsigned char *pcurr = p0;

      for ( ; pcurr < p1; pcurr += cb_frame + 10) {

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

        cb_frame = unsigned_from_sync_safe(pcurr[4], pcurr[5], pcurr[6], pcurr[7]);

        if (ENCR != id) {
          continue;
        }

        if (pcurr + 11 > p1) {
          throw invalid_tag();
        }

        unsigned char f0 = p0[8];

        id3v2_4_frame::tag_alter_preservation tap = (0 != f0 & 0x40) ?
          id3v2_4_frame::tag_alter_preservation::discard :
          id3v2_4_frame::tag_alter_preservation::preserve;
        id3v2_4_frame::file_alter_preservation fap = (0 != f0 & 0x20) ?
          id3v2_4_frame::file_alter_preservation::discard :
          id3v2_4_frame::file_alter_preservation::preserve;
        id3v2_4_frame::read_only ro = (0 != f0 & 0x10) ?
          id3v2_4_frame::read_only::set :
          id3v2_4_frame::read_only::clear;

        unsigned char f1 = pcurr[9];

        // If the frame is compressed, encrypted, or grouped, there will be
        // additional bytes added starting at pcurr + 10. Since pcurr is used
        // in the loop test clause, we need to capture the 'current read point'
        // in another variable:
        const unsigned char *pread = pcurr + 10;
        std::size_t cb_read = cb_frame;
        boost::optional<unsigned char> group_id = boost::none;
        if (0 != (f1 & 0x40)) {
          group_id = *pread++;
          cb_read -= 1;
        }
        bool compressed = f1 & 0x80;
        boost::optional<unsigned char> encryption_method = boost::none;
        if (0 != (f1 & 0x04)) {
          encryption_method = *pread++;
          cb_read -= 1;
        }
        bool unsynchronisation = f1 & 0x02;
        boost::optional<std::size_t> data_len_ind;
        if (f1 & 0x01) {
          data_len_ind = unsigned_from_sync_safe(pread[0], pread[1], pread[2], pread[3]);
          pread += 4;
          cb_read -= 4;
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

        if (compressed) {
          std::tie(pf, cb_read) = decompress(pread, cb_read, data_len_ind.get());
          pread = pf.get();
        }

        scribbu::ENCR_2_4 encr(pread, pread + cb_read, tap, fap, ro,
                               encryption_method, group_id, compressed,
                               unsynchronisation, data_len_ind);

        register_encryption_method(encr);

      } // End iteration over all frames.

    } while (found_encrypted); // End while on encrypted ENCR frames.

    // Now that we've got all our encryption methods registered, walk the
    // frames one final time.
    for ( ; p0 < p1; p0 += cb_frame + 10) {

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

      cb_frame = unsigned_from_sync_safe(p0[4], p0[5], p0[6], p0[7]);

      unsigned char f0 = p0[8];

      id3v2_4_frame::tag_alter_preservation tap = (0 != f0 & 0x40) ?
        id3v2_4_frame::tag_alter_preservation::discard :
        id3v2_4_frame::tag_alter_preservation::preserve;
      id3v2_4_frame::file_alter_preservation fap = (0 != f0 & 0x20) ?
        id3v2_4_frame::file_alter_preservation::discard :
        id3v2_4_frame::file_alter_preservation::preserve;
      id3v2_4_frame::read_only ro = (0 != f0 & 0x10) ?
        id3v2_4_frame::read_only::set :
        id3v2_4_frame::read_only::clear;

      unsigned char f1 = p0[9];

      // If the frame is compressed, encrypted, or grouped, there will be
      // additional bytes added starting at pcurr + 10. Since pcurr is used
      // in the loop test clause, we need to capture the 'current read point'
      // in another variable:
      const unsigned char *pread = p0 + 10;
      std::size_t cb_read = cb_frame;
      boost::optional<unsigned char> group_id = boost::none;
      if (0 != (f1 & 0x40)) {
        group_id = *pread++;
        cb_read -= 1;
      }
      bool compressed = f1 & 0x80;
      boost::optional<unsigned char> encryption_method = boost::none;
      if (0 != (f1 & 0x04)) {
        encryption_method = *pread++;
        cb_read -= 1;
      }
      bool unsynchronisation = f1 & 0x02;
      boost::optional<std::size_t> data_len_ind;
      if (f1 & 0x01) {
        data_len_ind = unsigned_from_sync_safe(pread[0], pread[1], pread[2], pread[3]);
        pread += 4;
        cb_read -= 4;
      }

      // If we decrypt and/or decompress, we need to make a copy-- keep the
      // copy here...
      boost::shared_array<unsigned char> pf;

      if (boost::none != encryption_method) {
        if (! encryption_method_regd(encryption_method.get())) {
          // TODO: Throw custom exception
        }
        std::tie(pf, cb_read) = decrypt(pread, cb_read, encryption_method.get());
        pread = pf.get();
      }

      if (boost::none != data_len_ind) {
        std::tie(pf, cb_read) = decompress(pread, cb_read, data_len_ind.get());
        pread = pf.get();
      }

      // OK-- parse the frame (parse_frame will update all our internal
      // datastructures).
      parse_frame(id, cb_read, tap, fap, ro, encryption_method, group_id,
                  compressed, unsynchronisation, data_len_ind, pread,
                  pread + cb_read);

    } // End iteration over all frames.

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
  }

}

void
scribbu::id3v2_4_tag::parse_frame(
  const frame_id4 &id,
  std::size_t size,
  id3v2_4_frame::tag_alter_preservation tap,
  id3v2_4_frame::file_alter_preservation fap,
  id3v2_4_frame::read_only read_only,
  const boost::optional<unsigned char> &encmth,
  const boost::optional<unsigned char> &group_id,
  bool compressed,
  bool unsynchronisation,
  const boost::optional<std::size_t> & dli,
  const unsigned char *p0,
  const unsigned char *p1)
{
  using namespace std;

  static const frame_id4 COMMID("COMM"), PCNTID("PCNT");

  // COMM is handled specially-- the frame parser for "COMM" may not
  // be replaced
  if (COMMID == id) {
    COMM_2_4 *pcomm = new COMM_2_4(p0, p1, tap, fap, read_only, encmth,
                                   group_id, compressed, unsynchronisation,
                                   dli);
    comms_.push_back(make_pair(pcomm, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_4_frame>(pcomm));
  }
  else if (PCNTID == id) {
    PCNT_2_4 *pcnt = new PCNT_2_4(p0, p1, tap, fap, read_only, encmth,
                                  group_id, compressed, unsynchronisation,
                                  dli);
    pcnts_.push_back(make_pair(pcnt, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_4_frame>(pcnt));
  }
  else {
    // check to see if we have a text frame parser registered for `id'...
    auto pfn0 = text_parsers_.find(id);
    if (text_parsers_.end() != pfn0) {

      // we do: treat this frame as a text frame. Create an
      // id3v2_2_text_frame...
      auto ptr = pfn0->second(id, p0, p1 - p0,
                              tap,
                              fap,
                              read_only, encmth,
                              group_id, compressed,
                              unsynchronisation, dli);
      // enter the address thereof into our text frame index...
      text_map_.insert(make_pair(id, ptr.get()));
      // and move our ptr-to-id3v2_4_text_frame into our frame collection
      // as a ptr-to-id3v2_4_frame.
      frames_.push_back(unique_ptr<id3v2_4_frame>(std::move(ptr)));

      // NB The spec states that there may only be one text frame in a tag for
      // each identifier; however, counter-examples do exist in the wild.
    }
    else {

      // We do not-- check to see if we have a generic parser registered...
      auto pfn1 = generic_parsers_.find(id);
      if (generic_parsers_.end() == pfn1) {
        frames_.push_back(
          unique_ptr<id3v2_4_frame>(
            new unknown_id3v2_4_frame(id, tap,
                                      fap, read_only,
                                      encmth, group_id,
                                      compressed, unsynchronisation,
                                      dli, p0, p1)));
      }
      else {
        frames_.push_back(pfn1->second(id, p0, p1 - p0,
                                       tap,
                                       fap,
                                       read_only, encmth,
                                       group_id, compressed,
                                       unsynchronisation, dli));
      }
    }
  }

  // and finally update our id index.
  frame_map_.insert(std::make_pair(id, frames_.size() - 1));

}

/*static*/ bool
scribbu::id3v2_4_tag::parsing_is_reserved(const frame_id4 &id)
{
  using namespace std;

  vector<frame_id4> RSVD{ "COMM", "PCNT", "POPM" };

  return RSVD.end() != find(RSVD.begin(), RSVD.end(), id);
}

void scribbu::id3v2_4_tag::register_encryption_method(const ENCR_2_4 &encr)
{
  if (0 != encryption_methods_.count(encr.method_symbol())) {
    // TODO: Throw a custom exception, here
    throw std::runtime_error("duplicate encryption method");
  }

  encryption_methods_.insert(std::make_pair(encr.method_symbol(), encr));

}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string
scribbu::id3v2_4_tag::text_frame_as_str(
  const frame_id4 &id,
  encoding dst /*= encoding::UTF_8*/,
  on_no_encoding rsp /*= on_no_encoding::fail*/,
  const boost::optional<encoding> &src /*= boost::none*/) const
{
  return text_map_.find(id)->second->as_str<std::string>(dst, rsp, src);
}
