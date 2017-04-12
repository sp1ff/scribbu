#include <id3v23.hh>

#include <id3v2.hh>

#include <zlib.h>


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_2_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef std::unordered_map<scribbu::frame_id4,
                           scribbu::id3v2_3_tag::frame_parser>
def_reg_type;

typedef
std::unordered_map<scribbu::frame_id4,
                   scribbu::id3v2_3_tag::reserved_frame_parser>
reserved_reg_type;

static unsigned int nifty_counter_ = 0;

static typename
std::aligned_storage<sizeof(std::mutex), alignof(std::mutex)>::type
mutex_buf_;

static typename
std::aligned_storage< sizeof(def_reg_type), alignof(def_reg_type)>::type
map_buf_;

static typename
std::aligned_storage< sizeof(reserved_reg_type),
                      alignof(reserved_reg_type)>::type
reserved_buf_;

/*static*/ std::mutex&
scribbu::id3v2_3_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_reg_type&
scribbu::id3v2_3_tag::default_parsers_ =
  reinterpret_cast<def_reg_type&>(map_buf_);

/*static*/ reserved_reg_type&
scribbu::id3v2_3_tag::reserved_parsers_ =
  reinterpret_cast<reserved_reg_type&>(reserved_buf_);

scribbu::id3v2_3_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_3_tag::mutex_) std::mutex();
    new (&id3v2_3_tag::default_parsers_) def_reg_type();
    new (&id3v2_3_tag::reserved_parsers_) reserved_reg_type();

#   define REG(id, tag) \
    id3v2_3_tag::default_parsers_.insert( \
      std::make_pair(frame_id4((id)), tag::create)) \

#   define RREG(id, pfn) \
    id3v2_3_tag::reserved_parsers_.insert( \
        std::make_pair(frame_id4((id)), &id3v2_3_tag::pfn)) \

    RREG("COMM", create_COMM);
    RREG("PCNT", create_PCNT);
    RREG("TXXX", create_TXXX);
    RREG("UFID", create_UFID);
    RREG("TALB", create_text_frame);
    RREG("TBPM", create_text_frame);
    RREG("TCOM", create_text_frame);
    RREG("TCON", create_text_frame);
    RREG("TCOP", create_text_frame);
    RREG("TDAT", create_text_frame);
    RREG("TDLY", create_text_frame);
    RREG("TENC", create_text_frame);
    RREG("TEXT", create_text_frame);
    RREG("TFLT", create_text_frame);
    RREG("TIME", create_text_frame);
    RREG("TIT1", create_text_frame);
    RREG("TIT2", create_text_frame);
    RREG("TIT3", create_text_frame);
    RREG("TKEY", create_text_frame);
    RREG("TLAN", create_text_frame);
    RREG("TLEN", create_text_frame);
    RREG("TMED", create_text_frame);
    RREG("TOAL", create_text_frame);
    RREG("TOFN", create_text_frame);
    RREG("TOLY", create_text_frame);
    RREG("TOPE", create_text_frame);
    RREG("TORY", create_text_frame);
    RREG("TOWN", create_text_frame);
    RREG("TPE1", create_text_frame);
    RREG("TPE2", create_text_frame);
    RREG("TPE3", create_text_frame);
    RREG("TPE4", create_text_frame);
    RREG("TPOS", create_text_frame);
    RREG("TPUB", create_text_frame);
    RREG("TRCK", create_text_frame);
    RREG("TRDA", create_text_frame);
    RREG("TRSN", create_text_frame);
    RREG("TRSO", create_text_frame);
    RREG("TSIZ", create_text_frame);
    RREG("TSRC", create_text_frame);
    RREG("TSSE", create_text_frame);
    RREG("TYER", create_text_frame);

    REG("ENCR", ENCR);
    REG("POPM", POPM);

#   undef RREG
#   undef REG

  }
}

scribbu::id3v2_3_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_3_tag::mutex_)->~mutex();
    (&id3v2_3_tag::default_parsers_)->~unordered_map();
    (&id3v2_3_tag::reserved_parsers_)->~unordered_map();
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
  get_default_frame_parsers(std::inserter(parsers_, parsers_.begin()));
  parse(is, flags() & 0x40);
}

scribbu::id3v2_3_tag::id3v2_3_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  experimental_(flags() & 0x20)
{
  get_default_frame_parsers(std::inserter(parsers_, parsers_.begin()));
  parse(is, flags() & 0x40);
}

/*virtual*/ void
scribbu::id3v2_3_tag::accept_for_print(id3v2_acyclic_visitor &V,
                                       std::ostream          &os) const
{
  id3v2_3_tag_printer *p = dynamic_cast<id3v2_3_tag_printer*>(&V);
  if (p) {
    p->print_on(os, *this);
  }

  for (std::ptrdiff_t i = 0; i < frames_.size(); ++i) {

    if (comments_.count(i)) {
      comments_.at(i)->accept_for_print(V, os);
    }
    else if (play_counts_.count(i)) {
      play_counts_.at(i)->accept_for_print(V, os);
    }
    else if (ufids_.count(i)) {
      ufids_.at(i)->accept_for_print(V, os);
    }
    else if (udts_.count(i)) {
      udts_.at(i)->accept_for_print(V, os);
    }
    else if (texts_pos_.count(i)) {
      texts_pos_.at(i)->accept_for_print(V, os);
    }
    else {
      frames_.at(i)->accept_for_print(V, os);
    }

  } // End iteration over frames.

} // End id3v2_3_tag::accept_for_print.

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::all_comments(std::vector<scribbu::comments> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id4 ID("COMM");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const COMM &F = dynamic_cast<const COMM&>(*frames_[i]);
      out.push_back(F.data());
      ++nout;
    }
  }

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::all_play_counts(std::vector<scribbu::play_count> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id4 ID("PCNT");

  auto pend = frame_map_.end();

  auto p = frame_map_.find(ID);
  if (pend == p) {
    return 0;
  }

  std::size_t nout = 0;
  do {
    const PCNT &F = dynamic_cast<const PCNT&>(*frames_[p->second]);
    out.push_back(F.count());
    ++p;
    ++nout;
  } while (pend != p && ID == p->first);

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::all_ufids(std::vector<scribbu::unique_file_id> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id4 ID("UFID");

  auto pend = frame_map_.end();

  auto p = frame_map_.find(ID);
  if (pend == p) {
    return 0;
  }

  std::size_t nout = 0;
  do {
    const UFID &F = dynamic_cast<const UFID&>(*frames_[p->second]);
    out.push_back(F.file_id());
    ++p;
    ++nout;
  } while (pend != p && ID == p->first);

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::all_udts(std::vector<scribbu::user_defined_text> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast
  using scribbu::frame_id4;
  using scribbu::TXXX;

  const frame_id4 ID("TXXX");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const TXXX &F = dynamic_cast<const TXXX&>(*frames_[i]);
      out.push_back(F.udt());
      ++nout;
    }
  }

  return nout;

}

/*static*/
bool
scribbu::id3v2_3_tag::has_framework_parser(const frame_id4 &id)
{
  return "COMM" == id || "PCNT" == id || "UFID" == id ||
    "TXXX" == id || id.text();
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
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);
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

      // OK-- parse the frame & add it to our collection...
      frames_.push_back(parse_frame(id, tap, fap, ro, encryption_method,
                                    group_id, decompressed_size,
                                    pread, pread + cb_read));
      // & note the position of the new frame in our map.
      frame_map_.insert(std::make_pair(id, frames_.size() - 1));

    } // End iteration over frames.

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
  }
}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::parse_frame(const frame_id4                       &id,
                                  id3v2_3_frame::tag_alter_preservation  tag_alter_preservation,
                                  id3v2_3_frame::file_alter_preservation file_alter_preservation,
                                  id3v2_3_frame::read_only               read_only,
                                  const boost::optional<unsigned char>  &encryption_method,
                                  const boost::optional<unsigned char>  &group_id,
                                  const boost::optional<std::size_t>    &decompressed_size,
                                  const unsigned char *                  p0,
                                  const unsigned char *                  p1) const
{
  parser_map_type::const_iterator p = parsers_.find(id);
  if (parsers_.end() == p) {
    return std::unique_ptr<id3v2_3_frame>(
      new unknown_id3v2_3_frame(id, tag_alter_preservation,
                                file_alter_preservation, read_only,
                                encryption_method, group_id,
                                decompressed_size, p0, p1));
  }
  else {
    return p->second(id, p0, p1 - p0,
                     tag_alter_preservation,
                     file_alter_preservation,
                     read_only,
                     encryption_method,
                     group_id,
                     decompressed_size);
  }
}

void scribbu::id3v2_3_tag::register_encryption_method(const ENCR &encr) {

  if (0 != encryption_methods_.count(encr.get_method().method_symbol())) {
    // TODO: Throw a custom exception, here
  }

  encryption_methods_.insert(std::make_pair(encr.get_method().method_symbol(), encr));

}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::create_COMM(
  const frame_id4                    &/*id*/,
  std::ptrdiff_t                        i,
  const unsigned char                  *p,
  std::size_t                           cb,
  tag_alter_preservation                tag_alter_preservation,
  file_alter_preservation               file_alter_preservation,
  read_only                             read_only,
  const boost::optional<unsigned char> &encryption_method,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t>   &decompressed_size)
{
  // TODO: Make this exception-safe
  COMM *pf = new COMM(p,
                      p + cb,
                      tag_alter_preservation,
                      file_alter_preservation,
                      read_only,
                      encryption_method,
                      group_id,
                      decompressed_size);
  comments_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_3_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::create_PCNT(
  const frame_id4                    &/*id*/,
  std::ptrdiff_t                        i,
  const unsigned char                  *p,
  std::size_t                           cb,
  tag_alter_preservation                tag_alter_preservation,
  file_alter_preservation               file_alter_preservation,
  read_only                             read_only,
  const boost::optional<unsigned char> &encryption_method,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t>   &decompressed_size)
{
  // TODO: Make this exception-safe
  PCNT *pf = new PCNT(p,
                      p + cb,
                      tag_alter_preservation,
                      file_alter_preservation,
                      read_only,
                      encryption_method,
                      group_id,
                      decompressed_size);
  play_counts_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_3_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::create_UFID(
  const frame_id4                   & /*id*/,
  std::ptrdiff_t                        i,
  const unsigned char                  *p,
  std::size_t                           cb,
  tag_alter_preservation                tag_alter_preservation,
  file_alter_preservation               file_alter_preservation,
  read_only                             read_only,
  const boost::optional<unsigned char> &encryption_method,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t>   &decompressed_size)
{
  // TODO: Make this exception-safe
  UFID *pf = new UFID(p,
                      p + cb,
                      tag_alter_preservation,
                      file_alter_preservation,
                      read_only,
                      encryption_method,
                      group_id,
                      decompressed_size);
  ufids_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_3_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::create_TXXX(
  const frame_id4                    &/*id*/,
  std::ptrdiff_t                        i,
  const unsigned char                  *p,
  std::size_t                           cb,
  tag_alter_preservation                tag_alter_preservation,
  file_alter_preservation               file_alter_preservation,
  read_only                             read_only,
  const boost::optional<unsigned char> &encryption_method,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t>   &decompressed_size)
{
  // TODO: Make this exception-safe
  TXXX *pf = new TXXX(p,
                      p + cb,
                      tag_alter_preservation,
                      file_alter_preservation,
                      read_only,
                      encryption_method,
                      group_id,
                      decompressed_size);
  udts_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_3_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_tag::create_text_frame(
  const frame_id4                      &id,
  std::ptrdiff_t                        i,
  const unsigned char                  *p,
  std::size_t                           cb,
  tag_alter_preservation                tag_alter_preservation,
  file_alter_preservation               file_alter_preservation,
  read_only                             read_only,
  const boost::optional<unsigned char> &encryption_method,
  const boost::optional<unsigned char> &group_id,
  const boost::optional<std::size_t>   &decompressed_size)
{
  // TODO: Make this exception-safe
  id3v2_3_text_frame *pf = new id3v2_3_text_frame(id,
                                                  p,
                                                  p + cb,
                                                  tag_alter_preservation,
                                                  file_alter_preservation,
                                                  read_only,
                                                  encryption_method,
                                                  group_id,
                                                  decompressed_size);
  texts_pos_.insert(std::make_pair(i, pf));
  text_.insert(std::make_pair(id, pf));
  return std::unique_ptr<scribbu::id3v2_3_frame>( pf );
}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string scribbu::id3v2_3_tag::text_frame_as_utf8(const frame_id4 &id) const
{
  std::size_t n = frame_map_.count(id);
  if (0 == n) {
    throw unknown_frame_error(id);
  } else if (1 != n) {
    throw duplicate_frame_error(id, n);
  }

  std::ptrdiff_t idx = frame_map_.find(id)->second;

  // TODO: Re-evaluate this to see if I can re-structure in such a way as to
  // not need the dynamic cast...
  const id3v2_3_text_frame &F = dynamic_cast<const id3v2_3_text_frame&>( *frames_[idx].get() );

  return F.as_utf8();

}
