/**
 * \file id3v23.cc
 *
 * Copyright (C) 2015-2018 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#include <id3v23.hh>

#include <framesv2.hh>
#include <id3v2.hh>

#include <algorithm>
#include <numeric>

#include <arpa/inet.h>
#include <zlib.h>

const char PAD[] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const size_t CBPAD = sizeof(PAD);


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_3_tag nifty counter                         //
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
//                        class id3v2_3_tag::ext_header                      //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::id3v2_3_tag::invalid_ext_header::what() const noexcept
{
  return "invalid extended header";
}

scribbu::id3v2_3_tag::ext_header::ext_header(const unsigned char *p0,
                                             const unsigned char *p1):
  dirty_(true)
{
  using scribbu::detail::uint32_from_non_sync_safe;
  using scribbu::detail::unsigned_from_non_sync_safe;

  std::size_t cb = unsigned_from_non_sync_safe(p0[0], p0[1], p0[2], p0[3]);

  bool fcrc = p0[4] & 0x80;

  if ( (fcrc && cb != 10) || (!fcrc && cb !=6 ) ) {
    throw invalid_ext_header();
  }

  cbpad_ = unsigned_from_non_sync_safe(p0[6], p0[7], p0[8], p0[9]);

  if (fcrc) {
    crc_ = uint32_from_non_sync_safe(p0[10], p0[11], p0[12], p0[13]);
  }
}

/// Construct a fresh, new extended header; if a CRC is desired, pass the tag
/// whose frames are to be checksummed
scribbu::id3v2_3_tag::ext_header::ext_header(std::size_t        cbpad,
                                             const id3v2_3_tag *ptag_for_crc /*= std::nullptr*/):
  cbpad_(cbpad), dirty_(true)
{
  if (ptag_for_crc) {
    crc(*ptag_for_crc);
  }
}

/// Compute the size on disk
std::size_t
scribbu::id3v2_3_tag::ext_header::serialized_size(bool unsync /*= false*/) const
{
  ensure_cached_data_is_fresh();
  return cache_[unsync ? SERIALIZED_WITH_UNSYNC : SERIALIZED].size();
}

/// True if the serialized form would contain false syncs
std::size_t
scribbu::id3v2_3_tag::ext_header::needs_unsynchronisation() const
{
  ensure_cached_data_is_fresh();
  return num_false_syncs_;
}

std::uint32_t
scribbu::id3v2_3_tag::ext_header::crc(const id3v2_3_tag &tag) const
{
  using namespace std;
  uint32_t crc = accumulate(tag.begin(), tag.end(), crc32(0L, Z_NULL, 0),
                            [](uint32_t checksum, const id3v2_3_frame &F)
                            { return F.crc(checksum); });
  if ( ! (bool)crc_ || *crc_ != crc ) {
    dirty_ = true;
    crc_ = crc;
  }
  return *crc_;
}

std::size_t
scribbu::id3v2_3_tag::ext_header::write(std::ostream &os, bool unsync) const
{
  using namespace std;
  ensure_cached_data_is_fresh();
  size_t idx = unsync ? SERIALIZED : SERIALIZED_WITH_UNSYNC;
  os.write((char*)&cache_[idx][0], cache_[idx].size());
  return cache_[idx].size();
}

void
scribbu::id3v2_3_tag::ext_header::ensure_cached_data_is_fresh() const
{
  using namespace std;
  using scribbu::detail::count_false_syncs;
  using scribbu::detail::unsynchronise;

  if ( !dirty_ ) {
    return;
  }

  cache_[SERIALIZED].clear();
  cache_[SERIALIZED_WITH_UNSYNC].clear();

  stringstream stm;

  size_t cbhdr = size();
  if (cbhdr > 0xffffffff) {
    throw invalid_ext_header();
  }

  cbhdr = htonl(cbhdr);
  stm.write((char*)&cbhdr, 4);

  char buf[2] = { 0x00, 0x00 };
  if (has_crc()) {
    buf[0] = 0x80;
  }
  stm.write(buf, 2);

  size_t cbpad = padding_size();
  cbpad = htonl(cbpad);
  stm.write((char*)&cbpad, 4);

  if (has_crc()) {
    uint32_t crc32 = crc();
    crc32 = htonl(crc32);
    stm.write((char*)&crc32, 4);
  }

  string sbuf = stm.str();
  copy(sbuf.begin(), sbuf.end(), back_inserter(cache_[SERIALIZED]));
  num_false_syncs_ = count_false_syncs(cache_[SERIALIZED].begin(),
                                       cache_[SERIALIZED].end());
  unsynchronise(back_inserter(cache_[SERIALIZED_WITH_UNSYNC]),
                cache_[SERIALIZED].begin(),
                cache_[SERIALIZED].end());

  dirty_ = false;
}


///////////////////////////////////////////////////////////////////////////////
//                             class id3v2_3_tag                             //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_3_tag::id3v2_3_tag(std::istream &is):
  id3v2_tag(is),
  padding_(0)
{
  get_default_generic_frame_parsers(
    std::inserter(generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(
    text_parsers_, text_parsers_.begin()));

  // id3v2_tag has consumed the first five bytes of the header-- this call will
  // consume the next five...
  std::size_t size;
  unsigned char flags;
  std::tie(flags, size) = parse_flags_and_size(is);

  unsynchronised(0 != (flags & 0x80));
  experimental_ = flags & 0x20;
  parse(is, size, flags & 0x40);
}

scribbu::id3v2_3_tag::id3v2_3_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  experimental_(H.flags_ & 0x20),
  padding_(0)
{
  get_default_generic_frame_parsers(
    std::inserter(generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(
    std::inserter(text_parsers_, text_parsers_.begin()));
  parse(is, H.size_, H.flags_ & 0x40);
}

scribbu::id3v2_3_tag::id3v2_3_tag(std::size_t cbpad /*= 0*/,
                                  bool fexp /*= false*/,
                                  want_extended_header ext /*= want_extended_header::none*/):
  id3v2_tag(3, 0),
  experimental_(fexp),
  padding_(cbpad)
{
  using namespace std;
  if (want_extended_header::with_crc == ext) {
    pext_header_ = make_shared<ext_header>(cbpad, this);
  } else if (want_extended_header::present == ext) {
    pext_header_ = make_shared<ext_header>(cbpad);
  }
}

/*virtual*/ unsigned char
scribbu::id3v2_3_tag::flags() const
{
  unsigned char flags = 0;

  boost::optional<bool> unsync = unsynchronised();
  if (unsync && *unsync) {
    flags |= 0x80;
  }
  if (pext_header_) {
    flags |= 0x40;
  }
  if (experimental_) {
    flags |= 0x20;
  }
  return flags;
}


/////////////////////////////////////////////////////////////////////////////
//                          ID3v2 Serialization                            //
/////////////////////////////////////////////////////////////////////////////

/**
 * \brief Compute the size of this tag (in bytes) exclusive of the ID3v2 header
 * (i.e. return the total tag size, in bytes, less ten)
 *
 *
 * \param unync [in] The caller shall set this to true to apply the
 * unsynchronisation scheme when computing the tag size
 *
 * \return The serialized size of this tag, in bytes, exclusive of the ID3v2
 * header (i.e. the value that could be written in bytes six through nine of
 * the tag)
 *
 *
 * Computing tag size becomes more complex with ID3v2.3.  Both the potential
 * presence of a checksum (in the extended header) and the possible encryption
 * and/or compression of individual frames makes the calculation much more
 * complex.
 *
 * This implemenentation proceeds as follows:
 *
 * - the ID3v2 header is always ten bytes, regardless of whether
 *   unsynchronisation is being applied, and is not counted in any case
 *
 * - \em if there is an extended header:
 *
 *   + if unsynchronisation is \em not being applied, it will be 6 or 10 bytes,
 *     depending on whether a CRC checksum is present
 *
 *   + if unsynchronisation \em is being applied:
 *
 *     * if there is no CRC checksum, it will still be 6 bytes (there is no chance
 *       of a false sync in this case)
 *
 *     * otherwise, we have to compute the CRC checksum, then check it for
 *       false syncs
 *
 *       * ask each frame to serialize itself \em without the unsynchronisation
 *         scheme & update a CRC-32 checksum
 *
 *       * the extended header will contribute 10 plus the number of false
 *         syncs or $ff 00 pairs in the checksum
 *
 * - ask each frame to compute its serialized size, incorporating the
 *   unsynchronisation scheme if requested:
 *
 *   + compressed frames shall apply compression
 *
 *   + encrypted frames shall apply encyrption (in this order)
 *
 *   + if unsynchronisation is being applied, all frames will need to return
 *     their serialized size plus the number of false syncs and $ff 00 pairs
 *     found in their serialization
 *
 * - add the padding, if any
 *
 *
 * \todo If \a unsync is requested, and the last byte of the last frame is
 * 0xff, we need to add one.
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::size(bool unsync) const
{
  using namespace std;

  // We do not include the ID3v2 header...
  size_t cb = 0;

  // but we *do* include the extended header, if it's present...
  if (pext_header_) {
    if (pext_header_->has_crc()) {
      pext_header_->crc(*this);
    }
    cb += pext_header_->serialized_size(unsync);
  }

  // we of course include all our frames...
  cb += accumulate(begin(), end(), 0,
                   [unsync](size_t n, const id3v2_3_frame &f)
                   { return n + f.serialized_size(unsync); });

  cb += padding();

  return cb;

}

/**
 * \brief Return true if this the serialization of this tag would contain false
 * syncs if serialized in its present state
 *
 *
 * With ID3v2.3, this calculation becomes slightly more complex, due to the
 * (potential) presence of an extended header, which may in turn contain a
 * checksum.
 *
 * - the ID3v2 header is synch-safe, so that contributes zero false syncs
 *
 * - \em if there is an ID3v2.3 extended header presesnt
 *
 *   + the last byte of the ID3v2.3 header is sync-safe, so there's no chance
 *     of a false sync between the header & extended header
 *
 *   + the size & padding may contain false syncs
 *
 *   + if there is a CRC checksum present, we'll need to calculate it to check
 *     for false synchs; this will mean asking each frame to serialize itself
 *     \em without the unsynchronisation scheme
 *
 *  - ask each frame to check itself for false syncs
 *
 *  - padding is synchsafe, so no need to count
 *
 *
 * \todo If the last byte of the last frame is 0xff, that's a false sync
 *
 *
 */

/*virtual*/
bool
scribbu::id3v2_3_tag::needs_unsynchronisation() const
{

  using namespace std;

  if (pext_header_) {
    if (pext_header_->has_crc()) {
      pext_header_->crc(*this);
    }
    if (pext_header_->needs_unsynchronisation()) {
      return true;
    }
  }

  return any_of(begin(), end(), [](const id3v2_3_frame &F) { return F.needs_unsynchronisation(); });
}

/**
 * \brief Serialize this tag to an output stream, perhaps applying the
 * unsynchronisation scheme if the caller so chooses ("unsynchronised" will be
 * updated accordingly)
 *
 *
 * \param os [in] std output stream to which this tag shall be written
 *
 * \param unsync [in] the caller shall set this true to apply unsynchronisation while writing
 *
 *
 * Serialization, too, becomes more complex with ID3v2.3. The implementation
 * proceeds as follows:
 *
 * - write the ID3v2 header
 *
 *   + call size() with the \a unsync parameter to get the tag size
 *
 *   + the header is synch-safe, so no need to unsynchronise further
 *
 * - \em if there is an extended header
 *
 *   + write the extended header size, flags, & sizeof padding, applying
 *     unsynchronisation if requested
 *
 *   + if the CRC checksum is present, compute it & write it, applying
 *     unsynchronisation, if requested
 *
 *     * the checksum shall be calculated on the collected frames, \em prior to
 *       unsynchronisations, but \em after encryption & compression
 *
 *     * this will involve asking each frame to serialize itself \em without
 *       unsynchronisation
 *
 * - write the frames
 *
 * - write the padding; this is synch-safe, so no need to worry bout it
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_3_tag::write(std::ostream &os, bool unsync) const
{
  using namespace std;

  write_header(os, flags(), size(unsync));
  size_t cb = 10;

  if (pext_header_) {
    if (pext_header_->has_crc()) {
      pext_header_->crc(*this);
    }
    cb += pext_header_->write(os, unsync);
  }

  cb = accumulate(begin(), end(), cb,
                  [&os, unsync](size_t n, const id3v2_3_frame &f)
                  { return n + f.write(os, unsync); });

  size_t cbpad = padding();
  while (cbpad) {
    size_t towrite = min(cbpad, CBPAD);
    os.write(PAD, towrite);
    cb += towrite;
    cbpad -= towrite;
  }

  return cb;
}

/////////////////////////////////////////////////////////////////////////////
//                    Frames Common to all ID3v2 Tags                      //
/////////////////////////////////////////////////////////////////////////////

/*virtual*/ std::size_t
scribbu::id3v2_3_tag::play_count() const {
  switch (has_play_count()) {
  case 1:
    return pcnts_.front().first->count();
  case 0:
    throw std::logic_error("no play counts");
  default:
    throw std::logic_error("multiple play counts");
  }
}

/*virtual*/
void
scribbu::id3v2_3_tag::add_comment(const std::string &text,
                                  language lang /*= language::from_locale*/,
                                  encoding src /*= encoding::UTF_8*/,
                                  use_unicode unicode /*= use_unicode::no*/,
                                  const std::string &description /*= std::string()*/,
                                  on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::unique_ptr<COMM> pnew = std::make_unique<COMM>(lang, text, src, unicode,
                                                      tag_alter_preservation::preserve,
                                                      file_alter_preservation::preserve,
                                                      read_only::clear,
                                                      boost::none,
                                                      boost::none,
                                                      description);
  const COMM &F = *pnew;
  std::size_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, d);
}

/*virtual*/
void
scribbu::id3v2_3_tag::add_user_defined_text(const std::string &text,
                                            encoding src /*= encoding::UTF_8*/,
                                            use_unicode unicode /*= use_unicode::no*/,
                                            const std::string &description /*= std::string()*/,
                                            on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::unique_ptr<TXXX> pnew = std::make_unique<TXXX>(text, src, unicode,
                                                      tag_alter_preservation::preserve,
                                                      file_alter_preservation::preserve,
                                                      read_only::clear,
                                                      boost::none,
                                                      boost::none,
                                                      description);
  const TXXX &F = *pnew;
  std::size_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, d);
}

///////////////////////////////////////////////////////////////////////////
//                           tag as container                            //
///////////////////////////////////////////////////////////////////////////

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(mutable_frame_proxy &&that)
{
  using namespace scribbu;

  static const frame_id4 COMID("COMM"), CNTID("PCNT"), POPID("POPM");

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(that.p_->frames_[that.idx_]);
  idx_ = that.idx_;

  id3v2_3_tag::frames_type::iterator pnew = p_->frames_.begin() + idx_;
  frame_id4 id = (**pnew).id();
  if (id == CNTID) {
    PCNT &frame = dynamic_cast<PCNT&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == COMID) {
    COMM &frame = dynamic_cast<COMM&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == POPID) {
    POPM &frame = dynamic_cast<POPM&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id.text_frame()) {
    id3v2_3_text_frame &frame = dynamic_cast<id3v2_3_text_frame&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else {
    id3v2_3_frame &frame = *(pnew->get());
    p_->add_frame_to_lookups(frame, idx_);
  }

  return *this;
}

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(const id3v2_3_frame &frame)
{
  std::unique_ptr<id3v2_3_frame> pnew(frame.clone());

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(pnew);
  p_->add_frame_to_lookups(*(p_->frames_[idx_]), idx_);

  return *this;
}

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(const id3v2_3_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<id3v2_3_text_frame> pnew(new id3v2_3_text_frame(frame));
  // and saving a reference to that copy.
  id3v2_3_text_frame &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(const PCNT &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<PCNT> pnew(new PCNT(frame));
  // and saving a reference to that copy.
  PCNT &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(const COMM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<COMM> pnew(new COMM(frame));
  // and saving a reference to that copy.
  COMM &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_3_tag::mutable_frame_proxy&
scribbu::id3v2_3_tag::mutable_frame_proxy::operator=(const POPM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<POPM> pnew(new POPM(frame));
  // and saving a reference to that copy.
  POPM &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::insert(const_iterator p, const id3v2_3_frame &frame)
{
  std::unique_ptr<id3v2_3_frame> pnew(frame.clone());

  auto p1 = frames_.emplace(frames_.cbegin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(*frames_[d], d);

  return iterator(this, p1);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::insert(const_iterator p, const id3v2_3_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<id3v2_3_text_frame> pnew = std::make_unique<id3v2_3_text_frame>(frame);
  id3v2_3_text_frame &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::insert(const_iterator p, const PCNT &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<PCNT> pnew = std::make_unique<PCNT>(frame);
  PCNT &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::insert(const_iterator p, const COMM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<COMM> pnew = std::make_unique<COMM>(frame);
  COMM &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);
  return iterator(this, p1);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::insert(const_iterator p, const POPM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<POPM> pnew = std::make_unique<POPM>(frame);
  POPM &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);
  return iterator(this, p1);
}

void
scribbu::id3v2_3_tag::push_back(const id3v2_3_frame &frame)
{
  std::unique_ptr<id3v2_3_frame> pnew(frame.clone());

  frames_.emplace_back(std::move(pnew));
  std::size_t d = frames_.size() - 1;
  add_frame_to_lookups(*frames_[d], d);
}

void
scribbu::id3v2_3_tag::push_back(const id3v2_3_text_frame &frame)
{
  using namespace std;
  auto pnew = make_unique<id3v2_3_text_frame>(frame);
  id3v2_3_text_frame &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_3_tag::push_back(const PCNT &frame)
{
  using namespace std;
  auto pnew = make_unique<PCNT>(frame);
  PCNT &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_3_tag::push_back(const COMM &frame)
{
  using namespace std;
  auto pnew = make_unique<COMM>(frame);
  COMM &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_3_tag::push_back(const POPM &frame)
{
  using namespace std;
  auto pnew = make_unique<POPM>(frame);
  POPM &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::erase(const_iterator p)
{
  std::size_t idx = p - begin();
  remove_frame_from_lookups(p->id(), idx);
  frames_.erase(frames_.begin() + idx);
}

scribbu::id3v2_3_tag::iterator
scribbu::id3v2_3_tag::erase(const_iterator p0, const_iterator p1)
{
  const_iterator p2 = p0;
  for (size_t i = p2 - begin(); p2 != p1; ++p2, ++i) {
    remove_frame_from_lookups(p2->id(), i);
  }

  frames_.erase(frames_.begin() + p0.index(),
                frames_.begin() + p1.index());
}

std::ostream&
scribbu::id3v2_3_tag::write_header(std::ostream &os,
                                   unsigned char flags,
                                   std::size_t cb) const
{
  unsigned char buf[] = { 'I', 'D', '3', 3, 0, flags };
  os.write((const char*)buf, sizeof(buf));
  detail::sync_safe_from_unsigned(cb, buf);
  os.write((const char*)buf, 4);
  return os;
}

void
scribbu::id3v2_3_tag::remove_frame_from_lookups(const frame_id4 &id, std::size_t idx)
{
  static const frame_id4 COM("COMM"), CNT("PCNT"), POP("POPM");

  if (COM == id) {
    comm_frame_lookup_type::iterator p =
      std::find_if(comms_.begin(), comms_.end(),
                   [idx](const comm_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    comms_.erase(p);
  }
  else if (CNT == id) {
    pcnt_frame_lookup_type::iterator p =
      std::find_if(pcnts_.begin(), pcnts_.end(),
                   [idx](const pcnt_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    pcnts_.erase(p);
  }
  else if (POP == id) {
    popm_frame_lookup_type::iterator p =
      std::find_if(popms_.begin(), popms_.end(),
                   [idx](const popm_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    popms_.erase(p);
  }
  else if (id.text_frame()) {
    // TODO(sp1ff): This is awful-- better to store the index in text_map_, but
    // I want to get this working, first.
    const id3v2_3_frame *pf = frames_[idx].get();
    text_frame_lookup_type::iterator p =
      std::find_if(text_map_.begin(), text_map_.end(),
                   [pf, id](const text_frame_lookup_type::value_type &x)
                   { return x.first == id && x.second == pf; });
    text_map_.erase(p);
  }

  frame_lookup_type::iterator pflu =
    std::find_if(frame_map_.begin(), frame_map_.end(),
                 [idx, id](const frame_lookup_type::value_type &x)
                 { return x.first == id && x.second == idx; });
  frame_map_.erase(pflu);
}

void
scribbu::id3v2_3_tag::add_frame_to_lookups(const id3v2_3_frame &frame, std::size_t idx)
{
  using namespace std;
  frame_map_.insert(make_pair(frame.id(), idx));
}

void
scribbu::id3v2_3_tag::add_frame_to_lookups(id3v2_3_text_frame &frame, std::size_t idx)
{
  using namespace std;
  text_map_.insert(make_pair(frame.id(), &frame));
  add_frame_to_lookups((const id3v2_3_frame&)frame, idx);
}

void
scribbu::id3v2_3_tag::add_frame_to_lookups(PCNT &frame, std::size_t idx)
{
  using namespace std;
  pcnts_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_3_frame&)frame, idx);
}

void
scribbu::id3v2_3_tag::add_frame_to_lookups(COMM &frame, std::size_t idx)
{
  using namespace std;
  comms_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_3_frame&)frame, idx);
}

void
scribbu::id3v2_3_tag::add_frame_to_lookups(POPM &frame, std::size_t idx)
{
  using namespace std;
  popms_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_3_frame&)frame, idx);
}

/// Not thread-safe
bool
scribbu::id3v2_3_tag::register_generic_frame_parser(
  const frame_id4 &id,
  const generic_frame_parser &F) {
  if (parsing_is_reserved(id)) {
    throw reserved_frame_error(id);
  }
  generic_parsers_.insert(std::make_pair(id, F)).first;
}

bool
scribbu::id3v2_3_tag::register_text_frame_parser(const frame_id4 &id,
                                                 const text_frame_parser &F) {
  if (parsing_is_reserved(id)) {
    throw reserved_frame_error(id);
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
  throw std::logic_error("unimplemented");
}

void scribbu::id3v2_3_tag::parse(std::istream &is, std::size_t size, bool extended)
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
    std::size_t cb_tag = size;

    // std::array's size is fixed at compile time, which we can't do, and
    // std::vector is permitted to allocate additional memory to accomodate
    // later insertions.
    std::unique_ptr<unsigned char[]> pb(new unsigned char[cb_tag]);
    is.read((char*)pb.get(), cb_tag);

    // In ID3v2.3, the extended header and frames are *not* sync-safe, so we
    // resynchronise here (unlike, say, 2.4).
    boost::optional<bool> unsync = unsynchronised();
    if (unsync && *unsync) {
      cb_tag = resynchronise(pb.get(), cb_tag);
    }

    const unsigned char *p0 = pb.get();
    const unsigned char *p1 = p0 + cb_tag;

    // Unlike ID3v2.2, we may have an extended header next.
    if (extended) {
      pext_header_.reset(new ext_header(p0, p1));
      // N.B. ID3v2.3 extended header `size' excludes the four bytes
      // in the header expressing the size.
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

      // `cb_tag' is the size of the tag on disk (i.e. after encryption,
      // compression & unsynchronisation), exclusive of header & footer (if
      // any). `cb_frame' is the size of the frame on disk (i.e. after...)
      // exclusive of the frame header. IOW, a minimal tag, one with only the
      // standard ID3v2 header, no padding, and this frame, would satisfy:

      //     cb_tag = cb_frame + 10

      // Therefore, we have cb_frame < cb_tag. If this relationship isn't
      // satisfied, it's a good bet that this tag is either corrupt or
      // was written incorrectly.
      if (cb_frame >= cb_tag) {
        throw invalid_tag();
      }

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

  is.exceptions(exc_mask);
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

  static const frame_id4 COMMID("COMM"), PCNTID("PCNT"), POPMID("POPM");

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
  else if (POPMID == id) {
    POPM *popm = new POPM(p0, p1, tap, fap, read_only, encmth,
                          group_id, dcsz);
    popms_.push_back(make_pair(popm, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_3_frame>(popm));
  }
  else {

    // check to see if we have a text frame parser registered for `id'...
    auto pfn0 = text_parsers_.find(id);
    if (text_parsers_.end() != pfn0) {

      // we do: treat this frame as a text frame. Create an
      // id3v2_3_text_frame...
      auto ptr = pfn0->second(id, p0, p1 - p0, tap,
                              fap, read_only,
                              encmth, group_id,
                              dcsz);
      // enter the address thereof into our text frame index...
      text_map_.insert(make_pair(id, ptr.get()));
      // and move our ptr-to-id3v2_3_text_frame into our frame collection
      // as a ptr-to-id3v2_3_frame.
      frames_.push_back(unique_ptr<id3v2_3_frame>(std::move(ptr)));

      // NB The spec states that there may only be one text frame in a tag for
      // each identifier; however, counter-examples do exist in the wild.
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
    throw std::invalid_argument("duplicate encryption method");
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
  return text_map_.find(id)->second->as_str<std::string>(dst, rsp, src);
}

/// Replace a text frame if it exists, append it otherwise
void scribbu::id3v2_3_tag::set_text_frame(const frame_id4 &id,
                                          const std::string &text,
                                          encoding src /*= encoding::UTF_8*/,
                                          bool add_bom /*= false*/,
                                          on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  using namespace std;
  auto p = text_map_.find(id);
  if (p != text_map_.end()) {
    p->second->set(text, src, add_bom, rsp);
  }
  else {
    auto p = make_unique<id3v2_3_text_frame>(id, text, src, add_bom, rsp, false);
    id3v2_3_text_frame &F = *p;
    frames_.push_back(unique_ptr<id3v2_3_text_frame>(move(p)));
    add_frame_to_lookups(F, frames_.size() - 1);
  }
}
