/**
 * \file id3v24.cc
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
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

#include <id3v24.hh>

#include <framesv24.hh>

#include <algorithm>
#include <numeric>

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
    REGG("XTAG", XTAG_2_4);

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
                                             const unsigned char *p1):
  size_dirty_(false),
  crc_dirty_ (false),
  fcrc_      (false)
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
 is_update_  = p0[5] & 0x40;
 fcrc_       = p0[5] & 0x20;
 restricted_ = p0[5] & 0x10;

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

  if (fcrc_) {
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

}

std::size_t
scribbu::id3v2_4_tag::ext_header::size() const
{
  if (size_dirty_) {
    throw std::logic_error("dirty ID3v2.3 extended header");
  }
  return size_;
}

std::uint32_t
scribbu::id3v2_4_tag::ext_header::crc() const
{
  if (crc_dirty_ || !has_crc()) {
    throw invalid_ext_header();
  }
  return crc_;
}

std::uint32_t
scribbu::id3v2_4_tag::ext_header::crc(const id3v2_4_tag &tag) const
{
  using namespace std;
  if (crc_dirty_) {
    crc_ = accumulate(tag.begin(), tag.end(), crc32(0L, Z_NULL, 0),
                      [](uint32_t checksum, const id3v2_4_frame &F)
                      { return F.crc(checksum); });
    size_t cbpad = tag.padding();
    while (cbpad) {
      size_t toadd = min(cbpad, CBPAD);
      crc_ = crc32(crc_, (const unsigned char*)PAD, toadd);
      cbpad -= toadd;
    }
    crc_dirty_ = false;
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

std::size_t
scribbu::id3v2_4_tag::ext_header::write(std::ostream &os,
                                        const id3v2_4_tag &tag) const
{
  using namespace std;

  using scribbu::detail::sync_safe_from_unsigned;

  size_t cb = 0;

  unsigned char buf[6];
  sync_safe_from_unsigned(size(), buf);

  buf[4] = 0x01;

  unsigned char flags = 0x0;
  if (is_update()) {
    flags |= 64;
  }
  if (has_crc()) {
    flags |= 32;
  }
  if (restricted()) {
    flags |= 16;
  }

  buf[5] = flags;

  os.write((char*)buf, 6);
  cb += 6;

  // Move on to flag data
  if (is_update()) {
    char zed = 0;
    os.write(&zed, 1);
    cb += 1;
  }

  if (has_crc()) {
    sync_safe_from_unsigned(crc(tag), buf);
    os.write((char*)buf, 5);
    cb += 5;
  }

  if (restricted()) {

    tag_size ts;
    bool te;
    text_size sz;
    bool ie;
    image_size is;
    tie(ts, te, sz, ie, is) = get_restrictions();

    unsigned char tagr = 0x00;

    switch (ts) {
    case tag_size::restricted:
      // -Wswitch
      break;
    case tag_size::more_restricted:
      tagr |= (0x1 << 6);
      break;
    case tag_size::very_restricted:
      tagr |= (0x2 << 6);
      break;
    case tag_size::extremely_restricted:
      tagr |= (0x3 << 6);
      break;
    }

    if (te) {
      tagr |= (1 << 5);
    }

    switch (sz) {
    case text_size::unrestricted:
      // -Wswitch
      break;
    case text_size::restricted:
      tagr |= (1 << 3);
      break;
    case text_size::more_restricted:
      tagr |= (2 << 3);
      break;
    case text_size::very_restricted:
      tagr |= (3 << 3);
      break;
    }

    if (ie) {
      tagr |= (1 << 2);
    }

    switch (is) {
    case image_size::unrestricted:
      // -Wswitch
      break;
    case image_size::restricted:
      tagr |= 0x1;
      break;
    case image_size::more_restricted:
      tagr |= 0x2;
      break;
    case image_size::very_restricted:
      tagr |= 0x3;
      break;
    }

    os.write((char*)&tagr, 1);
    cb += 1;
  }

  return cb;
}

scribbu::id3v2_4_tag::id3v2_4_tag(std::istream &is): id3v2_tag(is), padding_(0)

{
  get_default_generic_frame_parsers(std::inserter(
    generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(
    text_parsers_, text_parsers_.begin()));

  // id3v2_tag has consumed the first five bytes of the header-- this call will
  // consume the next five...
  std::size_t size;
  unsigned char flags;
  std::tie(flags, size) = parse_flags_and_size(is);

  unsynchronised(0 != (flags & 0x80));
  experimental_ = flags & 0x20;
  footer_ = flags & 0x10;
  parse(is, size, flags & 0x40);
}

scribbu::id3v2_4_tag::id3v2_4_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  experimental_(H.flags_ & 0x20),
  footer_(H.flags_ & 0x10),
  padding_(0)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                                  text_parsers_.begin()));
  parse(is, H.size_, H.flags_ & 0x40);
}

scribbu::id3v2_4_tag::id3v2_4_tag(std::size_t cbpad /*= 0*/, 
                                  bool fexp /*= false*/):
  id3v2_tag(4, 0),
  experimental_(fexp),
  footer_(false),
  padding_(cbpad)
{ 
  get_default_generic_frame_parsers(
    std::inserter(generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(
    std::inserter(text_parsers_, text_parsers_.begin()));
}

scribbu::id3v2_4_tag::id3v2_4_tag(const id3v2_4_tag &that):
  id3v2_tag(*this),
  experimental_(that.experimental_),
  footer_(that.footer_),
  generic_parsers_(that.generic_parsers_),
  text_parsers_(that.text_parsers_),
  pext_header_(new ext_header(*that.pext_header_)),
  padding_(that.padding_)
{
  for (auto &p: that.frames_) {
    frames_.push_back(std::unique_ptr<id3v2_4_frame>(p->clone()));
    add_frame_to_lookups( *frames_.back(), frames_.size() );
    if (frames_.back()->id() == frame_id4("ENCR")) {
      register_encryption_method(dynamic_cast<const ENCR_2_4&>(*frames_.back()));
    }
  }
}

scribbu::id3v2_4_tag& scribbu::id3v2_4_tag::operator=(const id3v2_4_tag &that)
{
  if (this != &that) {
    id3v2_tag::operator=(*this);
    experimental_ = that.experimental_;
    footer_ = that.footer_;
    generic_parsers_ = that.generic_parsers_;
    text_parsers_ = that.text_parsers_;
    pext_header_.reset(new ext_header(*that.pext_header_));
    padding_ = that.padding_;

    comms_.clear();
    pcnts_.clear();
    popms_.clear();
    encryption_methods_.clear();
    frames_.clear();
    frame_map_.clear();
    text_map_.clear();

    for (auto &p: that.frames_) {
      frames_.push_back(std::unique_ptr<id3v2_4_frame>(p->clone()));
      add_frame_to_lookups( *frames_.back(), frames_.size() );
      if (frames_.back()->id() == frame_id4("ENCR")) {
        register_encryption_method(dynamic_cast<const ENCR_2_4&>(*frames_.back()));
      }
    }
  }
  
  return *this;
}

/*virtual*/ unsigned char
scribbu::id3v2_4_tag::flags() const
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
  if (footer_) {
    flags |= 0x10;
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
 * \param unync [in] The caller shall set this to true to apply
 * unsynchronisation to \em all frames when computing the tag size
 *
 * \return Size Serialized size of this tag, in bytes, exclusive of the ID3v2
 * header (i.e. the value that could be written in bytes six through nine of
 * the tag)
 *
 *
 * Computing the tag size is simpler in ID3v2.4 than ID3v2.3. The header,
 * extended header, padding, & footer are all sync-safe, so unsynchronisation
 * is applied on a frame-by-frame basis.
 *
 * This implementation proceeds as follows:
 *
 * - the ID3v2 header is always ten bytes, regardless of whether unsynch is being applied
 *
 * - the extended header is synch-safe by design, so its size can be computed
 *   regardless of whether unsynch is being applied (the size will vary based
 *   on which flags are set)
 *
 * - ask each frame to compute its size; if \a unsync is true, apply
 *   unsynchronisation unconditionally, otherwise the frame will decide whether
 *   or not to apply it
 *
 * - padding & footer are synch-safe, so their size can be computed regardless
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_4_tag::size(bool unsync) const
{
  using namespace std;

  size_t cb = padding();
  if (has_footer()) {
    cb += 10;
  }
  if (pext_header_) {
    cb += pext_header_->size();
  }
  return accumulate(begin(), end(), cb,
                   [unsync](size_t n, const id3v2_4_frame &f)
                   { return n + f.serialized_size(unsync); });
}

/**
 * \brief Return true if all frames would contain false syncs if serialized in
 * their present state
 *
 *
 * The meaning of this method changes with ID3v2.4. Now, the unsynchronisation
 * flag means that the unsynchronisation scheme is applied to \em all frames;
 * consequently, this method will return true if and only if all frames need
 * it.
 *
 *
 */

/*virtual*/
bool
scribbu::id3v2_4_tag::needs_unsynchronisation() const
{

  using namespace std;
  return all_of(begin(), end(), [](const id3v2_4_frame &f) { return f.needs_unsynchronisation(); });
}

/**
 * \brief Serialize this tag to an output stream, perhaps applying the
 * unsynchronisation scheme if the caller so chooses
 *
 *
 * \param os [in] std output stream to which this tag shall be written
 *
 * \param unsync [in] the caller shall set this true to apply unsynchronisation while writing
 *
 *
 * Here, unsynch means "apply to all frames"-- not set means frames will choose
 * for themselves whether to apply the unsynchronisation scheme.
 *
 * This implementation proceeds as follows:
 *
 * - write the ID3v2 header
 *
 *   + call size() with the \a unsync to get the tag size
 *
 *   + the header is synch-safe, so no need to unsynchronise further
 *
 * - write the ID3v2.4 extended header, if present
 *
 *   + the ID3v2.4 extended header is synch-safe
 *
 *   + the CRC checksum here is calculated on frames *and* padding
 *   (i.e. everything between the extended header & the footer, if present)
 *
 *     * the spec says "The CRC is calculated on all the data between the header
 *       and footer..."-- I'm not sure how to interpret that, but it doesn't
 *       make much sense to me *other* than in the same way as ID3v2.3-- the
 *       checksum should be carried out after all the frames have been
 *       re-synchronised ; this means asking frames to serialize themselves
 *       \em without unsynch
 *
 * - write the frames
 *
 *   + if unsynch is requested, we must force unsynch on each frame
 *
 *   + else, each frame should decide for itself
 *
 * - write the padding
 *
 * - write the footer, if present
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_4_tag::write(std::ostream &os, bool unsync) const
{
  using namespace std;

  // Write the header...
  write_header(os, flags(), size(unsync));
  size_t cb = 10;

  if (pext_header_) {
    cb += pext_header_->write(os, *this);
  }

  cb = accumulate(begin(), end(), cb,
                  [&os, unsync](size_t n, const id3v2_4_frame &f)
                  { return n + f.write(os, unsync); });

  size_t cbpad = padding();
  while (cbpad) {
    size_t towrite = min(cbpad, CBPAD);
    os.write(PAD, towrite);
    cb += towrite;
    cbpad -= towrite;
  }

  if (has_footer()) {
    cb += 10;
    write_footer(os, flags(), size(unsync));
  }

  return cb;
}

/*virtual*/ std::size_t
scribbu::id3v2_4_tag::play_count() const {
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
scribbu::id3v2_4_tag::add_comment(const std::string &text,
                                  language lang /*= language::from_locale*/,
                                  encoding src /*= encoding::UTF_8*/,
                                  use_unicode unicode /*= use_unicode::no*/,
                                  const std::string &description /*= std::string()*/,
                                  on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::unique_ptr<COMM_2_4> pnew = std::make_unique<COMM_2_4>(lang, text, src, unicode,
                                                              tag_alter_preservation::preserve,
                                                              file_alter_preservation::preserve,
                                                              read_only::clear,
                                                              boost::none,
                                                              boost::none,
                                                              false, false,
                                                              boost::none,
                                                              description);
  std::ptrdiff_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(*pnew, d);
}

/*virtual*/
void
scribbu::id3v2_4_tag::add_user_defined_text(const std::string &text,
                                            encoding src /*= encoding::UTF_8*/,
                                            use_unicode unicode /*= use_unicode::no*/,
                                            const std::string &description /*= std::string()*/,
                                            on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::unique_ptr<TXXX_2_4> pnew = std::make_unique<TXXX_2_4>(text, src, unicode,
                                                              tag_alter_preservation::preserve,
                                                              file_alter_preservation::preserve,
                                                              read_only::clear,
                                                              boost::none,
                                                              boost::none,
                                                              false, false,
                                                              description);
  std::ptrdiff_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(*pnew, d);
}

/*virtual*/
std::string
scribbu::id3v2_4_tag::text(
    id3v2_text_frames                id,
    encoding                         dst /*= encoding::UTF_8*/,
    on_no_encoding                   rsp /*= on_no_encoding::fail*/,
    const boost::optional<encoding> &src /*= boost::none*/) const
{
  return text_frame_as_str(frame_id4(id), dst, rsp, src);
}

/// Set the contents of an arbitrary text frame
/*virtual*/ 
void
scribbu::id3v2_4_tag::text(
    id3v2_text_frames  id,
    const std::string &text,
    encoding           src     /*= encoding::UTF_8*/,
    bool               add_bom /*= false*/,
    on_no_encoding     rsp     /*= on_no_encoding::fail*/)
{
  set_text_frame(frame_id4(id), text, src, DST, add_bom, rsp);
}

/// Delete an arbitrary text frame
/*virtual*/
void
scribbu::id3v2_4_tag::delete_frame(id3v2_text_frames id)
{
  frame_id4 id3(id);
  auto p = frame_map_.find(id3);
  if (p != frame_map_.end()) {
    std::ptrdiff_t idx = p->second;
    remove_frame_from_lookups(id3, idx);
  }
}

///////////////////////////////////////////////////////////////////////////
//                           tag as container                            //
///////////////////////////////////////////////////////////////////////////

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(mutable_frame_proxy &&that)
{
  using namespace scribbu;

  static const frame_id4 COMID("COMM"), CNTID("PCNT"), POPID("POPM");

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(that.p_->frames_[that.idx_]);
  idx_ = that.idx_;

  id3v2_4_tag::frames_type::iterator pnew = p_->frames_.begin() + idx_;
  frame_id4 id = (**pnew).id();
  if (id == CNTID) {
    PCNT_2_4 &frame = dynamic_cast<PCNT_2_4&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == COMID) {
    COMM_2_4 &frame = dynamic_cast<COMM_2_4&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == POPID) {
    POPM_2_4 &frame = dynamic_cast<POPM_2_4&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id.text_frame()) {
    id3v2_4_text_frame &frame = dynamic_cast<id3v2_4_text_frame&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else {
    id3v2_4_frame &frame = *(pnew->get());
    p_->add_frame_to_lookups(frame, idx_);
  }

  return *this;
}

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(const id3v2_4_frame &frame)
{
  std::unique_ptr<id3v2_4_frame> pnew(frame.clone());

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(pnew);
  p_->add_frame_to_lookups(*(p_->frames_[idx_]), idx_);

  return *this;
}

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(const id3v2_4_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<id3v2_4_text_frame> pnew(new id3v2_4_text_frame(frame));
  // and saving a reference to that copy.
  id3v2_4_text_frame &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(const PCNT_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<PCNT_2_4> pnew(new PCNT_2_4(frame));
  // and saving a reference to that copy.
  PCNT_2_4 &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(const COMM_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<COMM_2_4> pnew(new COMM_2_4(frame));
  // and saving a reference to that copy.
  COMM_2_4 &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_4_tag::mutable_frame_proxy&
scribbu::id3v2_4_tag::mutable_frame_proxy::operator=(const POPM_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<POPM_2_4> pnew(new POPM_2_4(frame));
  // and saving a reference to that copy.
  POPM_2_4 &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::insert(const_iterator p, const id3v2_4_frame &frame)
{
  std::unique_ptr<id3v2_4_frame> pnew(frame.clone());

  auto p1 = frames_.emplace(frames_.cbegin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(*frames_[d], d);
  return iterator(this, p1);
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::insert(const_iterator p, const id3v2_4_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<id3v2_4_text_frame> pnew = std::make_unique<id3v2_4_text_frame>(frame);
  id3v2_4_text_frame &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::insert(const_iterator p, const PCNT_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<PCNT_2_4> pnew = std::make_unique<PCNT_2_4>(frame);
  PCNT_2_4 &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::insert(const_iterator p, const COMM_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<COMM_2_4> pnew = std::make_unique<COMM_2_4>(frame);
  COMM_2_4 &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::insert(const_iterator p, const POPM_2_4 &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<POPM_2_4> pnew = std::make_unique<POPM_2_4>(frame);
  POPM_2_4 &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

void
scribbu::id3v2_4_tag::push_back(const id3v2_4_frame &frame)
{
  std::unique_ptr<id3v2_4_frame> pnew(frame.clone());

  frames_.emplace_back(std::move(pnew));
  std::size_t d = frames_.size() - 1;
  add_frame_to_lookups(*frames_[d], d);
}

void
scribbu::id3v2_4_tag::push_back(const id3v2_4_text_frame &frame)
{
  using namespace std;
  auto pnew = make_unique<id3v2_4_text_frame>(frame);
  id3v2_4_text_frame &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_4_tag::push_back(const PCNT_2_4 &frame)
{
  using namespace std;
  auto pnew = make_unique<PCNT_2_4>(frame);
  PCNT_2_4 &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_4_tag::push_back(const COMM_2_4 &frame)
{
  using namespace std;
  auto pnew = make_unique<COMM_2_4>(frame);
  COMM_2_4 &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_4_tag::push_back(const POPM_2_4 &frame)
{
  using namespace std;
  auto pnew = make_unique<POPM_2_4>(frame);
  POPM_2_4 &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::erase(const_iterator p)
{
  std::size_t idx = p - begin();
  remove_frame_from_lookups(p->id(), idx);
  return iterator(this, frames_.erase(frames_.begin() + idx));
}

scribbu::id3v2_4_tag::iterator
scribbu::id3v2_4_tag::erase(const_iterator p0, const_iterator p1)
{
  const_iterator p2 = p0;
  for (size_t i = p2 - begin(); p2 != p1; ++p2, ++i) {
    remove_frame_from_lookups(p2->id(), i);
  }

  return iterator(this, frames_.erase(frames_.begin() + p0.index(),
                                      frames_.begin() + p1.index()));
}

std::ostream&
scribbu::id3v2_4_tag::write_header(std::ostream &os,
                                   unsigned char flags,
                                   std::size_t cb) const
{
  unsigned char buf[] = { 'I', 'D', '3', 4, 0, flags };
  os.write((const char*)buf, sizeof(buf));
  detail::sync_safe_from_unsigned(cb, buf);
  os.write((const char*)buf, 4);
  return os;
}

std::ostream&
scribbu::id3v2_4_tag::write_footer(std::ostream &os,
                                   unsigned char flags,
                                   std::size_t cb) const
{
  unsigned char buf[] = { '3', 'D', 'I', 4, 0, flags };
  os.write((const char*)buf, sizeof(buf));
  detail::sync_safe_from_unsigned(cb, buf);
  os.write((const char*)buf, 4);
  return os;
}

void
scribbu::id3v2_4_tag::remove_frame_from_lookups(const frame_id4 &id, std::size_t idx)
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
    const id3v2_4_frame *pf = frames_[idx].get();
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
scribbu::id3v2_4_tag::add_frame_to_lookups(const id3v2_4_frame &frame, std::size_t idx)
{
  using namespace std;
  frame_map_.insert(make_pair(frame.id(), idx));
}

void
scribbu::id3v2_4_tag::add_frame_to_lookups(id3v2_4_text_frame &frame, std::size_t idx)
{
  using namespace std;
  text_map_.insert(make_pair(frame.id(), &frame));
  add_frame_to_lookups((const id3v2_4_frame&)frame, idx);
}

void
scribbu::id3v2_4_tag::add_frame_to_lookups(PCNT_2_4 &frame, std::size_t idx)
{
  using namespace std;
  pcnts_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_4_frame&)frame, idx);
}

void
scribbu::id3v2_4_tag::add_frame_to_lookups(COMM_2_4 &frame, std::size_t idx)
{
  using namespace std;
  comms_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_4_frame&)frame, idx);
}

void
scribbu::id3v2_4_tag::add_frame_to_lookups(POPM_2_4 &frame, std::size_t idx)
{
  using namespace std;
  popms_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_4_frame&)frame, idx);
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

void scribbu::id3v2_4_tag::parse(std::istream &is, std::size_t size, bool extended)
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
  std::streampos here = is.tellg();

  try {

    // Size, in bytes, of the tag *after* the common header, *before*
    // resynchronisation. If a footer is present this equals to ('total size' -
    // 20) bytes, otherwise ('total size' - 10) bytes.
    std::size_t cb_tag = size;

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
      p0 += pext_header_->size();
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

        cb_frame = unsigned_from_sync_safe(pcurr[4], pcurr[5], pcurr[6],
                                           pcurr[7]);

        if (ENCR != id) {
          continue;
        }

        if (pcurr + 11 > p1) {
          throw invalid_tag();
        }

        unsigned char f0 = p0[8];

        id3v2_4_frame::tag_alter_preservation tap = (0 != (f0 & 0x40)) ?
          id3v2_4_frame::tag_alter_preservation::discard :
          id3v2_4_frame::tag_alter_preservation::preserve;
        id3v2_4_frame::file_alter_preservation fap = (0 != (f0 & 0x20)) ?
          id3v2_4_frame::file_alter_preservation::discard :
          id3v2_4_frame::file_alter_preservation::preserve;
        id3v2_4_frame::read_only ro = (0 != (f0 & 0x10)) ?
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

      id3v2_4_frame::tag_alter_preservation tap = (0 != (f0 & 0x40)) ?
        id3v2_4_frame::tag_alter_preservation::discard :
        id3v2_4_frame::tag_alter_preservation::preserve;
      id3v2_4_frame::file_alter_preservation fap = (0 != (f0 & 0x20)) ?
        id3v2_4_frame::file_alter_preservation::discard :
        id3v2_4_frame::file_alter_preservation::preserve;
      id3v2_4_frame::read_only ro = (0 != (f0 & 0x10)) ?
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
          throw std::invalid_argument("unregistered encryption method");
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

  } catch (const scribbu::error &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw;
  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw;
  }

  is.exceptions(exc_mask);
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

  static const frame_id4 COMMID("COMM"), PCNTID("PCNT"), POPMID("POPM");

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
  else if (POPMID == id) {
    POPM_2_4 *popm = new POPM_2_4(p0, p1, tap, fap, read_only, encmth,
                                  group_id, compressed, unsynchronisation,
                                  dli);
    popms_.push_back(make_pair(popm, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_4_frame>(popm));
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

/// Replace a text frame if it exists, append it otherwise
void scribbu::id3v2_4_tag::set_text_frame(const frame_id4 &id,
                                          const std::string &text,
                                          encoding src /*= encoding::UTF_8*/,
                                          id3v2_4_text_frame::frame_encoding dst /*=id3v2_4_text_frame::frame_encoding::UTF_8*/,
                                          bool add_bom /*= false*/,
                                          on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  using namespace std;
  auto p = text_map_.find(id);
  if (p != text_map_.end()) {
    p->second->set(text, src, dst, add_bom, rsp);
  }
  else {
    auto p = make_unique<id3v2_4_text_frame>(id, text, src, dst, add_bom, rsp);
    id3v2_4_text_frame &F = *p;
    frames_.push_back(unique_ptr<id3v2_4_text_frame>(move(p)));
    add_frame_to_lookups(F, frames_.size() - 1);
  }
}
