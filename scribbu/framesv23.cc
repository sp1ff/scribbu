#include <scribbu/framesv23.hh>

#include <scribbu/charsets.hh>
#include <scribbu/id3v2.hh>

#include <zlib.h>

///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_3_plus_frame                          //
///////////////////////////////////////////////////////////////////////////////

/**
 * \brief Update a CRC-32 checksum using this frames serialized form
 *
 *
 * \param crc [in] current value of the checksum
 *
 * \return updated value of the checksum with this frame's serialized 
 * representation added
 *
 *
 * ID3v2.3 & v2.4 differ on the content that goes into the CRC checksum, but
 * include frames in both cases. The standards are not explicit on this point,
 * but the intent seems to be error detection between writer &
 * reader. Therefore, while it will have to be computed without the
 * unsynchronisation scheme (since that's always the last transformation on
 * wirte), it is presumably meant to be computed after compression &
 * encryption.
 *
 *
 */

std::uint32_t
scribbu::id3v2_3_plus_frame::crc(std::uint32_t crc) const
{
  ensure_cached_data_is_fresh();
  return crc32(crc, &cache_[SERIALIZED_WITH_CE][0], cache_[SERIALIZED_WITH_CE].size());
}

/**
 * \brief Return the number of bytes this frame will occupy when serialized to
 * disk, including the header
 *
 *
 * \param unsync [in] The caller shall set this to true to request the serialized
 * size of this frame after the unsynchronisation scheme has been applied, and false
 * to request it without the unsynchronisation scheme
 *
 * \return The number of bytes this frame will occupy when serialized,
 * including the ehader (i.e. the size of the required buffer)
 *
 *
 * \todo Override separately for ID3v2.4-- should respect the unsync flag.
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_3_plus_frame::serialized_size(bool unsync) const
{
  ensure_cached_data_is_fresh();
  return cache_[unsync ? SERIALIZED_WITH_CEU : SERIALIZED_WITH_CE].size();
}

/**
 * \brief Return zero if this tag would not contain false syncs if serialized
 * in its present state; else return the number of false sync it would contain
 *
 * \return The number of false syncs, after compressiona and/or encryption are
 * applied
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_3_plus_frame::needs_unsynchronisation() const
{
  ensure_cached_data_is_fresh();
  return num_false_syncs_;
}

/**
 * \brief Serialize this tag to an output stream, perhaps applying the
 * unsynchronisation scheme if the caller so chooses ("unsynchronised" will be
 * updated accordingly)
 *
 *
 * \param os [in] Standard ostream to which this frame shall be written
 *
 * \param unsync [in] The caller shall set this to be true to request that the
 * unsynchronisation scheme be applied, and false to request that it not.
 *
 * \return The number of bytes written to \a os
 *
 *
 * \todo Override separately for ID3v2.4-- should respect the unsync flag.
 *
 *
 */

/*virtual*/
std::size_t
scribbu::id3v2_3_plus_frame::write(std::ostream &os, bool unsync) const
{
  ensure_cached_data_is_fresh();
  unsigned char idx = unsync ? SERIALIZED_WITH_CEU : SERIALIZED_WITH_CE;
  os.write((char*)&cache_[idx][0], cache_[idx].size());
  return cache_[idx].size();
}

/*virtual*/
void
scribbu::id3v2_3_plus_frame::dirty(bool f) const
{
  if (f) {
    cache_[SERIALIZED_WITH_CE ].clear();
    cache_[SERIALIZED_WITH_CEU].clear();
  }
  id3v2_frame::dirty(f);
}

/**
 * \brief If the dirty flag is set, re-serialize & re-apply compression,
 * encryption & unsynchronisation
 *
 *
 */

void
scribbu::id3v2_3_plus_frame::ensure_cached_data_is_fresh() const
{
  using namespace std;
  using scribbu::detail::count_false_syncs;
  using scribbu::detail::unsynchronise;

  // This logic is still too complex for my tastes, but this is as simple
  // as I've been able to get it so far. If the 'dirty" flag is set...
  if (is_dirty()) {

    // clear our cache & re-build it.
    cache_[SERIALIZED_WITH_CE ].clear();
    cache_[SERIALIZED_WITH_CEU].clear();

    // I like the convenient interface of an ostream, so let's build
    // one backed by an in-memory streambuf; i.e. a stringstream...
    stringstream os;
    // and present that to our subclasses:
    serialize(os);
    // in this method, however, we'll be working with the underlying
    // string:
    string buf = os.str();

    // At this point, we have this frame's payload in serialized form, prior to
    // the application of any compression, encryption or unsynchronisation.
    size_t cb_no_ceu = buf.size();

    // We *would* next apply compression and/or encryption, except that we
    // don't at this point, support encryption (I would need to find some
    // actual examples, first).
    if (encrypted()) {
      throw std::logic_error("encryption not implemented");
    }

    // Instead, if this frame is compressed...
    vector<unsigned char> payload;
    if (compressed()) {

      // compress `buf' into `payload'...
      unsigned long destLen = compressBound(cb_no_ceu);
      payload.resize(destLen);
      int status =  compress(&payload[0], &destLen,
                             (unsigned char*)&buf[0], cb_no_ceu);
      if (Z_OK != status) {
        throw zlib_error(status);
      }
      payload.resize(destLen);

    }
    else {

      // else, just copy it.
      payload.resize(cb_no_ceu);
      copy(buf.begin(), buf.end(), payload.begin());

    }

    // In the case of ID3v2.4, unsynchorinisation is applied on a frame-by-frame
    // basis, so we would need to check the releveant flag, and if it's set,
    // unsynchronise the payload.

    // What I'm doing instead is assembling the entire frame payload in
    // `cache-', & then applying unsynchronisation to that.  In the case of
    // ID3v2.4, the frame header is sync-safe to begin with, so the data in the
    // SERIALIZED_WITH_CEU will be exactly what we would have gotten by writing
    // the header & then unsynchronising the payload.

    // Serialize the frame header...
    stringstream hdr;
    write_header(hdr, payload.size(), cb_no_ceu);
    buf = hdr.str();

    // and accumulate the entire thing in `cache_'.
    cache_[SERIALIZED_WITH_CE].resize(buf.size() + payload.size());
    auto pout = copy(buf.begin(), buf.end(), cache_[SERIALIZED_WITH_CE].begin());
    copy(payload.begin(), payload.end(), pout);

    // Either way, count the # of false syncs in that buffer...
    num_false_syncs_ = count_false_syncs(cache_[SERIALIZED_WITH_CE].begin(),
                                         cache_[SERIALIZED_WITH_CE].end());

    // and prepare an unsynchronised copy.
    unsynchronise(back_inserter(cache_[SERIALIZED_WITH_CEU]),
                  cache_[SERIALIZED_WITH_CE].begin(),
                  cache_[SERIALIZED_WITH_CE].end());

    dirty(false);
  }
  
}


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_3_frame                            //
///////////////////////////////////////////////////////////////////////////////

namespace scribbu {
  
  template <>
  /*static*/
  std::string id3v2_3_frame::as_str(
    const unsigned char *pbuf,
    std::size_t cbbuf,
    unsigned char unicode,
    scribbu::encoding dstenc,
    scribbu::on_no_encoding rsp /*= 
      scribbu::on_no_encoding::fail*/,
    const boost::optional<scribbu::encoding> &force /*=
      boost::none*/)
  {
    using std::string;
    using scribbu::encoding;
    using scribbu::convert_encoding;

    encoding src = encoding::ISO_8859_1;
    if (force) {
      src = force.get();
    }
    else if (unicode) {

      if (1 != unicode) {
        throw std::range_error("encoding should be zero or one");
      }
      
      if (1 < cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
        src =  encoding::UCS_2BE;
      } else if (1 < cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
        src =  encoding::UCS_2LE;
      } else {
        src =  encoding::UCS_2;
      }

    }

    return convert_encoding<string>(pbuf, cbbuf, src, dstenc, rsp);
  }

}

/// Serialize this tag's header to an output stream, including any special
/// fields such as decompressed size, group id, &c
/*virtual*/
std::size_t
scribbu::id3v2_3_frame::write_header(std::ostream &os,
                                     std::size_t cb_payload,
                                     std::size_t dlind) const
{
  std::size_t cb = 10, sz = cb_payload;

  char flags[2] = { 0, 0 };
  if (tap()) {
    flags[0] |= 64;
  }
  if (fap()) {
    flags[0] |= 32;
  }
  if (ro()) {
    flags[0] |= 16;
  }

  if (compressed()) {
    flags[1] |= 64;
    sz += 4;
    cb += 4;
  }
  if (encrypted()) {
    flags[1] |= 32;
    sz += 1;
    cb += 1;
  }
  if (grouped()) {
    flags[1] |= 16;
    sz += 1;
    cb += 1;
  }

  char idbuf[4]; id().copy(idbuf);

  // ID3v2.3 frame sizes are not sync-safe:
  char szbuf[4];
  szbuf[0] = (sz & 0xff000000) >> 24;
  szbuf[1] = (sz & 0x00ff0000) >> 16;
  szbuf[2] = (sz & 0x0000ff00) >>  8;
  szbuf[3] =  sz & 0x000000ff;

  os.write(idbuf, 4);
  os.write(szbuf, 4);
  os.write(flags, 2);

  if (compressed()) {
    os.write((char*)&dlind, 4);
  }

  if (encrypted()) {
    unsigned char x = encmeth();
    os.write((char*)&x, 1);
  }

  if (grouped()) {
    unsigned char x = gid();
    os.write((char*)&x, 1);
  }
  
  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                        class unknown_id3v2_3_frame                        //
///////////////////////////////////////////////////////////////////////////////

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::unknown_id3v2_3_frame::size() const
{
  return data_.size();
}

/*virtual*/
std::size_t
scribbu::unknown_id3v2_3_frame::serialize(std::ostream &os) const
{
  os.write((char*)&(data_[0]), data_.size());
  return data_.size();
}


///////////////////////////////////////////////////////////////////////////////
//                                class UFID                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::UFID::create(const frame_id4 &/*id*/,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new UFID(p, p + cb, tap, fap,
                                                 read_only, enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::UFID::size() const
{
  return unique_file_id::size();
}

/*virtual*/
std::size_t
scribbu::UFID::serialize(std::ostream &os) const
{
  return unique_file_id::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                class ENCR                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::ENCR::create(const frame_id4 &/*id*/,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new ENCR(p, p + cb, tap, fap,
                                                 read_only, enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::ENCR::size() const
{
  return encryption_method::size();
}

/*virtual*/
std::size_t
scribbu::ENCR::serialize(std::ostream &os) const
{
  return encryption_method::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_3_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::id3v2_3_text_frame::size() const
{
  return 1 + text_.size();
}

/*virtual*/
std::size_t
scribbu::id3v2_3_text_frame::serialize(std::ostream &os) const
{
  os.write((char*)&unicode_, 1);
  os.write((char*)&(text_[0]), text_.size());
  return 1 + text_.size();
}

void
scribbu::id3v2_3_text_frame::set(const std::string &text,
                                 encoding src /*= encoding::UTF_8*/,
                                 bool add_bom /*=false*/,
                                 on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  // Attempt to convert to ISO-8859-1 first
  bool ok;
  std::vector<unsigned char> test;
  // TODO(sp1ff): Implement a version that doesn't throw
  try {
    test = scribbu::convert_encoding(text, src, encoding::ISO_8859_1, add_bom, rsp);
    ok = true;
  }
  catch (const std::exception&) {
    ok = false;
  }

  if (ok) {
    unicode_ = 0;
    text_.swap(test);
    return;
  }

  test = convert_encoding(text, src, encoding::UCS_2, add_bom, rsp);
  unicode_ = 1;
  text_.swap(test);
}


/*static*/ std::unique_ptr<scribbu::id3v2_3_text_frame>
scribbu::id3v2_3_text_frame::create(const frame_id4 &id,
                                    const unsigned char *p,
                                    std::size_t cb,
                                    tag_alter_preservation tap,
                                    file_alter_preservation fap,
                                    read_only read_only,
                                    const boost::optional<unsigned char> &enc,
                                    const boost::optional<unsigned char> &gid,
                                    const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_text_frame>(
    new id3v2_3_text_frame(id, p, p + cb, tap, fap, read_only,
                           enc, gid, dsz));
}




///////////////////////////////////////////////////////////////////////////////
//                                class TXXX                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::TXXX::create(const frame_id4 &id,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new TXXX(p, p + cb, tap,
                                                 fap, read_only,
                                                 enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::TXXX::size() const
{
  return user_defined_text::size();
}

/*virtual*/
std::size_t
scribbu::TXXX::serialize(std::ostream &os) const
{
  return user_defined_text::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                class COMM                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::COMM::create(const frame_id4 &id,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new COMM(p, p + cb, tap,
                                                 fap, read_only,
                                                 enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::COMM::size() const
{
  return comments::size();
}

/*virtual*/
std::size_t
scribbu::COMM::serialize(std::ostream &os) const
{
  return comments::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                class PCNT                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::PCNT::create(const frame_id4 &id,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new PCNT(p, p + cb, tap,
                                                 fap, read_only,
                                                 enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::PCNT::size() const
{
  return play_count::size();
}

/*virtual*/
std::size_t
scribbu::PCNT::serialize(std::ostream &os) const
{
  return play_count::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                class POPM                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::POPM::create(const frame_id4 &id,
                      const unsigned char *p,
                      std::size_t cb,
                      tag_alter_preservation tap,
                      file_alter_preservation fap,
                      read_only read_only,
                      const boost::optional<unsigned char> &enc,
                      const boost::optional<unsigned char> &gid,
                      const boost::optional<std::size_t> &dsz)
{
  return std::unique_ptr<id3v2_3_frame>(new POPM(p, p + cb, tap,
                                                 fap, read_only,
                                                 enc, gid, dsz));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::POPM::size() const
{
  return popularimeter::size();
}

/*virtual*/
std::size_t
scribbu::POPM::serialize(std::ostream &os) const
{
  return popularimeter::write(os);
}
