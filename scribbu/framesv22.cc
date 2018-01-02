#include <scribbu/framesv22.hh>

#include <scribbu/charsets.hh>


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_2_frame                            //
///////////////////////////////////////////////////////////////////////////////

namespace scribbu {
  
  template <>
  /*static*/
  std::string id3v2_2_frame::as_str(
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

/// Return the number of bytes this frame will occupy when serialized to
/// disk, including the header
/*virtual*/
std::size_t
scribbu::id3v2_2_frame::serialized_size(bool unsync) const
{
  ensure_cached_data_is_fresh();
  return cache_[unsync ? SERIALIZED : SERIALIZED_WITH_UNSYNC].size();
}

/// Return zero if this tag would not contain false syncs if serialized in
/// its present state; else return the number of false sync it would
/// contain
/*virtual*/
std::size_t
scribbu::id3v2_2_frame::needs_unsynchronisation() const
{
  ensure_cached_data_is_fresh();
  return num_false_syncs_;
}

/// Serialize this tag to an output stream, perhaps applying the
/// unsynchronisation scheme if the caller so chooses ("unsynchronised"
/// will be updated accordingly)
/*virtual*/
std::size_t
scribbu::id3v2_2_frame::write(std::ostream &os, bool unsync) const
{
  ensure_cached_data_is_fresh();
  unsigned char idx = unsync ? SERIALIZED : SERIALIZED_WITH_UNSYNC;
  os.write((char*)&cache_[idx][0], cache_[idx].size());
  return cache_[idx].size();
}

/// Return the number of bytes the header will occupy when serialized to
/// disk, including the header
std::size_t
scribbu::id3v2_2_frame::serialized_header_size(bool unsync) const
{
  std::size_t cb = 6;

  if (unsync) cb += header_needs_unsynchronisation();

  return cb;
  
} // End id3v2_2_frame::header_size.

/// Return zero if this tag's header would not contain false syncs if
/// serialized in its present state; else return the number of false sync it
/// would contain
std::size_t
scribbu::id3v2_2_frame::header_needs_unsynchronisation() const
{
  std::size_t num_ffs = 0;

  char buf[3]; id().copy(buf);
  if (255 == buf[0]) num_ffs++;
  if (255 == buf[1]) num_ffs++;
  if (255 == buf[2]) num_ffs++;

  std::size_t cb = size();
  buf[0] = (cb & 0xff0000) >> 16;
  buf[1] = (cb & 0x00ff00) >>  8;
  buf[2] =  cb & 0x0000ff;
  if (255 == buf[0]) num_ffs++;
  if (255 == buf[1]) num_ffs++;
  if (255 == buf[2]) num_ffs++;

  return num_ffs;
  
} // End id3v2_2_frame::header_needs_unsynchronisation.

std::size_t
scribbu::id3v2_2_frame::write_header(std::ostream &os,
                                     std::size_t cb_payload) const
{
  char idbuf[3]; id().copy(idbuf);

  char szbuf[3];

  // ID3v2.2 frame sizes are not sync-safe: We have 24 bits with
  // which to represent the size, meaning we can represent frame
  // sizes up to 0xffffff (inclusive).

  const std::size_t MAX_FRAME_SIZE = 0xffffff;
  if (cb_payload > MAX_FRAME_SIZE) {
    // TODO(sp1ff): Custom exception here!
    throw std::logic_error("Invalid ID3v2.2 frame size");
  }
  
  szbuf[0] = (cb_payload & 0xff0000) >> 16;
  szbuf[1] = (cb_payload & 0x00ff00) >>  8;
  szbuf[2] =  cb_payload & 0x0000ff;

  os.write(idbuf, 3);
  os.write(szbuf, 3);

  return 6;

} // End id3v2_2_frame::write_header.

/*virtual*/
void
scribbu::id3v2_2_frame::dirty(bool f) const
{
  if (f) {
    cache_[SERIALIZED            ].clear();
    cache_[SERIALIZED_WITH_UNSYNC].clear();
  }
  id3v2_frame::dirty(f);
}

/// Refresh the cache, if dirty (otherwise, do nothing)
void
scribbu::id3v2_2_frame::ensure_cached_data_is_fresh() const
{
  using namespace std;
  using scribbu::detail::count_false_syncs;
  using scribbu::detail::unsynchronise;

  if (is_dirty()) {

    // Clear the cache...
    cache_[SERIALIZED].clear();
    cache_[SERIALIZED_WITH_UNSYNC].clear();

    stringstream os;
    serialize(os);
    string payload = os.str();

    stringstream hdr;
    write_header(hdr, payload.size());
    string buf = hdr.str();

    // Accumulate the entire thing in `cache_'.
    cache_[SERIALIZED].resize(buf.size() + payload.size());
    auto pout = copy(buf.begin(), buf.end(), cache_[SERIALIZED].begin());
    copy(payload.begin(), payload.end(), pout);

    // Either way, count the # of false syncs in that buffer...
    num_false_syncs_ = count_false_syncs(cache_[SERIALIZED].begin(),
                                         cache_[SERIALIZED].end());

    // and prepare an unsynchronised copy.
    unsynchronise(back_inserter(cache_[SERIALIZED_WITH_UNSYNC]),
                  cache_[SERIALIZED].begin(),
                  cache_[SERIALIZED].end());

    dirty(false);
  }
}

/// Write a three-tuple while removing false syncs
std::size_t
scribbu::id3v2_2_frame::unsynchronise_triplet(std::ostream &os, char buf[3]) const
{
  static const char zed = 0;

  std::size_t num_ffs = 0;
  if (255 == buf[0]) num_ffs++;
  if (255 == buf[1]) num_ffs++;
  if (255 == buf[2]) num_ffs++;

  if (num_ffs) {
    os.write(buf, 1);
    if (255 == buf[0]) os.write(&zed, 1);
    os.write(buf + 1, 1);
    if (255 == buf[1]) os.write(&zed, 1);
    os.write(buf + 2, 1);
    if (255 == buf[2]) os.write(&zed, 1);
  }
  else {
    os.write(buf, 3);
  }
  
  return num_ffs;
}


///////////////////////////////////////////////////////////////////////////////
//                           unknown_id3v2_2_frame                           //
///////////////////////////////////////////////////////////////////////////////

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::unknown_id3v2_2_frame::size() const
{
  return data_.size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::unknown_id3v2_2_frame::serialize(std::ostream &os) const
{
  os.write((char*)&(data_[0]), data_.size());
  return data_.size();
}


///////////////////////////////////////////////////////////////////////////////
//                                 class UFI                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::UFI::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new UFI(p, p + cb) );
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::UFI::size() const
{
  // TODO(sp1ff): Implement unique_file_id::size()
  return unique_file_id::size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::UFI::serialize(std::ostream &os) const
{
  return unique_file_id::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                          class id3v2_text_frame                           //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
std::size_t
scribbu::id3v2_2_text_frame::size() const
{
  return 1 + text_.size();
}

void
scribbu::id3v2_2_text_frame::set(const std::string &text,
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

/*static*/ std::unique_ptr<scribbu::id3v2_2_text_frame>
scribbu::id3v2_2_text_frame::create(const frame_id3& id,
                                    const unsigned char *p,
                                    std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_text_frame>(
    new id3v2_2_text_frame(id, p, p + cb) );
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::id3v2_2_text_frame::serialize(std::ostream &os) const
{
  os.write((char*)&unicode_, 1);
  os.write((char*)&(text_[0]), text_.size());
  return 1 + text_.size();
}


///////////////////////////////////////////////////////////////////////////////
//                                 class TXX                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::TXX::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new TXX(p, p + cb) );
}

/*virtual*/
std::size_t
scribbu::TXX::size() const
{
  // TODO(sp1ff): Implement user_defined_text::size()
  return user_defined_text::size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::TXX::serialize(std::ostream &os) const
{
  return user_defined_text::write(os); 
}


///////////////////////////////////////////////////////////////////////////////
//                                 class COM                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::COM::create(const frame_id3& id,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new COM(p, p + cb) );
}

/*virtual*/
std::size_t
scribbu::COM::size() const
{
  // TODO(sp1ff): Implement comments::size()
  return comments::size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::COM::serialize(std::ostream &os) const
{
  return comments::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                 class CNT                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::CNT::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new CNT(p, p + cb) );
}

/*virtual*/
std::size_t
scribbu::CNT::size() const
{
  // TODO(sp1ff): Implement play_count::size()
  return play_count::size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::CNT::serialize(std::ostream &os) const
{
 return play_count::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                                 class POP                                 //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::POP::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new POP(p, p + cb) );
}

/*virtual*/
std::size_t
scribbu::POP::size() const
{
  // TODO(sp1ff): Implement popularimeter::size()
  return popularimeter::size();
}

/// Serialize this frame to \a os, exclusive of any compression, encryption
/// or unsynchronisation; return the number of bytes written
/*virtual*/
std::size_t
scribbu::POP::serialize(std::ostream &os) const
{
  return popularimeter::write(os);
}

