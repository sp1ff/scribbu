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

std::size_t
scribbu::id3v2_2_frame::write_header(std::ostream &os, bool unsync) const
{
  const char zed = 0;

  char buf[3]; id().copy(buf);
  os.write(buf, 3);

  // ID3v2.2 frame sizes are not sync-safe:
  std::size_t cb = size();
  buf[0] = (cb & 0xff0000) >> 16;
  buf[1] = (cb & 0x00ff00) >>  8;
  buf[2] =  cb & 0x0000ff;

  std::size_t num_ffs = 0;
  if (255 == buf[0]) num_ffs++;
  if (255 == buf[1]) num_ffs++;
  if (255 == buf[2]) num_ffs++;

  if (unsync && num_ffs) {
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
  
  return 6 + num_ffs;
}


///////////////////////////////////////////////////////////////////////////////
//                           unknown_id3v2_2_frame                           //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ std::size_t
scribbu::unknown_id3v2_2_frame::serialized_size(bool unsync) const
{
  std::size_t cb = 6 + data_.size();
  if (unsync) {
    cb += detail::count_ffs(data_.begin(), data_.end());
  }
  return cb;
}

/*virtual*/ std::size_t
scribbu::unknown_id3v2_2_frame::needs_unsynchronisation() const
{
  // TODO(sp1ff): Will probably factor this out, but...  we seek two-byte
  // sequences where the first is 255 & the second is greater than 223.  NB
  // False sync's can't occur across frame boundries-- since the first byte of
  // the frame will be an ASCII character, it will be less than 224. Even if
  // the last byte of a frame is ff, the first padding byte will be zero (if
  // there is no padding, then we can't have a false sync). Note that if the
  // last byte is ff and unsync *is* turned on, we need to write an add'l 00
  // when we serialize.
  using namespace scribbu::detail;
  return count_false_syncs(data_.begin(), data_.end());
}

/*virtual*/ std::size_t
scribbu::unknown_id3v2_2_frame::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  std::size_t cb_ffs = detail::count_ffs(data_.begin(), data_.end());
  if (unsync && cb_ffs) {
    std::size_t cb = 0;
    cb += detail::unsynchronise(os, data_.begin(), data_.end());
    return cb;
  }
  else {
    os.write((char*)&(data_[0]), data_.size());
    return 6 + data_.size();
  }
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

/*virtual*/ std::size_t
scribbu::UFI::serialized_size(bool unsync) const
{
  return 6 + unique_file_id::serialized_size(unsync);
}

/*virtual*/ std::size_t
scribbu::UFI::needs_unsynchronisation() const
{
  return unique_file_id::needs_unsynchronisation();
}

/*virtual*/ std::size_t
scribbu::UFI::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  return 6 + unique_file_id::write(os, unsync);
}


///////////////////////////////////////////////////////////////////////////////
//                          class id3v2_text_frame                           //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ std::size_t
scribbu::id3v2_2_text_frame::serialized_size(bool unsync) const
{
  std::size_t cb = 6 + 1 + text_.size();
  if (unsync) {
    cb += detail::count_ffs(text_.begin(), text_.end());
    if (255 == unicode_) ++cb;
  }
  return cb;
}

/*virtual*/ std::size_t
scribbu::id3v2_2_text_frame::needs_unsynchronisation() const
{
  using namespace scribbu::detail;
  size_t cb = is_false_sync(unicode_, text_.front()) ? 1 : 0;
  cb += count_false_syncs(text_.begin(), text_.end());
  return cb;
}

/*virtual*/ std::size_t
scribbu::id3v2_2_text_frame::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  std::size_t cb_ffs = 255 == unicode_ ? 1 : 0;
  cb_ffs += detail::count_ffs(text_.begin(), text_.end());
  if (unsync && cb_ffs) {
    std::size_t cb = 0;
    if (255 == unicode_) {
      unsigned char buf[2] = { unicode_, 0 };
      os.write((const char*)buf, 2);
      cb += 2;
    }
    else {
      os.write((const char*)&unicode_, 1);
      cb += 1;
    }
    cb += detail::unsynchronise(os, text_.begin(), text_.end());
    return cb;
  }
  else {
    os.write((char*)&unicode_, 1);
    os.write((char*)&(text_[0]), text_.size());
    return 1 + text_.size();
  }
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

/*virtual*/ std::size_t
scribbu::TXX::serialized_size(bool unsync) const
{
  return 6 + user_defined_text::serialized_size(unsync);
}

/*virtual*/ std::size_t
scribbu::TXX::needs_unsynchronisation() const
{
  return user_defined_text::needs_unsynchronisation();
}

/*virtual*/ std::size_t
scribbu::TXX::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  return 6 + user_defined_text::write(os, unsync);
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

/*virtual*/ std::size_t
scribbu::COM::serialized_size(bool unsync) const
{
  return 6 + comments::serialized_size(unsync);
}

/*virtual*/ std::size_t
scribbu::COM::needs_unsynchronisation() const
{
  return comments::needs_unsynchronisation();
}

/*virtual*/ std::size_t
scribbu::COM::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  return 6 + comments::write(os, unsync);
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

/*virtual*/ std::size_t
scribbu::CNT::serialized_size(bool unsync) const
{
  return 6 + play_count::serialized_size(unsync);
}

/*virtual*/ std::size_t
scribbu::CNT::needs_unsynchronisation() const
{
  return play_count::needs_unsynchronisation();
}

/*virtual*/ std::size_t
scribbu::CNT::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  return 6 + play_count::write(os, unsync);
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

/*virtual*/ std::size_t
scribbu::POP::serialized_size(bool unsync) const
{
  return 6 + popularimeter::serialized_size(unsync);
}

/*virtual*/ std::size_t
scribbu::POP::needs_unsynchronisation() const
{
  return popularimeter::needs_unsynchronisation();
}

/*virtual*/ std::size_t
scribbu::POP::write(std::ostream &os, bool unsync) const
{
  write_header(os, unsync);
  return 6 + popularimeter::write(os, unsync);
}
