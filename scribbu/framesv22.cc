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

/*static*/ std::unique_ptr<scribbu::id3v2_2_text_frame>
scribbu::id3v2_2_text_frame::create(const frame_id3& id,
                                    const unsigned char *p,
                                    std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_text_frame>(
    new id3v2_2_text_frame(id, p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::TXX::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new TXX(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::UFI::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new UFI(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::COM::create(const frame_id3& id,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new COM(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::CNT::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new CNT(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::POP::create(const frame_id3& /*id*/,
                     const unsigned char *p,
                     std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new POP(p, p + cb) );
}
