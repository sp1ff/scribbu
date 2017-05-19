#include <scribbu/framesv23.hh>

#include <scribbu/charsets.hh>


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_2_frame                            //
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
        // TODO: Does this ever actually happen?
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
//                     miscellaneous creation functions                      //
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
