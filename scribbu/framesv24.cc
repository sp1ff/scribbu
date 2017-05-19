#include <scribbu/framesv24.hh>

#include <scribbu/charsets.hh>


///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_4_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

namespace scribbu {

  template <>
  /*static*/
  std::string id3v2_4_frame::as_str(
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

      if (1 == unicode || 2 == unicode ) {
        // Spec says this is UTF-16 with BOM or without BOM, resp., but let's
        // be generous
        if (1 < cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
          src =  encoding::UTF_16BE;
        } else if (1 < cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
          src =  encoding::UTF_16LE;
        } else {
          src =  encoding::UTF_16;
        }
      }
      else if (3 == unicode) {
        src = encoding::UTF_8;
      }
      else {
        // TODO: Does this ever really happen?
        throw std::domain_error("illegal encoding");
      }

    }

    return convert_encoding<string>(pbuf, cbbuf, src, dstenc, rsp);
  }

}

/*static*/
std::unique_ptr<scribbu::id3v2_4_text_frame>
scribbu::id3v2_4_text_frame::create(const frame_id4 &id,
                                    const unsigned char *p,
                                    std::size_t cb,
                                    tag_alter_preservation tap,
                                    file_alter_preservation fap,
                                    read_only read_only,
                                    const boost::optional<unsigned char> &enc,
                                    const boost::optional<unsigned char> &gid,
                                    bool compressed,
                                    bool unsynchronised,
                                    const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_text_frame>(new id3v2_4_text_frame(id, p, p + cb, tap,
                                                               fap, read_only,
                                                               enc, gid,
                                                               compressed, unsynchronised,
                                                               dli));
}


///////////////////////////////////////////////////////////////////////////////
//                     miscellaneous creation functions                      //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::UFID_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new UFID_2_4(p, p + cb,
                                                     tap,
                                                     fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::ENCR_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new ENCR_2_4(p, p + cb,
                                                     tap,
                                                     fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::TXXX_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new TXXX_2_4(p, p + cb, tap, fap,
                                                     read_only, enc, gid,
                                                     compressed,
                                                     unsynchronised, dli));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::COMM_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new COMM_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::PCNT_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new PCNT_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::POPM_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new POPM_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}
