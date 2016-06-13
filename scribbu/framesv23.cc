#include <framesv23.hh>



///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_3_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

std::string scribbu::id3v2_3_text_frame::as_utf8() const
{
  using scribbu::detail::iconv_guard;

  const char * const ISO88591 = "ISO-8859-1";
  const char * const UCS2BE   = "UCS-2BE";
  const char * const UCS2LE   = "UCS-2LE";
  const char * const UCS2     = "UCS-2";

  const char *encoding = ISO88591;
  if (unicode()) {
    if (1 < text_.size() && 0xfe == text_[0] && 0xff == text_[1]) {
      encoding = UCS2BE;
    } else if (1 < text_.size() && 0xff == text_[0] && 0xfe == text_[1]) {
      encoding = UCS2LE;
    } else {
      encoding = UCS2;
    }
  }

  iconv_guard guard("UTF-8", encoding);
  return to_utf8(guard, &(text_[0]), text_.size());
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::id3v2_3_text_frame::create(const frame_id4                      &id,
                                    const unsigned char                  *p,
                                    std::size_t                           cb,
                                    tag_alter_preservation                tag_alter_preservation,
                                    file_alter_preservation               file_alter_preservation,
                                    read_only                             read_only,
                                    const boost::optional<unsigned char> &encryption_method,
                                    const boost::optional<unsigned char> &group_id,
                                    const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new id3v2_3_text_frame(id, p, p + cb, tag_alter_preservation,
                                                               file_alter_preservation, read_only,
                                                               encryption_method, group_id,
                                                               decompressed_size));
}


///////////////////////////////////////////////////////////////////////////////
//                     miscellaneous creation functions                      //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::UFID::create(const frame_id4                      &/*id*/,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new UFID(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::ENCR::create(const frame_id4                      &/*id*/,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new ENCR(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::TXXX::create(const frame_id4                      &id,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new TXXX(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::COMM::create(const frame_id4                      &id,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new COMM(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::PCNT::create(const frame_id4                      &id,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new PCNT(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}

/*static*/ std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::POPM::create(const frame_id4                      &id,
                      const unsigned char                  *p,
                      std::size_t                           cb,
                      tag_alter_preservation                tag_alter_preservation,
                      file_alter_preservation               file_alter_preservation,
                      read_only                             read_only,
                      const boost::optional<unsigned char> &encryption_method,
                      const boost::optional<unsigned char> &group_id,
                      const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<id3v2_3_frame>(new POPM(p, p + cb, tag_alter_preservation,
                                                 file_alter_preservation, read_only,
                                                 encryption_method, group_id,
                                                 decompressed_size));
}
