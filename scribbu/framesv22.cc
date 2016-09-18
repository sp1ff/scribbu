#include <scribbu.hh>
#include <framesv22.hh>


///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_2_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

std::string scribbu::id3v2_2_text_frame::as_utf8() const
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

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_text_frame::create(const frame_id3& id, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new id3v2_2_text_frame(id, p, p + cb) );
}





/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::UFI::create(const frame_id3& /*id*/, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new UFI(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::TXX::create(const frame_id3& /*id*/, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new TXX(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::COM::create(const frame_id3& id, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new COM(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::CNT::create(const frame_id3& /*id*/, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new CNT(p, p + cb) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::POP::create(const frame_id3& /*id*/, const unsigned char *p, std::size_t cb)
{
  return std::unique_ptr<scribbu::id3v2_2_frame>( new POP(p, p + cb) );
}
