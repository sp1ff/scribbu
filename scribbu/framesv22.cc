#include <scribbu.hh>
#include <framesv22.hh>


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_2_frame                            //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void scribbu::id3v2_2_frame::accept_for_print(id3v2_acyclic_visitor &P,
                                              std::ostream          &os) const
{
  id3v2_2_frame_printer *p = dynamic_cast<id3v2_2_frame_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                                 class UFI                                 //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::UFI::accept_for_print(id3v2_acyclic_visitor &P,
                               std::ostream          &os) const
{
  UFI_printer *p = dynamic_cast<UFI_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


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

/*virtual*/
void
scribbu::id3v2_2_text_frame::accept_for_print(id3v2_acyclic_visitor &P,
                                              std::ostream          &os) const
{
  id3v2_2_text_printer *p = dynamic_cast<id3v2_2_text_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                                 class TXX                                 //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::TXX::accept_for_print(id3v2_acyclic_visitor &P,
                               std::ostream          &os) const
{
  TXX_printer *p = dynamic_cast<TXX_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                                 class COM                                 //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::COM:: accept_for_print(id3v2_acyclic_visitor &P,
                                std::ostream          &os) const
{
  COM_printer *p = dynamic_cast<COM_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                                 class CNT                                 //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::CNT:: accept_for_print(id3v2_acyclic_visitor &P,
                                std::ostream          &os) const
{
  CNT_printer *p = dynamic_cast<CNT_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}
