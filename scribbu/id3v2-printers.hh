#ifndef ID3V22_PRINTERS_HH_INCLUDED
#define ID3V22_PRINTERS_HH_INCLUDED 1

#include <scribbu/id3v22.hh>
#include <scribbu/framesv22.hh>

namespace scribbu {


  class compact_id3v2_printer:
    public id3v2_2_tag_printer,
    public UFI_printer,
    public id3v2_2_text_printer,
    public TXX_printer,
    public COM_printer,
    public id3v2_2_frame_printer
  {
  public:
    virtual void print_on(std::ostream&, const id3v2_2_tag&);
    virtual void print_on(std::ostream&, const UFI&);
    virtual void print_on(std::ostream&, const id3v2_2_text_frame&);
    virtual void print_on(std::ostream&, const TXX&);
    virtual void print_on(std::ostream&, const COM&);
    virtual void print_on(std::ostream&, const id3v2_2_frame&);
  };




      // id3v2_acyclic_visitor* p = (id3v2_acyclic_visitor*)os.pword(id3v2_manip::index());
      // if (! p) {
      //   p = &DEFAULT_ID3V2_PRINTER;
      // }


  // inline
  // std::ostream&
  // operator<<(std::ostream &os, const id3v2_2_tag &tag)
  // {
  //   return scribbu::detail::insert(os, tag);
  // }

  // inline
  // std::ostream&
  // operator<<(std::ostream &os, const id3v2_3_tag &tag)
  // {
  //   return scribbu::detail::insert(os, tag);
  // }

  // inline
  // std::ostream&
  // operator<<(std::ostream &os, const id3v2_4_tag &tag)
  // {
  //   return scribbu::detail::insert(os, tag);
  // }

} // End namespace scribbu.

#endif // not ID3V22_PRINTERS_HH_INCLUDED
