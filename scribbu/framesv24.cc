#include <framesv24.hh>


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_4_frame                            //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::id3v2_4_frame::accept_for_print(id3v2_acyclic_visitor &P,
                                         std::ostream          &os) const
{
  id3v2_4_frame_printer *p = dynamic_cast<id3v2_4_frame_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                              class UFID_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::UFID_2_4::accept_for_print(id3v2_acyclic_visitor &P,
                                    std::ostream          &os) const
{
  UFID_2_4_printer *p = dynamic_cast<UFID_2_4_printer*>(&P);
  if (p) {
    p->print_on(os, *this);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_4_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

std::string scribbu::id3v2_4_text_frame::as_utf8() const
{
  using scribbu::detail::iconv_guard;

  // TODO: Not sure how to handle this... what if encoding_ is not 0, 1, 2, or
  // 3?
  const char * const ISO_8859_1 = "ISO-8859-1";
  const char * const UTF_16     = "UTF-16";
  const char * const UTF_16_BE  = "UTF-16BE";

  if (3 == encoding_) {
    // TODO: Spec says this should be null-terminated; remove that null, if
    // present.
    return std::string(text_.begin(), text_.end());
  }

  const char * p;
  if (0 == encoding_) {
    p = ISO_8859_1;
  } else if (1 == encoding_) {
    p = UTF_16;
  } else if (2 == encoding_) {
    p = UTF_16_BE;
  } else {
    throw std::runtime_error("Unknown encoding");
  }

  iconv_guard guard("UTF-8", p);
  return to_utf8(guard, &(text_[0]), text_.size());
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::id3v2_4_text_frame::create(const frame_id4                      &id,
                                    const unsigned char                  *p,
                                    std::size_t                           cb,
                                    tag_alter_preservation                tag_alter_preservation,
                                    file_alter_preservation               file_alter_preservation,
                                    read_only                             read_only,
                                    const boost::optional<unsigned char> &encryption_method,
                                    const boost::optional<unsigned char> &group_id,
                                    bool                                  compressed,
                                    bool                                  unsynchronised,
                                    const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<id3v2_4_frame>(new id3v2_4_text_frame(id, p, p + cb, tag_alter_preservation,
                                                               file_alter_preservation, read_only,
                                                               encryption_method, group_id,
                                                               compressed, unsynchronised,
                                                               data_len_indicator));
}


///////////////////////////////////////////////////////////////////////////////
//                     miscellaneous creation functions                      //
///////////////////////////////////////////////////////////////////////////////


/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::ENCR_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<id3v2_4_frame>(new ENCR_2_4(p, p + cb,
                                                     tag_alter_preservation,
                                                     file_alter_preservation,
                                                     read_only, encryption_method,
                                                     group_id, compressed,
                                                     unsynchronised, data_len_indicator));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::TXXX_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<id3v2_4_frame>(new TXXX_2_4(p, p + cb,
                                                       tag_alter_preservation,
                                                       file_alter_preservation,
                                                       read_only, encryption_method,
                                                       group_id, compressed,
                                                       unsynchronised, data_len_indicator));
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::POPM_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<id3v2_4_frame>(new POPM_2_4(p, p + cb,
                                                     tag_alter_preservation,
                                                     file_alter_preservation,
                                                     read_only, encryption_method,
                                                     group_id, compressed,
                                                     unsynchronised, data_len_indicator));
}
