#include "id3v2-printers.hh"


///////////////////////////////////////////////////////////////////////////////
//                        class compact_id3v2_printer                        //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream      &os,
                                         const id3v2_2_tag &x)
{
}

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream &os,
                                         const UFI    &x)
{
}

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream             &os,
                                         const id3v2_2_text_frame &x)
{
}

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream &os,
                                         const TXX    &x)
{
}

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream &os,
                                         const COM    &x)
{
}

/*virtual*/
void
scribbu::compact_id3v2_printer::print_on(std::ostream        &os,
                                         const id3v2_2_frame &x)
{
}
