/**
 * \file tdf-pprinter.hh
 *
 * Copyright (C) 2019-2022 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#ifndef TDF_PPRINTER_INCLUDED_HH
#define TDF_PPRINTER_INCLUDED_HH 1

#include <scribbu/pprinter.hh>

namespace scribbu {

  /**
   * \class tdf_pprinter
   *
   * \brief A pretty-pritner that produces TAB or ASCII-delimited values
   *
   *
   * tdf_pprinter is a pprinter implementation that produces TAB-delimited
   * values; each entity being pretty-printed will write out its fields
   * separated by a TAB. It differs from csv_pprinter in that there is no
   * escaping mechanism-- field values containing TABS are simply not allowed.
   * More specifically, it produces output in the same format as the
   * \ref tdf_pprinter_ref_1 "text/tab-separated-values" MIME type.
   *
   * As pointed out in \ref tdf_pprinter_ref_3 "[3]", however, there is really
   * no need for this-- the ASCII character set has control codes designed for
   * this very purpose: this is generally referred
   * to as \ref tdf_pprinter_ref_2 "ASCII-delimited text". Since the mechanisms
   * are so similar, differing only in the delimiter byte, I've decided to
   * support that in this implementation as well.
   *
   * Finally, while the \ref tdf_pprinter_ref_1 "text/tab-separated-values" MIME
   * type makes no mention of non-ASCII character encodings, enocdings which
   * agree with ASCII in their first 128 positions (such as UTF-8) can easily be
   * used, as well. This implementation deduces the output encoding from the
   * ostream provided to the \c pprint_ methods; if char is not a code unit for
   * that encoding, a bad_code_unit exception shall be thrown.
   *
   *
   * 1. \anchor tdf_pprinter_ref_1 [1] Lindner, Paul, U of MN Internet Gopher
   * Team. IANA, Text Media Types,
   * text/tab-separated-values. https://www.iana.org/assignments/media-types/text/tab-separated-values
   * (updated January 1, 2019)
   *
   * 2. \anchor tdf_pprinter_ref_2 [2] multiple authors. ASCII Delimited
   * Text. https://en.wikipedia.org/wiki/Delimiter#ASCII_delimited_text
   * (updated January 1, 2019)
   *
   * 3. \anchor tdf_pprinter_ref_3 [3] Duncan, Ronald. Text File formats – ASCII
   * Delimited Text – Not CSV or TAB delimited text.
   * https://ronaldduncan.wordpress.com/2009/10/31/text-file-formats-ascii-delimited-text-not-csv-or-tab-delimited-text/#comments
   * (updated August 31, 2019)
   *
   *
   */

  class tdf_pprinter: public pprinter {

  public:

    static const std::size_t DEFAULT_NCOMMENTS = 4;
    static const boost::optional<encoding> DEFAULT_V1ENC;
    static const boost::optional<encoding> DEFAULT_V2ENC;
    static const bool DEFAULT_USE_ASCII = false;

  public:
    tdf_pprinter(std::size_t                      ncomm = DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc = DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc = DEFAULT_V2ENC,
                 bool                             ascii = DEFAULT_USE_ASCII);

  public:
    virtual std::ostream&
    pprint_v2_2_tag(const id3v2_2_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_3_tag(const id3v2_3_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_4_tag(const id3v2_4_tag&, std::ostream&);
    virtual std::ostream&
    pprint_track_data(const track_data&, std::ostream&);
    virtual std::ostream&
    pprint_v1_tag(const id3v1_tag&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_2_text_frame(const id3v2_2_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFI(const UFI&, std::ostream&);
    virtual std::ostream&
    pprint_TXX(const TXX&, std::ostream&);
    virtual std::ostream&
    pprint_COM(const COM&, std::ostream&);
    virtual std::ostream&
    pprint_CNT(const CNT&, std::ostream&);
    virtual std::ostream&
    pprint_POP(const POP&, std::ostream&);
    virtual std::ostream&
    pprint_XTG(const XTG&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_3_text_frame(const id3v2_3_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID(const UFID&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR(const ENCR&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX(const TXXX&, std::ostream&);
    virtual std::ostream&
    pprint_COMM(const COMM&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT(const PCNT&, std::ostream&);
    virtual std::ostream&
    pprint_POPM(const POPM&, std::ostream&);
    virtual std::ostream&
    pprint_XTAG(const XTAG&, std::ostream&);
    virtual std::ostream&
    pprint_PRIV(const PRIV&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_4_text_frame(const id3v2_4_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID_2_4(const UFID_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR_2_4(const ENCR_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX_2_4(const TXXX_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_COMM_2_4(const COMM_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT_2_4(const PCNT_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_POPM_2_4(const POPM_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_XTAG_2_4(const XTAG_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_PRIV_2_4(const PRIV_2_4&, std::ostream&);

    virtual ~tdf_pprinter()
    { }
    virtual pprinter* clone() {
      return new tdf_pprinter(*this);
    }

  private:
    std::size_t ncomm_;
    boost::optional<encoding> v1enc_;
    boost::optional<encoding> v2enc_;
    char sep_;

  }; // End class tdf_pprinter.

  class print_as_tdf: public pprint_manipulator {
  public:
    print_as_tdf(std::size_t ncomm = tdf_pprinter::DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc =
                   tdf_pprinter::DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc =
                 tdf_pprinter::DEFAULT_V2ENC,
                 bool ascii = tdf_pprinter::DEFAULT_USE_ASCII):
      pprint_manipulator(new tdf_pprinter(ncomm, v1enc, v2enc, ascii))
    { }
    print_as_tdf(const tdf_pprinter &that):
      pprint_manipulator(new tdf_pprinter(that))
    { }
  };

} // End namespace scribbu.

#endif // not TDF_PPRINTER_INCLUDED_HH
