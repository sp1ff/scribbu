/**
 * \file csv-pprinter.hh
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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

#ifndef CSV_PPRINTER_INCLUDED_HH
#define CSV_PPRINTER_INCLUDED_HH 1

#include <scribbu/pprinter.hh>

namespace scribbu {

  /**
   * \class csv_pprinter
   *
   * \brief A pretty-printer that produces comma-separated values
   *
   *
   * csv_pprinter is a pprinter implementation that produces output that
   * attempts to comply with \ref csv_pprinter_ref_1 "RFC 4180". I say
   * "attempts" due to the fact that CSV has not been standardized (see \ref
   * csv_pprinter_ref_2 "[2]" or below for for some of the problems).
   *
   *
   * More specifically, this implementation shall produce output according
   * to the grammar:
   *
   \code

   record = field *(SEPARATOR field)

   field = (escaped|non-escaped)

   escaped = ESCAPE *(TEXTDATA | SEPARATOR | CR | LF | 2ESCAPE) ESCAPE

   non-escaped = *TEXTDATA

   CR = 0x0D ;as per section 6.1 of RFC 2234 [2]

   LF = 0x0A ;as per section 6.1 of RFC 2234 [2]

   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]

   TEXTDATA = (0x09|0x20|0x21|0x23-0xFF) - {SEPARATOR}

   SEPARATOR = (0x09|0x21-0x2F|0x5B-0x60|0x7B-0x7E) (0x2C = ',' by default)

   ESCAPE = 0x22 = '"'

   \endcode
   *
   * Note that tabs are explicitly included here, so one \em could use this
   * implementation to produce tab-delimited values, with any tabs within a
   * field escaped. Note that this is different than TSV format, which generally
   * doesn't escape tabs within fields (it simply forbids them).
   *
   * csv_pprinter instances shall use the text encoding with which the output
   * streams provided are imbued (UTF-8, by default). If char is not a code unit
   * for that encoding a bad_code_unit exception shall be thrown.
   *
   *
   * <a href="https://tools.ietf.org/html/rfc4180">RFC 4180</a> \em implies that
   * the character ecnoding for CSV output shall be ASCII (it \em specifies that
   * the fields shall be comprised of characters encoded as single bytes in the
   * range "%x20-21 / %x23-2B / %x2D-7E"). That seems un-necessarily restrictive
   * to me, however. There are any number of character encodings that match
   * ASCII below 128 (the most important of which being UTF-8); using such an
   * encoding we could retain the use of the comma as a separator and the
   * double-quote character to escape commas or line breaks (UTF-8 always sets
   * the high bit in second & later bytes, so there's no chance of confusing
   * them with ASCII).
   *
   * In addition, there are locales where the comma is used as a decimal point
   * (AKA radix character); \ref csv_pprinter_ref_2 "some" implementations will
   * switch to the ';' character in such locales. It seems prudent to allow the
   * column marker to be configurable (so long as it is a valid ASCII value).
   *
   * Allowing the column separator to be any ASCII character, however, turns
   * out to be inconvenient in the implementation (any column now has to be
   * checked for escaping, regardless if it's something we "know" can't have
   * a comma). Therefore, I've made the column separator and quote characters
   * configurable, but limited the permissible range to non-alphanumeric
   * characters.
   *
   *
   * 1. \anchor csv_pprinter_ref_1 [1] Shafranovich. RFC 4180 Common Format and
   * MIME Type for Comma-Separated Values (CSV) Files
   * https://tools.ietf.org/html/rfc4180 (updated Deceber 29, 2018)
   *
   * 2. \anchor csv_pprinter_ref_2 [2] Warrick, Chris. CSV is not a standard
   * https://chriswarrick.com/blog/2017/04/07/csv-is-not-a-standard/ (updated
   * December 30, 2018).
   *
   *
   */

  class csv_pprinter: public pprinter {

  public:

    static const std::size_t DEFAULT_NCOMMENTS = 4;
    static const boost::optional<encoding> DEFAULT_V1ENC;
    static const boost::optional<encoding> DEFAULT_V2ENC;
    static const char DEFAULT_SEP = ',';
    static const char ESC = '"';

    /**
     * \brief Escape a string for CSV output per RFC 4180
     *
     *
     * \param s [in] the text to be escaped
     *
     * \param sep [in] the delimiter to be used; must be valid ASCII
     *
     * \param esc [in] the escape character to be used; must be valid ASCII
     *
     * \return \a s if \a sep does not appear in \a s, otherwise, \a esc
     * \a s \esc with all occurences of \a esc in \a s repeated twice
     *
     * \pre \a s uses an encoding that is ASCII-compatible
     *
     *
     * Take some text & escape it for output in CSV format, if needed.
     *
     *
     */

    static std::string escape(const std::string &s, char sep, char esc = ESC);

    class bad_separator: public error
    {
    public:
      bad_separator(char sep): sep_(sep)
      { }
      virtual const char * what() const noexcept(true);

    private:
      char sep_;
      mutable std::shared_ptr<std::string> pwhat_;

    };

  public:
    csv_pprinter(std::size_t                      ncomm = DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc = DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc = DEFAULT_V2ENC,
                 char                             sep   = DEFAULT_SEP);

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

    virtual ~csv_pprinter()
    { }
    virtual pprinter* clone() {
      return new csv_pprinter(*this);
    }

  private:
    std::string escape(const std::string &s)
    { return escape(s, sep_, ESC); }

  private:
    std::size_t ncomm_;
    boost::optional<encoding> v1enc_;
    boost::optional<encoding> v2enc_;
    char sep_;

  };

  class print_as_csv: public pprint_manipulator {
  public:
    print_as_csv(std::size_t ncomm = csv_pprinter::DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc =
                   csv_pprinter::DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc =
                 csv_pprinter::DEFAULT_V2ENC,
                 char sep = csv_pprinter::DEFAULT_SEP):
      pprint_manipulator(new csv_pprinter(ncomm, v1enc, v2enc, sep))
    { }
    print_as_csv(const csv_pprinter &that):
      pprint_manipulator(new csv_pprinter(that))
    { }
  };

} // End namespace scribbu.

#endif // not CSV_PPRINTER_INCLUDED_HH
