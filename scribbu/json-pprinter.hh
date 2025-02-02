/**
 * \file json-pprinter.hh
 *
 * Copyright (C) 2025 Michael Herstine <sp1ff@pobox.com>
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

#ifndef JSON_PPRINTER_INCLUDED_HH
#define JSON_PPRINTER_INCLUDED_HH 1

#include <scribbu/pprinter.hh>

namespace scribbu {

  /**
   * \class json_pprinter
   *
   * \brief A pretty-printer that produces JSON
   *
   * I had first thought to implement this using the boost json library, but
   * soon realized that wouldn't work with this framework-- I can't accumulate
   * the entire object without going through `operator<<`.
   *
   *
   */

  class json_pprinter : public pprinter {
  public:
    static const boost::optional<encoding> DEFAULT_V1ENC;
    static const boost::optional<encoding> DEFAULT_V2ENC;

  public:
    static std::string escape(const std::string &s);

  public:
    json_pprinter(const boost::optional<encoding> &v1enc = DEFAULT_V1ENC,
                  const boost::optional<encoding> &v2enc = DEFAULT_V2ENC):
      v1enc_(v1enc), v2enc_(v2enc)
      {}

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

    virtual ~json_pprinter()
    { }
    virtual pprinter* clone() {
      return new json_pprinter(*this);
    }

  private:
    boost::optional<encoding> v1enc_;
    boost::optional<encoding> v2enc_;
  };

  class print_as_json: public pprint_manipulator {
  public:
    print_as_json(const boost::optional<encoding> &v1enc =
                  json_pprinter::DEFAULT_V1ENC,
                  const boost::optional<encoding> &v2enc =
                  json_pprinter::DEFAULT_V2ENC):
      pprint_manipulator(new json_pprinter(v1enc, v2enc))
    { }
    print_as_json(const json_pprinter &that):
      pprint_manipulator(new json_pprinter(that))
    { }
  };

} // End namespace scribbu.

#endif // JSON_PPRINTER_INCLUDED_HH
