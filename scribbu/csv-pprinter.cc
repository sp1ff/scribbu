/**
 * \file csv-pprinter.cc
 *
 * Copyright (C) 2015-2018 Michael Herstine <sp1ff@pobox.com>
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

#include "csv-pprinter.hh"

#include "ostream.hh"
#include "id3v1.hh"
#include "id3v22.hh"
#include "id3v23.hh"
#include "id3v24.hh"

namespace {

  unsigned int optional_to_uint(const boost::optional<bool> &x)
  {
    if (x) {
      return *x ? 1 : 0;
    }
    else {
      return ~0;
    }
  }

}

/*static*/
const boost::optional<scribbu::encoding>
scribbu::csv_pprinter::DEFAULT_V1ENC = boost::none;

/*static*/
const boost::optional<scribbu::encoding>
scribbu::csv_pprinter::DEFAULT_V2ENC = boost::none;

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_2_tag(const id3v2_2_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  const char * const COMMA = ",";

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << dec << (unsigned)tag.version() << COMMA <<
        (unsigned)tag.revision() << COMMA <<
        tag.size() << COMMA <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        COMMA << optional_to_uint(tag.unsynchronised()) << COMMA;

  for (auto id: {"TP1", "TT2", "TAL", "TCO", "TEN", "TYR", "TLA"}) {
    if (tag.has_frame(id)) {
      os << tag.get_frame(id);
    }
    os << COMMA;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("CNT");
  os << n << COMMA;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << COMMA;

  n = tag.has_frame("COM");
  os << n << COMMA;
  vector<COM> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_));
    }
    os << COMMA;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_3_tag(const id3v2_3_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  const char * const COMMA = ",";

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << dec << (unsigned) tag.version() << COMMA <<
        (unsigned)tag.revision() << COMMA <<
        tag.size() << COMMA <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        COMMA << optional_to_uint(tag.unsynchronised()) << COMMA;

  for (auto id: {"TPE1", "TIT2", "TALB", "TCON", "TENC", "TYER", "TLAN"}) {
    if (tag.has_frame(id)) {
      os << tag.get_frame(id);
    }
    os << COMMA;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("PCNT");
  os << n << COMMA;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << COMMA;

  n = tag.has_frame("COMM");
  os << n << COMMA;
  vector<COMM> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_));
    }
    os << COMMA;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_4_tag(const id3v2_4_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  const char * const COMMA = ",";

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << dec << (unsigned)tag.version() << COMMA <<
        (unsigned)tag.revision() << COMMA <<
        tag.size() << COMMA <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        COMMA << optional_to_uint(tag.unsynchronised()) << COMMA;

  for (auto id: {"TPE1", "TIT2", "TALB", "TCON", "TENC", "TYER", "TLAN"}) {
    if (tag.has_frame(id)) {
      stringstream stm;
      stm << print_as_csv(*this) << tag.get_frame(id);
      os << escape(stm.str());
    }
    os << COMMA;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("PCNT");
  os << n << COMMA;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << COMMA;

  n = tag.has_frame("COMM");
  os << n << COMMA;
  vector<COMM_2_4> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_));
    }
    os << COMMA;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_track_data(const track_data &data,
                                         std::ostream &os)
{
  using namespace std;

  const char * const COMMA = ",";

  os << dec << data.size() << COMMA << hex << setfill('0');

  vector<unsigned char> md5;
  data.get_md5(back_inserter(md5));

  for (auto x: md5) {
    os << setw(2) << (unsigned)x;
  }

  return os << COMMA;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v1_tag(const id3v1_tag &tag, std::ostream &os)
{
  using namespace std;

  const char * const COMMA = ",";

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << tag.v1_1() << COMMA << tag.extended() << COMMA <<
    escape(tag.artist<string>(v1enc_, dst, rsp)) << COMMA <<
    escape(tag.title<string>(v1enc_, dst, rsp)) << COMMA <<
    escape(tag.album<string>(v1enc_, dst, rsp)) << COMMA <<
    escape(tag.year<string>(v1enc_, dst, rsp)) << COMMA <<
    escape(tag.comment<string>(v1enc_, dst, rsp)) << COMMA <<
    dec << (unsigned)tag.genre() << COMMA;

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_2_text_frame(
  const id3v2_2_text_frame &frame,
  std::ostream &os)
{
  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFI(const UFI&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXX(const TXX&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COM(const COM&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_CNT(const CNT&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POP(const POP&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_3_text_frame(
  const id3v2_3_text_frame &frame,
  std::ostream &os)
{
  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFID(const UFID&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_ENCR(const ENCR&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXXX(const TXXX&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COMM(const COMM&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_PCNT(const PCNT&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POPM(const POPM&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_4_text_frame(
  const id3v2_4_text_frame &frame,
  std::ostream &os)
{
  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFID_2_4(const UFID_2_4&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_ENCR_2_4(const ENCR_2_4&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXXX_2_4(const TXXX_2_4&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COMM_2_4(const COMM_2_4&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_PCNT_2_4(const PCNT_2_4&, std::ostream&os)
{
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POPM_2_4(const POPM_2_4&, std::ostream&os)
{
  return os;
}

std::string
scribbu::csv_pprinter::escape(const std::string &s, char sep /*= ','*/)
{
  std::size_t comma = s.find(sep);
  if (std::string::npos == comma) {
    return s;
  }

  std::string r("\"");
  for (std::size_t i = 0, n = s.length(); i < n; ) {
    std::size_t dquote = s.find('"', i);
    r.append(s.substr(i, dquote - i));
    if (std::string::npos == dquote) {
      break;
    }
    r += "\"\"";
    i = dquote + 1;
  }

  r += '"';

  return r;

}
