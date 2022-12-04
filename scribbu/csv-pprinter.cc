/**
 * \file csv-pprinter.cc
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

#include "csv-pprinter.hh"

#include "ostream.hh"
#include "id3v1.hh"
#include "id3v22.hh"
#include "id3v23.hh"
#include "id3v24.hh"

/*virtual*/
const char * scribbu::csv_pprinter::bad_separator::what() const noexcept(true)
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << (int) sep_ << " is not a valid separater";
    pwhat_.reset(new std::string(stm.str()));
  }
  return pwhat_->c_str();
}

/*static*/
const boost::optional<scribbu::encoding>
scribbu::csv_pprinter::DEFAULT_V1ENC = boost::none;

/*static*/
const boost::optional<scribbu::encoding>
scribbu::csv_pprinter::DEFAULT_V2ENC = boost::none;

/*static*/ std::string
scribbu::csv_pprinter::escape(const std::string &s, char sep, char esc /*= '"'*/)
{
  const char NEEDS_ESC[] = { sep, '\n', 0 };
  std::size_t comma = s.find_first_of(NEEDS_ESC);
  if (std::string::npos == comma) {
    return s;
  }

  std::string r(1, esc);
  for (std::size_t i = 0, n = s.length(); i < n; ) {
    std::size_t dquote = s.find(esc, i);
    r.append(s.substr(i, dquote - i));
    if (std::string::npos == dquote) {
      break;
    }
    r += esc; r += esc;
    i = dquote + 1;
  }

  r += esc;

  return r;

}

scribbu::csv_pprinter::csv_pprinter(
  std::size_t                      ncomm /*= DEFAULT_NCOMMENTS*/,
  const boost::optional<encoding> &v1enc /*= DEFAULT_V1ENC    */,
  const boost::optional<encoding> &v2enc /*= DEFAULT_V2ENC    */,
  char                             sep   /*= DEFAULT_SEP      */):
  ncomm_(ncomm), v1enc_(v1enc), v2enc_(v2enc), sep_(sep)
{
  if (sep_ != 0x09 &&
      (sep_ < 0x21 || sep_ > 0x2F) &&
      (sep_ < 0x3A || sep_ > 0x40) &&
      (sep_ < 0x5B || sep_ > 0x60) &&
      (sep_ < 0x7B || sep_ > 0x7E)) {
    throw bad_separator(sep_);
  }
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_2_tag(const id3v2_2_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  os << dec << (unsigned)tag.version() << sep_ <<
        (unsigned)tag.revision() << sep_ <<
        tag.size() << sep_ <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        sep_ << optional_to_uint(tag.unsynchronised()) << sep_;

  for (auto id: {"TP1", "TT2", "TAL", "TCO", "TEN", "TYR", "TLA"}) {
    if (tag.has_frame(id)) {
      os << tag.get_frame(id);
    }
    os << sep_;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("CNT");
  os << n << sep_;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << sep_;

  n = tag.has_frame("COM");
  os << n << sep_;
  vector<COM> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_), sep_, ESC);
    }
    if (i != ncomm_ - 1) {
      os << sep_;
    }
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_3_tag(const id3v2_3_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  os << dec << (unsigned) tag.version() << sep_ <<
        (unsigned)tag.revision() << sep_ <<
        tag.size() << sep_ <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        sep_ << optional_to_uint(tag.unsynchronised()) << sep_;

  for (auto id: {"TPE1", "TIT2", "TALB", "TCON", "TENC", "TYER", "TLAN"}) {
    if (tag.has_frame(id)) {
      os << tag.get_frame(id);
    }
    os << sep_;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("PCNT");
  os << n << sep_;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << sep_;

  n = tag.has_frame("COMM");
  os << n << sep_;
  vector<COMM> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_), sep_, ESC);
    }
    if (i != ncomm_ - 1) {
      os << sep_;
    }
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v2_4_tag(const id3v2_4_tag &tag,
                                       std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  os << dec << (unsigned)tag.version() << sep_ <<
        (unsigned)tag.revision() << sep_ <<
        tag.size() << sep_ <<
        "0x" << hex << setw(2) << setfill('0') << (unsigned)tag.flags() <<
        sep_ << optional_to_uint(tag.unsynchronised()) << sep_;

  for (auto id: {"TPE1", "TIT2", "TALB", "TCON", "TENC", "TYER", "TLAN"}) {
    if (tag.has_frame(id)) {
      os << tag.get_frame(id);
    }
    os << sep_;
  }

  // There *should* only be one (by spec), but that's not always the case in
  // the wild; print the count...
  size_t n = tag.has_frame("PCNT");
  os << n << sep_;
  if (1 == n) {
    os << tag.play_count();
  }
  else if (1 < n) {
    os << '*';
  }

  os << sep_;

  n = tag.has_frame("COMM");
  os << n << sep_;
  vector<COMM_2_4> comments;
  tag.get_comments(back_inserter(comments));
  for (size_t i = 0; i < ncomm_; ++i) {
    if (i < comments.size()) {
      os << escape(comments[i].text<string>(dst, rsp, v2enc_), sep_, ESC);
    }
    if (i != ncomm_ - 1) {
      os << sep_;
    }
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_track_data(const track_data &data,
                                         std::ostream &os)
{
  using namespace std;

  os << dec << data.size() << sep_ << hex << setfill('0');

  vector<unsigned char> md5;
  data.get_md5(back_inserter(md5));

  for (auto x: md5) {
    os << setw(2) << (unsigned)x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_v1_tag(const id3v1_tag &tag, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  os << tag.v1_1() << sep_ << tag.extended() << sep_ <<
    escape(tag.artist<string>(v1enc_, dst, rsp), sep_, ESC) << sep_ <<
    escape(tag.title<string>(v1enc_, dst, rsp), sep_, ESC) << sep_ <<
    escape(tag.album<string>(v1enc_, dst, rsp), sep_, ESC) << sep_ <<
    escape(tag.year<string>(v1enc_, dst, rsp), sep_, ESC) << sep_ <<
    escape(tag.comment<string>(v1enc_, dst, rsp), sep_, ESC) << sep_ <<
    dec << (unsigned)tag.genre();

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &f,
                                                std::ostream &os)
{
  return os << f.id();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_2_text_frame(
  const id3v2_2_text_frame &frame,
  std::ostream &os)
{
  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_), sep_, ESC);
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFI(const UFI &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  os << escape(f.owner<string>(dst, rsp, src)) << sep_;

  vector<unsigned char> buf;
  f.idb(back_inserter(buf));

  os << "0x";
  for (auto x: buf) {
    os << setw(2) << setfill('0') << x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXX(const TXX &f, std::ostream &os)
{
  using std::string;

  string dsc = f.description<string>();
  string txt = f.text<string>();
  return os << (unsigned)f.unicode() << sep_ << escape(dsc) << sep_ << escape(txt);
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COM(const COM &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  char lang[3];
  f.lang(lang);

  return os << lang[0] << lang[1] << lang[2] << sep_ <<
    escape(f.description<string>(dst, rsp, v2enc_)) << sep_ <<
    escape(f.text<string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_CNT(const CNT &f, std::ostream&os)
{
  return os << (unsigned) f.count();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POP(const POP &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::UTF_8;
  if (v2enc_) {
    src = *v2enc_;
  }
  os << escape(f.email<string>(dst, rsp, src)) << sep_ <<
    dec << (unsigned)f.rating() << sep_;

  vector<unsigned char> counter;
  f.counterb(back_inserter(counter));

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_XTG(const XTG &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  string own = f.owner(), tags = f.urlencoded();
  os << escape(convert_encoding<string>(own.c_str(), own.length(),
                                        encoding::UTF_8, dst, rsp)) <<
    sep_ << escape(convert_encoding<string>(tags.c_str(), tags.length(),
                                            encoding::UTF_8, dst, rsp));

  return os;

}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame &f, std::ostream &os)
{
  return os << f.id();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_3_text_frame(
  const id3v2_3_text_frame &frame,
  std::ostream &os)
{
  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_), sep_, ESC);
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFID(const UFID &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  os << escape(f.owner<string>(dst, rsp, src)) << sep_;

  vector<unsigned char> buf;
  f.idb(back_inserter(buf));

  os << "0x";
  for (auto x: buf) {
    os << setw(2) << setfill('0') << x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_ENCR(const ENCR &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  vector<unsigned char> buf;
  f.datab(back_inserter(buf));

  os << escape(f.email<string>(dst, rsp, src)) << sep_ <<
    (unsigned) f.method_symbol() << sep_ << "0x";
  for (auto x: buf) {
    os << setw(2) << setfill('0') << x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXXX(const TXXX &f, std::ostream &os)
{
  using std::string;

  string dsc = f.description<string>();
  string txt = f.text<string>();
  return os << (unsigned)f.unicode() << sep_ << escape(dsc) << sep_ << escape(txt);
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COMM(const COMM &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  char lang[3];
  f.lang(lang);

  return os << lang[0] << lang[1] << lang[2] << sep_ <<
    escape(f.description<string>(dst, rsp, v2enc_)) << sep_ <<
    escape(f.text<string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_PCNT(const PCNT &f, std::ostream&os)
{
  return os << (unsigned) f.count();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POPM(const POPM &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  os << escape(f.email<string>(dst, rsp, src)) << sep_ <<
    dec << (unsigned)f.rating() << sep_;

  vector<unsigned char> counter;
  f.counterb(back_inserter(counter));

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_XTAG(const XTAG &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  string own = f.owner(), tags = f.urlencoded();
  os << escape(convert_encoding<string>(own.c_str(), own.length(),
                                        encoding::UTF_8, dst, rsp)) <<
    sep_ << escape(convert_encoding<string>(tags.c_str(), tags.length(),
                                            encoding::UTF_8, dst, rsp));

  return os;

}
/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame &f,
                                                std::ostream &os)
{
  return os << f.id();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_id3v2_4_text_frame(
  const id3v2_4_text_frame &frame,
  std::ostream &os)
{
  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  return os << escape(frame.as_str<std::string>(dst, rsp, v2enc_), sep_, ESC);
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_UFID_2_4(const UFID_2_4 &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  os << escape(f.owner<string>(dst, rsp, src)) << sep_;

  vector<unsigned char> buf;
  f.idb(back_inserter(buf));

  os << "0x";
  for (auto x: buf) {
    os << setw(2) << setfill('0') << x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_ENCR_2_4(const ENCR_2_4 &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  vector<unsigned char> buf;
  f.datab(back_inserter(buf));

  os << escape(f.email<string>(dst, rsp, src)) << sep_ <<
    (unsigned) f.method_symbol() << sep_ << "0x";
  for (auto x: buf) {
    os << setw(2) << setfill('0') << x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_TXXX_2_4(const TXXX_2_4 &f, std::ostream &os)
{
  using std::string;

  string dsc = f.description<string>();
  string txt = f.text<string>();
  return os << (unsigned)f.unicode() << sep_ << escape(dsc) << sep_ << escape(txt);
  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_COMM_2_4(const COMM_2_4 &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  char lang[3];
  f.lang(lang);

  return os << lang[0] << lang[1] << lang[2] << sep_ <<
    escape(f.description<string>(dst, rsp, v2enc_)) << sep_ <<
    escape(f.text<string>(dst, rsp, v2enc_));
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_PCNT_2_4(const PCNT_2_4 &f, std::ostream &os)
{
  return os << (unsigned) f.count();
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_POPM_2_4(const POPM_2_4 &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  os << escape(f.email<string>(dst, rsp, src)) << sep_ <<
    dec << (unsigned)f.rating() << sep_;

  vector<unsigned char> counter;
  f.counterb(back_inserter(counter));

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::csv_pprinter::pprint_XTAG_2_4(const XTAG_2_4 &f, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  std::tie(dst, rsp) = encoding_from_stream(os);

  string own = f.owner(), tags = f.urlencoded();
  os << escape(convert_encoding<string>(own.c_str(), own.length(),
                                        encoding::UTF_8, dst, rsp)) <<
    sep_ << escape(convert_encoding<string>(tags.c_str(), tags.length(),
                                            encoding::UTF_8, dst, rsp));

  return os;

}
