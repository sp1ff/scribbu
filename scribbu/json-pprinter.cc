/**
 * \file json-pprinter.cc
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

#include "json-pprinter.hh"

#include "ostream.hh"
#include "id3v1.hh"
#include "id3v22.hh"
#include "id3v23.hh"
#include "id3v24.hh"

#include <boost/algorithm/string/join.hpp>

#include <algorithm>
#include <strstream>

/*static*/
const boost::optional<scribbu::encoding>
scribbu::json_pprinter::DEFAULT_V1ENC = boost::none;

/*static*/
const boost::optional<scribbu::encoding>
scribbu::json_pprinter::DEFAULT_V2ENC = boost::none;

/*static*/
std::string scribbu::json_pprinter::escape(const std::string &s)
{
  std::stringstream stm;
  for (char c : s) {
    switch (c) {
    case '\b':
      stm << "\\b";
      break;
    case '\f':
      stm << "\\f";
      break;
    case '\n':
      stm << "\\n";
      break;
    case '\r':
      stm << "\\r";
      break;
    case '\t':
      stm << "\\t";
      break;
    case '"':
    case '\\':
      stm << '\\' << c;
      break;
    default:
      stm << c;
      break;
    }
  }
    return stm.str();
}

/*virtual*/ std::ostream &
scribbu::json_pprinter::pprint_v2_2_tag(const id3v2_2_tag &tag,
                                        std::ostream &os)
{
  using namespace std;

  os << R"({"version":"2.2","unsynchronised":)"
     << (tag.unsynchronised() ? "true" : "false")
     << R"(,"flags":)" << setbase(10) << tag.flags()
     << R"(,"size":)" << tag.size()
     << R"(,"padding":)" << tag.padding()
     << R"(",frames:[")";

  vector<string> frames;
  transform(tag.begin(), tag.end(), back_inserter(frames),
            [this](const id3v2_2_frame &frm) {
              ostrstream stm;
              stm << print_as_json(v1enc_, v2enc_) << frm;
              return stm.str();
            });

  os << boost::algorithm::join(frames, ",");

  return os << "]}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_v2_3_tag(const id3v2_3_tag &tag,
                                        std::ostream &os)
{
  using namespace std;

  os << R"({"version":"2.3","unsynchronised":)"
     << (tag.unsynchronised() ? "true" : "false")
     << R"(,"flags":)" << setbase(10) << (int)tag.flags()
     << R"(,"size":)" << tag.size()
     << R"(,"padding":)" << tag.padding()
     << R"(,"frames":[)";

  vector<string> frames;
  transform(tag.begin(), tag.end(), back_inserter(frames),
            [this](const id3v2_3_frame &frm) {
              ostrstream stm;
              stm << print_as_json(v1enc_, v2enc_) << frm << ends;
              return stm.str();
            });

  os << boost::algorithm::join(frames, ",");

  return os << "]}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_v2_4_tag(const id3v2_4_tag &tag,
                                        std::ostream &os)
{
  using namespace std;

  os << R"({"version":"2.4","unsynchronised":)"
     << (tag.unsynchronised() ? "true" : "false")
     << R"(,"flags":)" << setbase(10) << tag.flags()
     << R"(,"size":)" << tag.size()
     << R"(,"experimental":)" << (tag.experimental() ? "true" : "false")
     << R"(,"padding":)" << tag.padding()
     << R"(",frames:[")";

  vector<string> frames;
  transform(tag.begin(), tag.end(), back_inserter(frames),
            [this](const id3v2_4_frame &frm) {
              ostrstream stm;
              stm << print_as_json(v1enc_, v2enc_) << frm;
              return stm.str();
            });

  os << boost::algorithm::join(frames, ",");

  return os << "]}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_track_data(const track_data &d,
                                          std::ostream &os)
{
  using namespace std;

  os << R"({"md5":")" << hex << setfill('0');
  vector<unsigned char> md5;
  d.get_md5(back_inserter(md5));

  for (auto x: md5) {
    os << setw(2) << (unsigned)x;
  }

  return os << R"("})";
}

/*virtual*/ std::ostream &
scribbu::json_pprinter::pprint_v1_tag(const id3v1_tag &tag, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  string version("1");
  if (tag.v1_1()) {
    version += ".1";
  }
  if (tag.enhanced()) {
    version += "-enh";
  }

  os << R"({"version":")" << version << R"(","album":")" <<
    tag.album<string>(v1enc_, dst, rsp) <<
    R"(","artist":")" << escape(tag.artist<string>(v1enc_, dst, rsp)) <<
    R"(","comment":")" << escape(tag.comment<string>(v1enc_, dst, rsp)) <<
    R"(","genre":)" << setbase(10) << (int)tag.genre() <<
    R"(,"title":")" << escape(tag.title<string>(v1enc_, dst, rsp)) <<
    R"(","year":")" << escape(tag.year<string>(v1enc_, dst, rsp)) <<
    R"(")";

  bool has_track;
  unsigned char track;
  tie(has_track, track) = tag.track_number();
  if (has_track) {
    os << R"(,"track":)" << setbase(10) << (int)track;
  }

  if (tag.enhanced()) {
    os << R"(,"enhanced-genre":")" <<
      escape(tag.enh_genre<string>(v1enc_, dst, rsp)) <<
      R"(")";
  }

  return os << R"(})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_unk_id3v2_2_frame(
  const unknown_id3v2_2_frame &frm,
  std::ostream &os)
{
  using namespace std;

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_id3v2_2_text_frame(const id3v2_2_text_frame &frm,
                                                  std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"text":")" << escape(frm.as_str<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_UFI(const UFI &frm,
                                   std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":)" << escape(frm.owner<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_TXX(const TXX &frm,
                                   std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_COM(const COM &frm,
                                   std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_CNT(const CNT &cnt,
                                   std::ostream &os)
{
  using namespace std;

  return os << R"({"id":"CNT,"size":)" << setbase(10) << cnt.size()
            << R"(,"count":)" << cnt.count() << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_POP(const POP &frm,
                                   std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::UTF_8;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"(","rating":)" << (unsigned int)frm.rating()
            << R"(,"count":)" << frm.count()
            << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_XTG(const XTG &frm,
                                   std::ostream &os)
{
  using namespace std;
  using boost::algorithm::join;

  vector<string> tags;
  transform(frm.begin(), frm.end(), back_inserter(tags),
            [](const pair<const string, set<string>> &pr) {
              auto first = escape(pr.first);
              switch (pr.second.size()) {
              case 0:
                return "\"" + first + "\"";
              case 1:
                return "{\"" + first + R"(":")" + escape(*pr.second.begin()) +
                  "\"}";
              default:
                return "{\"" + first + R"(":[)" + join(pr.second, ",") + "]}";
               }
            });

  return os << R"({"id":")" << frm.id() << R"(",size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":")" << escape(frm.owner())
            << R"(","cloud":"[)" << join(tags, ",")
            << R"(]})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_unk_id3v2_3_frame(
  const unknown_id3v2_3_frame &frm,
  std::ostream &os)
{
  using namespace std;

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_id3v2_3_text_frame(const id3v2_3_text_frame &frm,
                                                  std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"text":")" << escape(frm.as_str<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_UFID(const UFID &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":)" << escape(frm.owner<string>(dst, rsp, src))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_ENCR(const ENCR &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)" << setbase(10)
            << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_TXXX(const TXXX &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_COMM(const COMM &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream &
scribbu::json_pprinter::pprint_PCNT(const PCNT &pcnt, std::ostream &os)
{
  using namespace std;

  return os << R"({"id":"PCNT,"size":)" << setbase(10) << pcnt.size()
            << R"(,"count":)" << pcnt.count() << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_POPM(const POPM &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"(","rating":)" << (unsigned int)frm.rating()
            << R"(,"count":)" << frm.count()
            << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_XTAG(const XTAG &frm,
                                    std::ostream &os)
{
  using namespace std;
  using boost::algorithm::join;

  vector<string> tags;
  transform(frm.begin(), frm.end(), back_inserter(tags),
            [](const pair<const string, set<string>> &pr) {
              auto first = escape(pr.first);
              switch (pr.second.size()) {
              case 0:
                return "\"" + first + "\"";
              case 1:
                return "{\"" + first + R"(":")" + escape(*pr.second.begin()) +
                  "\"}";
              default:
                return "{\"" + first + R"(":[)" + join(pr.second, ",") + "]}";
               }
            });

  return os << R"({"id":")" << frm.id() << R"(",size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":")" << escape(frm.owner())
            << R"(","cloud":"[)" << join(tags, ",")
            << R"(]})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_PRIV(const PRIV &frm,
                                    std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)" << setbase(10)
            << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_unk_id3v2_4_frame(
  const unknown_id3v2_4_frame &frm,
  std::ostream &os)
{
  using namespace std;

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_id3v2_4_text_frame(const id3v2_4_text_frame &frm,
                                                  std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"text":")" << escape(frm.as_str<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_UFID_2_4(const UFID_2_4 &frm, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":)" << escape(frm.owner<string>(dst, rsp, src))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_ENCR_2_4(const ENCR_2_4 &frm,
                                        std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)" << setbase(10)
            << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_TXXX_2_4(const TXXX_2_4 &frm, std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_COMM_2_4(const COMM_2_4 &frm,
                                        std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"description":")"
            << escape(frm.description<string>(dst, rsp, v2enc_))
            << R"(","text":")"
            << escape(frm.text<string>(dst, rsp, v2enc_))
            << R"("})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_PCNT_2_4(const PCNT_2_4 &pcnt, std::ostream &os)
{
  using namespace std;

  return os << R"({"id":"PCNT,"size":)" << setbase(10) << pcnt.size()
            << R"(,"count":)" << pcnt.count() << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_POPM_2_4(const POPM_2_4 &frm,
                                        std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"(","rating":)" << (unsigned int)frm.rating()
            << R"(,"count":)" << frm.count()
            << "}";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_XTAG_2_4(const XTAG_2_4 &frm,
                                        std::ostream &os)
{
  using namespace std;
  using boost::algorithm::join;

  vector<string> tags;
  transform(frm.begin(), frm.end(), back_inserter(tags),
            [](const pair<const string, set<string>> &pr) {
              auto first = escape(pr.first);
              switch (pr.second.size()) {
              case 0:
                return "\"" + first + "\"";
              case 1:
                return "{\"" + first + R"(":")" + escape(*pr.second.begin()) +
                  "\"}";
              default:
                return "{\"" + first + R"(":[)" + join(pr.second, ",") + "]}";
               }
            });

  return os << R"({"id":")" << frm.id() << R"(",size":)"
            << setbase(10) << (int)frm.size()
            << R"(,"owner":")" << escape(frm.owner())
            << R"(","cloud":"[)" << join(tags, ",")
            << R"(]})";
}

/*virtual*/ std::ostream&
scribbu::json_pprinter::pprint_PRIV_2_4(const PRIV_2_4 &frm,
                                        std::ostream &os)
{
  using namespace std;

  encoding dst;
  on_no_encoding rsp;
  tie(dst, rsp) = encoding_from_stream(os);

  encoding src = encoding::ISO_8859_1;
  if (v2enc_) {
    src = *v2enc_;
  }

  return os << R"({"id":")" << frm.id() << R"(","size":)" << setbase(10)
            << (int)frm.size()
            << R"(,"email":")"
            << escape(frm.email<string>(dst, rsp, src))
            << R"("})";
}
