/**
 * \file framesv2.cc
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
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

#include "framesv2.hh"

#include <unordered_map>

#include <boost/functional/hash.hpp>
#include <boost/static_assert.hpp>

#include <arpa/inet.h>
#include <iconv.h>


bool
scribbu::detail::is_false_sync(unsigned char x, unsigned char y)
{
  return 255 == x && 233 < y;
}

bool
scribbu::detail::needs_unsync(unsigned char x, unsigned char y)
{
  return 255 == x && (233 < y || 0 == y);
}

std::size_t
scribbu::detail::count_syncs(std::uint32_t n, bool false_only)
{
  using namespace std;

  size_t count = 0;
  const unsigned char *p = (const unsigned char*)&n;
  for (ptrdiff_t i = 0; i < 3; ++i) {
    if (false_only) {
      if (is_false_sync(p[i], p[i+1])) {
        ++count;
      }
    } else {
      if (needs_unsync(p[i], p[i+1])) {
        ++count;
      }
    }
  }
  
  return count;
}

///////////////////////////////////////////////////////////////////////////////
//                           enum id3v2_text_frames                          //
///////////////////////////////////////////////////////////////////////////////

std::istream&
scribbu::operator>>(std::istream &is, id3v2_text_frames &x)
{
  using namespace std;
  
  static const unordered_map<string, id3v2_text_frames, hash<string>> TBL{
    { "talb",         id3v2_text_frames::talb },
    { "TALB",         id3v2_text_frames::talb },
    { "tal",          id3v2_text_frames::talb },
    { "TAL",          id3v2_text_frames::talb },
    { "album",        id3v2_text_frames::talb },
    { "tbpm",         id3v2_text_frames::tbpm },
    { "TBPM",         id3v2_text_frames::tbpm },
    { "tbm",          id3v2_text_frames::tbpm },
    { "TBM",          id3v2_text_frames::tbpm },
    { "tcom",         id3v2_text_frames::tcom },
    { "TCOM",         id3v2_text_frames::tcom },
    { "tcm",          id3v2_text_frames::tcom },
    { "TCM",          id3v2_text_frames::tcom },
    { "composer",     id3v2_text_frames::tcom },
    { "tcon",         id3v2_text_frames::tcon },
    { "TCON",         id3v2_text_frames::tcon },
    { "tco",          id3v2_text_frames::tcon },
    { "TCO",          id3v2_text_frames::tcon },
    { "content-type", id3v2_text_frames::tcon },
    { "genre",        id3v2_text_frames::tcon },
    { "tcop",         id3v2_text_frames::tcop },
    { "TCOP",         id3v2_text_frames::tcop },
    { "tcr",          id3v2_text_frames::tcop },
    { "TCR",          id3v2_text_frames::tcop },
    { "tdat",         id3v2_text_frames::tdat },
    { "TDAT",         id3v2_text_frames::tdat },
    { "tda",          id3v2_text_frames::tdat },
    { "TDA",          id3v2_text_frames::tdat },
    { "date",         id3v2_text_frames::tdat },
    { "tdly",         id3v2_text_frames::tdly },
    { "TDLY",         id3v2_text_frames::tdly },
    { "tdy",          id3v2_text_frames::tdly },
    { "TDY",          id3v2_text_frames::tdly },
    { "tenc",         id3v2_text_frames::tenc },
    { "TENC",         id3v2_text_frames::tenc },
    { "ten",          id3v2_text_frames::tenc },
    { "TEN",          id3v2_text_frames::tenc },
    { "encoded-by",   id3v2_text_frames::tenc },
    { "text",         id3v2_text_frames::text },
    { "TEXT",         id3v2_text_frames::text },
    { "txt",          id3v2_text_frames::text },
    { "TXT",          id3v2_text_frames::text },
    { "tflt",         id3v2_text_frames::tflt },
    { "TFLT",         id3v2_text_frames::tflt },
    { "tft",          id3v2_text_frames::tflt },
    { "TFT",          id3v2_text_frames::tflt },
    { "time",         id3v2_text_frames::time },
    { "TIME",         id3v2_text_frames::time },
    { "tim",          id3v2_text_frames::time },
    { "TIM",          id3v2_text_frames::time },
    { "time",         id3v2_text_frames::time },
    { "tt1",          id3v2_text_frames::tit1 },
    { "TT1",          id3v2_text_frames::tit1 },
    { "tit1",         id3v2_text_frames::tit1 },
    { "TIT1",         id3v2_text_frames::tit1 },
    { "tit2",         id3v2_text_frames::tit2 },
    { "TIT2",         id3v2_text_frames::tit2 },
    { "tt2",          id3v2_text_frames::tit2 },
    { "TT2",          id3v2_text_frames::tit2 },
    { "title",        id3v2_text_frames::tit2 },
    { "tit3",         id3v2_text_frames::tit3 },
    { "TIT3",         id3v2_text_frames::tit3 },
    { "tt3",          id3v2_text_frames::tit3 },
    { "TT3",          id3v2_text_frames::tit3 },
    { "sub-title",    id3v2_text_frames::tit3 },
    { "tkey",         id3v2_text_frames::tkey },
    { "TKEY",         id3v2_text_frames::tkey },
    { "tke",          id3v2_text_frames::tkey },
    { "TKE",          id3v2_text_frames::tkey },
    { "tlan",         id3v2_text_frames::tlan },
    { "TLAN",         id3v2_text_frames::tlan },
    { "tla",          id3v2_text_frames::tlan },
    { "TLA",          id3v2_text_frames::tlan },
    { "languages",    id3v2_text_frames::tlan },
    { "tlen",         id3v2_text_frames::tlen },
    { "TLEN",         id3v2_text_frames::tlen },
    { "tle",          id3v2_text_frames::tlen },
    { "TLE",          id3v2_text_frames::tlen },
    { "length",       id3v2_text_frames::tlen },
    { "tmed",         id3v2_text_frames::tmed },
    { "TMED",         id3v2_text_frames::tmed },
    { "tmt",          id3v2_text_frames::tmed },
    { "TMT",          id3v2_text_frames::tmed },
    { "toal",         id3v2_text_frames::toal },
    { "TOAL",         id3v2_text_frames::toal },
    { "tot",          id3v2_text_frames::toal },
    { "TOT",          id3v2_text_frames::toal },
    { "tofn",         id3v2_text_frames::tofn },
    { "TOFN",         id3v2_text_frames::tofn },
    { "tof",          id3v2_text_frames::tofn },
    { "TOF",          id3v2_text_frames::tofn },
    { "toly",         id3v2_text_frames::toly },
    { "TOLY",         id3v2_text_frames::toly },
    { "tol",          id3v2_text_frames::toly },
    { "TOL",          id3v2_text_frames::toly },
    { "tope",         id3v2_text_frames::tope },
    { "TOPE",         id3v2_text_frames::tope },
    { "toa",          id3v2_text_frames::tope },
    { "TOA",          id3v2_text_frames::tope },
    { "tory",         id3v2_text_frames::tory },
    { "TORY",         id3v2_text_frames::tory },
    { "tor",          id3v2_text_frames::tory },
    { "TOR",          id3v2_text_frames::tory },
    { "town",         id3v2_text_frames::town },
    { "TOWN",         id3v2_text_frames::town },
    { "tpe1",         id3v2_text_frames::tpe1 },
    { "TPE1",         id3v2_text_frames::tpe1 },
    { "tp1",          id3v2_text_frames::tpe1 },
    { "TP1",          id3v2_text_frames::tpe1 },
    { "artist",       id3v2_text_frames::tpe1 },
    { "tpe2",         id3v2_text_frames::tpe2 },
    { "TPE2",         id3v2_text_frames::tpe2 },
    { "tp2",          id3v2_text_frames::tpe2 },
    { "TP2",          id3v2_text_frames::tpe2 },
    { "tpe3",         id3v2_text_frames::tpe3 },
    { "TPE3",         id3v2_text_frames::tpe3 },
    { "tp3",          id3v2_text_frames::tpe3 },
    { "TP3",          id3v2_text_frames::tpe3 },
    { "tpe4",         id3v2_text_frames::tpe4 },
    { "TPE4",         id3v2_text_frames::tpe4 },
    { "tp4",          id3v2_text_frames::tpe4 },
    { "TP4",          id3v2_text_frames::tpe4 },
    { "tpos",         id3v2_text_frames::tpos },
    { "TPOS",         id3v2_text_frames::tpos },
    { "tpa",          id3v2_text_frames::tpos },
    { "TPA",          id3v2_text_frames::tpos },
    { "tpub",         id3v2_text_frames::tpub },
    { "TPUB",         id3v2_text_frames::tpub },
    { "tpb",          id3v2_text_frames::tpub },
    { "TPB",          id3v2_text_frames::tpub },
    { "trck",         id3v2_text_frames::trck },
    { "TRCK",         id3v2_text_frames::trck },
    { "trk",          id3v2_text_frames::trck },
    { "TRK",          id3v2_text_frames::trck },
    { "track",        id3v2_text_frames::trck },
    { "trda",         id3v2_text_frames::trda },
    { "TRDA",         id3v2_text_frames::trda },
    { "trd",          id3v2_text_frames::trda },
    { "TRD",          id3v2_text_frames::trda },
    { "trsn",         id3v2_text_frames::trsn },
    { "TRSN",         id3v2_text_frames::trsn },
    { "trso",         id3v2_text_frames::trso },
    { "TRSO",         id3v2_text_frames::trso },
    { "tsiz",         id3v2_text_frames::tsiz },
    { "TSIZ",         id3v2_text_frames::tsiz },
    { "tsi",          id3v2_text_frames::tsiz },
    { "TSI",          id3v2_text_frames::tsiz },
    { "tsrc",         id3v2_text_frames::tsrc },
    { "TSRC",         id3v2_text_frames::tsrc },
    { "trc",          id3v2_text_frames::tsrc },
    { "TRC",          id3v2_text_frames::tsrc },
    { "tsse",         id3v2_text_frames::tsse },
    { "TSSE",         id3v2_text_frames::tsse },
    { "tss",          id3v2_text_frames::tsse },
    { "TSS",          id3v2_text_frames::tsse },
    { "tyer",         id3v2_text_frames::tyer },
    { "TYER",         id3v2_text_frames::tyer },
    { "tye",          id3v2_text_frames::tyer },
    { "TYE",          id3v2_text_frames::tyer },
    { "year",         id3v2_text_frames::tyer },
  };
    
  string text;
  is >> text;

  x = TBL.at(text);

  return is;
}

std::ostream&
scribbu::operator<<(std::ostream &os, const id3v2_text_frames &x)
{
  using namespace std;
  
  static 
  const unordered_map<id3v2_text_frames, string, hash<id3v2_text_frames>> TBL{
    { id3v2_text_frames::talb, "talb" },
    { id3v2_text_frames::tbpm, "tbpm" },
    { id3v2_text_frames::tcom, "tcom" },
    { id3v2_text_frames::tcon, "tcon" },
    { id3v2_text_frames::tcop, "tcop" },
    { id3v2_text_frames::tdat, "tdat" },
    { id3v2_text_frames::tdly, "tdly" },
    { id3v2_text_frames::tenc, "tenc" },
    { id3v2_text_frames::text, "text" },
    { id3v2_text_frames::tflt, "tflt" },
    { id3v2_text_frames::time, "time" },
    { id3v2_text_frames::tit1, "tit1" },
    { id3v2_text_frames::tit2, "tit2" },
    { id3v2_text_frames::tit3, "tit3" },
    { id3v2_text_frames::tkey, "tkey" },
    { id3v2_text_frames::tlan, "tlan" },
    { id3v2_text_frames::tlen, "tlen" },
    { id3v2_text_frames::tmed, "tmed" },
    { id3v2_text_frames::toal, "toal" },
    { id3v2_text_frames::tofn, "tofn" },
    { id3v2_text_frames::toly, "toly" },
    { id3v2_text_frames::tope, "tope" },
    { id3v2_text_frames::tory, "tory" },
    { id3v2_text_frames::town, "town" },
    { id3v2_text_frames::tpe1, "tpe1" },
    { id3v2_text_frames::tpe2, "tpe2" },
    { id3v2_text_frames::tpe3, "tpe3" },
    { id3v2_text_frames::tpe4, "tpe4" },
    { id3v2_text_frames::tpos, "tpos" },
    { id3v2_text_frames::tpub, "tpub" },
    { id3v2_text_frames::trck, "trck" },
    { id3v2_text_frames::trda, "trda" },
    { id3v2_text_frames::trsn, "trsn" },
    { id3v2_text_frames::trso, "trso" },
    { id3v2_text_frames::tsiz, "tsiz" },
    { id3v2_text_frames::tsrc, "tsrc" },
    { id3v2_text_frames::tsse, "tsse" },
    { id3v2_text_frames::tyer, "tyer" },
  };

  return os << TBL.at(x);
}

///////////////////////////////////////////////////////////////////////////////
//                              class frame_id3                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::frame_id3::frame_id3(unsigned char id0,
                              unsigned char id1,
                              unsigned char id2):
  experimental_('X' == id0 || 'Y' == id0 || 'Z' == id0)
{ id_[0] = id0; id_[1] = id1; id_[2] = id2; }

scribbu::frame_id3::frame_id3(const unsigned char id[3]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; }

scribbu::frame_id3::frame_id3(const char id[3]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; }

scribbu::frame_id3::frame_id3(id3v2_text_frames x) :
  experimental_(false)
{
  id_[0] = 'T';
  switch (x) {
  case id3v2_text_frames::talb:
    id_[1] = 'A'; id_[2] = 'L'; break;
  case id3v2_text_frames::tbpm:
    id_[1] = 'B'; id_[2] = 'M'; break;
  case id3v2_text_frames::tcom:
    id_[1] = 'C'; id_[2] = 'M'; break;
  case id3v2_text_frames::tcon:
    id_[1] = 'C'; id_[2] = 'O'; break;
  case id3v2_text_frames::tcop:
    id_[1] = 'C'; id_[2] = 'R'; break;
  case id3v2_text_frames::tdat:
    id_[1] = 'D'; id_[2] = 'A'; break;
  case id3v2_text_frames::tdly:
    id_[1] = 'D'; id_[2] = 'Y'; break;
  case id3v2_text_frames::tenc:
    id_[1] = 'E'; id_[2] = 'N'; break;
  case id3v2_text_frames::text:
    id_[1] = 'X'; id_[2] = 'T'; break;
  case id3v2_text_frames::tflt:
    id_[1] = 'F'; id_[2] = 'T'; break;
  case id3v2_text_frames::time:
    id_[1] = 'I'; id_[2] = 'M'; break;
  case id3v2_text_frames::tit1:
    id_[1] = 'T'; id_[2] = '1'; break;
  case id3v2_text_frames::tit2:
    id_[1] = 'T'; id_[2] = '2'; break;
  case id3v2_text_frames::tit3:
    id_[1] = 'T'; id_[2] = '3'; break;
  case id3v2_text_frames::tkey:
    id_[1] = 'K'; id_[2] = 'E'; break;
  case id3v2_text_frames::tlan:
    id_[1] = 'L'; id_[2] = 'A'; break;
  case id3v2_text_frames::tlen:
    id_[1] = 'L'; id_[2] = 'E'; break;
  case id3v2_text_frames::tofn:
    id_[1] = 'O'; id_[2] = 'F'; break;
  case id3v2_text_frames::tpe1:
    id_[1] = 'P'; id_[2] = '1'; break;
  case id3v2_text_frames::tpe2:
    id_[1] = 'P'; id_[2] = '2'; break;
  case id3v2_text_frames::tpe3:
    id_[1] = 'P'; id_[2] = '3'; break;
  case id3v2_text_frames::tpe4:
    id_[1] = 'P'; id_[2] = '4'; break;
  case id3v2_text_frames::tpos:
    id_[1] = 'P'; id_[2] = 'A'; break;
  case id3v2_text_frames::tpub:
    id_[1] = 'P'; id_[2] = 'B'; break;
  case id3v2_text_frames::trck:
    id_[1] = 'R'; id_[2] = 'K'; break;
  case id3v2_text_frames::trda:
    id_[1] = 'R'; id_[2] = 'D'; break;
  case id3v2_text_frames::tsiz:
    id_[1] = 'S'; id_[2] = 'I'; break;
  case id3v2_text_frames::tsrc:
    id_[1] = 'R'; id_[2] = 'C'; break;
  case id3v2_text_frames::tsse:
    id_[1] = 'S'; id_[2] = 'S'; break;
  case id3v2_text_frames::tyer:
    id_[1] = 'Y'; id_[2] = 'E'; break;
  default:
    // TODO(sp1ff): throw something more specific
    throw std::logic_error("");
  }
}

bool scribbu::operator==(const frame_id3 &lhs,
                         const frame_id3 &rhs)
{
  unsigned char id_lhs[3], id_rhs[3];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] && id_lhs[2] == id_rhs[2];
}

std::ostream& scribbu::operator<<(std::ostream &os, const scribbu::frame_id3 &x) {
  return os << x.as_string();
}

//template <>
std::size_t std::hash<scribbu::frame_id3>::operator()(const scribbu::frame_id3 &x) const
{
  unsigned char id[3];
  x.copy(id);

  std::size_t seed = 0;
  boost::hash_combine(seed, id[0]);
  boost::hash_combine(seed, id[1]);
  boost::hash_combine(seed, id[2]);

  return seed;
}


///////////////////////////////////////////////////////////////////////////////
//                              class frame_id4                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::frame_id4::frame_id4(unsigned char id0,
                              unsigned char id1,
                              unsigned char id2,
                              unsigned char id3):
  experimental_('X' == id0 || 'Y' == id0 || 'Z' == id0)
{ id_[0] = id0; id_[1] = id1; id_[2] = id2; id_[3] = id3; }

scribbu::frame_id4::frame_id4(const unsigned char id[4]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; id_[3] = id[3]; }

scribbu::frame_id4::frame_id4(const char id[4]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; id_[3] = id[3]; }

scribbu::frame_id4::frame_id4(id3v2_text_frames x):
  experimental_(false)
{
  id_[0] = 'T';
  switch (x) {
  case id3v2_text_frames::talb:
    id_[1] = 'A'; id_[2] = 'L'; id_[3] = 'B';
    break;
  case id3v2_text_frames::tbpm:
    id_[1] = 'B'; id_[2] = 'P'; id_[3] = 'M';
    break;
  case id3v2_text_frames::tcom:
    id_[1] = 'C'; id_[2] = 'O'; id_[3] = 'M';
    break;
  case id3v2_text_frames::tcon:
    id_[1] = 'C'; id_[2] = 'O'; id_[3] = 'N';
    break;
  case id3v2_text_frames::tcop:
    id_[1] = 'C'; id_[2] = 'O'; id_[3] = 'P';
    break;
  case id3v2_text_frames::tdat:
    id_[1] = 'D'; id_[2] = 'A'; id_[3] = 'T';
    break;
  case id3v2_text_frames::tdly:
    id_[1] = 'D'; id_[2] = 'L'; id_[3] = 'Y';
    break;
  case id3v2_text_frames::tenc:
    id_[1] = 'E'; id_[2] = 'N'; id_[3] = 'C';
    break;
  case id3v2_text_frames::text:
    id_[1] = 'E'; id_[2] = 'X'; id_[3] = 'T';
    break;
  case id3v2_text_frames::tflt:
    id_[1] = 'F'; id_[2] = 'L'; id_[3] = 'T';
    break;
  case id3v2_text_frames::time:
    id_[1] = 'I'; id_[2] = 'M'; id_[3] = 'E';
    break;
  case id3v2_text_frames::tit1:
    id_[1] = 'I'; id_[2] = 'T'; id_[3] = '1';
    break;
  case id3v2_text_frames::tit2:
    id_[1] = 'I'; id_[2] = 'T'; id_[3] = '2';
    break;
  case id3v2_text_frames::tit3:
    id_[1] = 'I'; id_[2] = 'T'; id_[3] = '3';
    break;
  case id3v2_text_frames::tkey:
    id_[1] = 'K'; id_[2] = 'E'; id_[3] = 'Y';
    break;
  case id3v2_text_frames::tlan:
    id_[1] = 'L'; id_[2] = 'A'; id_[3] = 'N';
    break;
  case id3v2_text_frames::tlen:
    id_[1] = 'L'; id_[2] = 'E'; id_[3] = 'N';
    break;
  case id3v2_text_frames::tmed:
    id_[1] = 'M'; id_[2] = 'E'; id_[3] = 'D';
    break;
  case id3v2_text_frames::toal:
    id_[1] = 'O'; id_[2] = 'A'; id_[3] = 'L';
    break;
  case id3v2_text_frames::tofn:
    id_[1] = 'O'; id_[2] = 'F'; id_[3] = 'N';
    break;
  case id3v2_text_frames::toly:
    id_[1] = 'O'; id_[2] = 'L'; id_[3] = 'Y';
    break;
  case id3v2_text_frames::tope:
    id_[1] = 'O'; id_[2] = 'P'; id_[3] = 'E';
    break;
  case id3v2_text_frames::tory:
    id_[1] = 'O'; id_[2] = 'R'; id_[3] = 'Y';
    break;
  case id3v2_text_frames::town:
    id_[1] = 'O'; id_[2] = 'W'; id_[3] = 'N';
    break;
  case id3v2_text_frames::tpe1:
    id_[1] = 'P'; id_[2] = 'E'; id_[3] = '1';
    break;
  case id3v2_text_frames::tpe2:
    id_[1] = 'P'; id_[2] = 'E'; id_[3] = '2';
    break;
  case id3v2_text_frames::tpe3:
    id_[1] = 'P'; id_[2] = 'E'; id_[3] = '3';
    break;
  case id3v2_text_frames::tpe4:
    id_[1] = 'P'; id_[2] = 'E'; id_[3] = '4';
    break;
  case id3v2_text_frames::tpos:
    id_[1] = 'P'; id_[2] = 'O'; id_[3] = 'S';
    break;
  case id3v2_text_frames::tpub:
    id_[1] = 'P'; id_[2] = 'U'; id_[3] = 'B';
    break;
  case id3v2_text_frames::trck:
    id_[1] = 'R'; id_[2] = 'C'; id_[3] = 'K';
    break;
  case id3v2_text_frames::trda:
    id_[1] = 'R'; id_[2] = 'D'; id_[3] = 'A';
    break;
  case id3v2_text_frames::trsn:
    id_[1] = 'R'; id_[2] = 'S'; id_[3] = 'N';
    break;
  case id3v2_text_frames::trso:
    id_[1] = 'R'; id_[2] = 'S'; id_[3] = 'O';
    break;
  case id3v2_text_frames::tsiz:
    id_[1] = 'S'; id_[2] = 'I'; id_[3] = 'Z';
    break;
  case id3v2_text_frames::tsrc:
    id_[1] = 'S'; id_[2] = 'R'; id_[3] = 'C';
    break;
  case id3v2_text_frames::tsse:
    id_[1] = 'S'; id_[2] = 'S'; id_[3] = 'E';
    break;
  case id3v2_text_frames::tyer:
    id_[1] = 'Y'; id_[2] = 'E'; id_[3] = 'R';
    break;
  default:
    // TODO(sp1ff): throw something more specific
    throw std::logic_error("");

  }
}

bool scribbu::operator==(const frame_id4 &lhs,
                         const frame_id4 &rhs)
{
  unsigned char id_lhs[4], id_rhs[4];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] &&
         id_lhs[2] == id_rhs[2] && id_lhs[3] == id_rhs[3];
}

std::ostream& scribbu::operator<<(std::ostream &os, const scribbu::frame_id4 &x) {
  return os << x.as_string();
}

std::size_t std::hash<scribbu::frame_id4>::operator()(const scribbu::frame_id4 &x) const
{
  unsigned char id[4];
  x.copy(id);

  std::size_t seed = 0;
  boost::hash_combine(seed, id[0]);
  boost::hash_combine(seed, id[1]);
  boost::hash_combine(seed, id[2]);
  boost::hash_combine(seed, id[3]);

  return seed;
}


///////////////////////////////////////////////////////////////////////////////
//                           class unique_file_id                            //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::unique_file_id::size() const
{
  return owner_.size() + 1 + id_.size(); // Need to add the trailing NULL
}

std::size_t
scribbu::unique_file_id::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::unique_file_id::needs_unsynchronisation() const
{
  return count_syncs(true);
}

std::size_t
scribbu::unique_file_id::write(std::ostream &os) const
{
  const char zed = 0;

  os.write((const char*)&(owner_[0]), owner_.size());
  os.write(&zed, 1);
  os.write((const char*)&(id_[0]), id_.size());
  return owner_.size() + 1 + id_.size();
}

std::size_t
scribbu::unique_file_id::count_syncs(bool false_only) const
{
  using namespace std;
  using namespace scribbu::detail;

  size_t n = detail::count_syncs(owner_.begin(), owner_.end(), false_only);
  // owner is null-terminated, so possibility of a false sync
  n += detail::count_syncs(id_.begin(), id_.end(), false_only);
  return n;
}


///////////////////////////////////////////////////////////////////////////////
//                          class encryption_method                          //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::encryption_method::size() const
{
  // Need to add the trailing NULL on email_ + the method
  return email_.size() + 1 + 1 + data_.size();
}

std::size_t
scribbu::encryption_method::serialized_size(bool unsync) const
{
  // Need to add the trailing NULL on email_ + the method
  std::size_t cb = email_.size() + 1 + 1 + data_.size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::encryption_method::needs_unsynchronisation() const
{
  return count_syncs(true);
}

std::size_t
scribbu::encryption_method::write(std::ostream &os) const
{
  const char zed = 0;

  os.write((const char*)&(email_[0]), email_.size());
  os.write((const char*)&method_symbol_, 1);
  os.write((const char*)&(data_[0]), data_.size());
  return email_.size() + 1 + data_.size();

}

std::size_t
scribbu::encryption_method::count_syncs(bool false_only) const
{
  using namespace std;
  using namespace scribbu::detail;

  size_t count = detail::count_syncs(email_.begin(), email_.end(), false_only);

  if (!email_.empty()) {
    if (false_only && is_false_sync(email_.back(), method_symbol_)) {
      ++count;
    }
    else if (!false_only && needs_unsync(email_.back(), method_symbol_)) {
      ++count;
    }
  }
  // email_ is null-terminated, so no possibility of a false sync...
  // but there could be one between method_symbol_ & data_:
  if (!data_.empty()) {
    if (false_only && is_false_sync(method_symbol_, data_.front())) {
      ++count;
    }
    else if (!false_only && is_false_sync(method_symbol_, data_.front())) {
      ++count;
    }
  }
  count += detail::count_syncs(data_.begin(), data_.end(), false_only);
  return count;
}


///////////////////////////////////////////////////////////////////////////////
//                          class user_defined_text                          //
///////////////////////////////////////////////////////////////////////////////

scribbu::user_defined_text::user_defined_text(id3v2_version ver,
                                              const std::string &text,
                                              encoding src,
                                              use_unicode unicode,
                                              const std::string &dsc /*= std::string()*/)
{
  encoding dst;
  bool add_bom;
  if (id3v2_version::v2 == ver || id3v2_version::v3 == ver) {
    switch (unicode) {
    case use_unicode::no:
      unicode_ = 0;
      dst = encoding::ISO_8859_1;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::yes:
      unicode_ = 1;
      dst = encoding::UCS_2;
      add_bom = false;
      cbnil_ = 2;
      break;
    case use_unicode::with_bom:
      unicode_ = 1;
      dst = encoding::UCS_2;
      add_bom = true;
      cbnil_ = 2;
      break;
    }
  }
  else {
    switch (unicode) {
    case use_unicode::no:
      unicode_ = 0;
      dst = encoding::ISO_8859_1;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::yes:
      unicode_ = 4;
      dst = encoding::UTF_8;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::with_bom:
      unicode_ = 4;
      dst = encoding::UTF_8;
      add_bom = true;
      cbnil_ = 1;
      break;
    }
  }

  text_ = convert_encoding(text, src, dst, add_bom);
  if (!dsc.empty()) {
    description_ = convert_encoding(dsc, src, dst, add_bom);
  }

}

std::size_t
scribbu::user_defined_text::size() const
{
  return 1 + description_.size() + cbnil_ + text_.size();
}

std::size_t
scribbu::user_defined_text::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::user_defined_text::needs_unsynchronisation() const
{
  return count_syncs(true);

}

std::size_t
scribbu::user_defined_text::write(std::ostream &os) const
{
  const char zed = 0;
  const char uzed[2] = { 0, 0 };

  std::size_t cb = 0;
  os.write((const char*)&unicode_, 1);
  cb += 1;
  os.write((const char*)&(description_[0]), description_.size());
  cb += description_.size();
  if (2 == cbnil_) {
    os.write(uzed, 2);
    cb += 2;
  }
  else {
    os.write(&zed, 1);
    cb += 1;
  }
  os.write((const char*)&(text_[0]), text_.size());
  cb += text_.size();

  return cb;
}

std::size_t
scribbu::user_defined_text::count_syncs(bool false_only) const
{
  using namespace scribbu::detail;

  std::size_t cb = 0;

  if (false_only && description_.size() &&
      is_false_sync(unicode_, description_.front())) {
    ++cb;
  }
  else if (!false_only && description_.size() &&
           needs_unsync(unicode_, description_.front())) {
    ++cb;
  }

  cb += detail::count_syncs(description_.begin(), description_.end(), false_only);
  cb += detail::count_syncs(text_.begin(), text_.end(), false_only);

  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                              class comments                               //
///////////////////////////////////////////////////////////////////////////////

scribbu::comments::comments(id3v2_version ver,
                            language lang,
                            const std::string &text,
                            encoding src,
                            use_unicode unicode,
                            const std::string &dsc /*= std::string()*/)
{
  if (language::from_locale == lang) {
    lang = language_from_locale();
  }

  language_to_iso_639_2(lang, lang_);

  encoding dst;
  bool add_bom;
  if (id3v2_version::v2 == ver || id3v2_version::v3 == ver) {
    switch (unicode) {
    case use_unicode::no:
      unicode_ = 0;
      dst = encoding::ISO_8859_1;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::yes:
      unicode_ = 1;
      dst = encoding::UCS_2;
      add_bom = false;
      cbnil_ = 2;
      break;
    case use_unicode::with_bom:
      unicode_ = 1;
      dst = encoding::UCS_2;
      add_bom = true;
      cbnil_ = 2;
      break;
    }
  }
  else {
    switch (unicode) {
    case use_unicode::no:
      unicode_ = 0;
      dst = encoding::ISO_8859_1;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::yes:
      unicode_ = 4;
      dst = encoding::UTF_8;
      add_bom = false;
      cbnil_ = 1;
      break;
    case use_unicode::with_bom:
      unicode_ = 4;
      dst = encoding::UTF_8;
      add_bom = true;
      cbnil_ = 1;
      break;
    }
  }

  text_ = convert_encoding(text, src, dst, add_bom);
  if (!dsc.empty()) {
    description_ = convert_encoding(dsc, src, dst, add_bom);
  }

}

std::size_t
scribbu::comments::size() const
{
  return 4 + description_.size() + cbnil_ + text_.size();
}

std::size_t
scribbu::comments::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::comments::needs_unsynchronisation() const
{
  return count_syncs(true);
}

std::size_t
scribbu::comments::write(std::ostream &os) const
{
  const char zed[2] = { 0, 0 };

  std::size_t cb = 0;

  os.write((const char*)&unicode_, 1);
  cb += 1;
  os.write((const char*)lang_, 3);
  cb += 3;
  os.write((const char*)&(description_[0]), description_.size());
  cb += description_.size();
  os.write(zed, cbnil_);
  cb += cbnil_;
  os.write((const char*)&(text_[0]), text_.size());
  cb += text_.size();

  return cb;
}

std::size_t
scribbu::comments::count_syncs(bool false_only) const
{
  using namespace scribbu::detail;

  std::size_t cb = 0;

  if (false_only && is_false_sync(unicode_, lang_[0])) {
    ++cb;
  }
  else if (!false_only && needs_unsync(unicode_, lang_[0])) {
    ++cb;
  }

  cb += detail::count_syncs(lang_, lang_ + 3, false_only);

  if (false_only && description_.size() &&
      is_false_sync(lang_[2], description_.front())) {
    ++cb;
  }
  else if (!false_only && description_.size() &&
           needs_unsync(lang_[2], description_.front())) {
    ++cb;
  }

  cb += detail::count_syncs(description_.begin(), description_.end(), false_only);

  if (false_only && description_.size() &&
      is_false_sync(description_.back(), text_.front())) {
    ++cb;
  }
  else if (!false_only && description_.size() &&
           needs_unsync(description_.back(), text_.front())) {
    ++cb;
  }

  cb += detail::count_syncs(text_.begin(), text_.end(), false_only);

  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                             class play_count                              //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::play_count::count() const
{
  uint32_t x;
  if (1 == counter_.size()) {
    x = counter_[0];
  }
  else if (2 == counter_.size()) {
   x = ( counter_[0] << 8 ) | counter_[1];
  }
  else if (3 == counter_.size()) {
    x = ( counter_[0] << 16 ) | ( counter_[1] << 8 ) | counter_[2];
  }
  else if (4 == counter_.size()) {
    x = ( counter_[0] << 24 ) | ( counter_[1] << 16 ) |
      ( counter_[2] << 8 )  | counter_[3];
  }
  else if (4 < counter_.size()) {
    throw std::domain_error("CNT overflow");
  }
  return x;
}

std::size_t
scribbu::play_count::size() const
{
  return counter_.size();
}

std::size_t
scribbu::play_count::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::play_count::needs_unsynchronisation() const
{
  return count_syncs(true);
}

std::size_t
scribbu::play_count::write(std::ostream &os) const
{
  os.write((const char*)&(counter_[0]), counter_.size());
  return counter_.size();
}

std::size_t
scribbu::play_count::count_syncs(bool false_only) const
{
  return detail::count_syncs(counter_.begin(), counter_.end(), false_only);
}

void
scribbu::play_count::reset_counter(std::size_t n)
{
  static const std::size_t FF = std::size_t( 0xff );

  counter_.erase(counter_.begin(), counter_.end());

  // Walk `n' from the MSB, looking for the first non-zero byte.
  std::size_t msb;
  for (msb = sizeof(size_t) - 1; msb > 0; --msb) {
    if ( (n & ( FF << (msb*8))) != 0 ) {
      break;
    }
  }
  
  while (msb > 0) {
    counter_.push_back( (0xff << msb) & n );
  }
  
  counter_.push_back( 0xff & n );  
}


///////////////////////////////////////////////////////////////////////////////
//                            class popularimeter                            //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::popularimeter::count() const
{
  uint32_t x;
  if (1 == counter_.size()) {
    x = counter_[0];
  }
  else if (2 == counter_.size()) {
   x = ( counter_[0] << 8 ) | counter_[1];
  }
  else if (3 == counter_.size()) {
    x = ( counter_[0] << 16 ) | ( counter_[1] << 8 ) | counter_[2];
  }
  else if (4 == counter_.size()) {
    x = ( counter_[0] << 24 ) | ( counter_[1] << 16 ) |
      ( counter_[2] << 8 )  | counter_[3];
  }
  else if (4 < counter_.size()) {
    throw std::domain_error("CNT overflow");
  }
  return x;
}

std::size_t
scribbu::popularimeter::size() const
{
  return email_.size() + 2 + counter_.size(); // Need to add the trailing NULL
}

std::size_t
scribbu::popularimeter::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

std::size_t
scribbu::popularimeter::needs_unsynchronisation() const
{
  return count_syncs(true);
}

std::size_t
scribbu::popularimeter::write(std::ostream &os) const
{
  static const char zed = 0;

  os.write((const char*)&(email_[0]), email_.size());
  os.write(&zed, 1);
  os.write((const char*)&rating_, 1);
  os.write((const char*)&(counter_[0]), counter_.size());
  return email_.size() + 2 + counter_.size();
}

std::size_t
scribbu::popularimeter::count_syncs(bool false_only) const
{
  std::size_t cb = 0;

  cb += detail::count_syncs(email_.begin(), email_.end(), false_only);
  if (false_only && counter_.size() &&
      detail::is_false_sync(rating_, counter_.front())) {
    ++cb;
  }
  else if (!false_only && counter_.size() &&
           detail::needs_unsync(rating_, counter_.front())) {
    ++cb;
  }

  cb += detail::count_syncs(counter_.begin(), counter_.end(), false_only);

  return cb;
}

void
scribbu::popularimeter::reset_counter(std::size_t n)
{
  static const std::size_t FF = std::size_t( 0xff );

  counter_.erase(counter_.begin(), counter_.end());

  // Walk `n' from the MSB, looking for the first non-zero byte.
  std::size_t msb;
  for (msb = sizeof(size_t) - 1; msb > 0; --msb) {
    if ( (n & ( FF << (msb*8))) != 0 ) {
      break;
    }
  }
  
  while (msb > 0) {
    counter_.push_back( (0xff << msb) & n );
  }
  
  counter_.push_back( 0xff & n );  
}

////////////////////////////////////////////////////////////////////////////
//                              class tag_cloud
////////////////////////////////////////////////////////////////////////////

/// Construct "from scratch"-- text shall be a query-string style representation
/// of the tag cloud (i.e. that which is returned from urlencoded())
scribbu::tag_cloud::tag_cloud(const std::string &owner, 
                              const std::string &text):
  own_(owner)
{
  parse_to_map(text, tags_);
}

/// Does a given value exist for a given key?
bool scribbu::tag_cloud::has_value(const std::string &key, 
                                   const std::string &val) const
{
  if (!tags_.count(key)) {
    return false;
  }
  return tags_.at(key).count(val);

}

/// Add key with no values; if the key exists this will be a NOP-- a return
/// value of false means the key already existed
bool scribbu::tag_cloud::add_key(const std::string &key)
{
  if (tags_.count(key)) {
    return false;
  }
  tags_[key] = std::set<std::string>();
  return true;
}

/// Add a value to an existent key-- a false return value means the key and
/// value already existed
bool scribbu::tag_cloud::add_value(const std::string &key, 
                                   const std::string &val)
{
  if (!tags_.count(key)) {
    tags_[key] = std::set<std::string>();
    tags_[key].insert(val);
    return true;
  }
  
  if (tags_[key].count(val)) {
    return false;
  }
  
  tags_[key].insert(val);
  return true;
}

/// Return an URL-encoded representation of the tag cloud
std::string 
scribbu::tag_cloud::urlencoded() const
{
  using namespace std;
  
  using scribbu::urlencode;
  
  string out;
  bool first = true;
  for (auto kv: tags_) {
    
    if (first) {
      first = false;
    } else {
      out += '&';
    }

    out += urlencode(kv.first);

    if (!kv.second.empty()) {
      
      out += '=';
      bool infirst = true;
      for (auto val: kv.second) {
        
        if (infirst) {
          infirst = false;
        } else {
          out += ',';
        }
        
        out += urlencode(val);        
      }
    }
  }
  
  return out;
}

/// Merge an URL-coded representation of some tags into the cloud
void 
scribbu::tag_cloud::merge(const std::string &tags)
{
  using namespace std;

  map_type M;
  parse_to_map(tags, M);
  for (auto p: M) {
    const string key = p.first;
    set<string> value = p.second;
    // tie(key, value) = p;
    
    if (tags_.count(key)) {
      tags_[key].insert(value.begin(), value.end());
    } else {
      tags_[key] = value;
    }
  }
}

/// Update the tag cloud
void 
scribbu::tag_cloud::update(const std::string &tags)
{
  tags_.clear();
  parse_to_map(tags, tags_);
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
std::size_t 
scribbu::tag_cloud::size() const
{
  std::size_t cb = 1 + own_.length() + 1;
  
  for (auto kv: tags_) {
    
    const std::string &key = kv.first;
    cb += key.length() + 1 + 4;
    for (auto v: kv.second) {
      cb += v.length() + 1;
    }
  }
  
  return cb;
}

/// Return the number of bytes this frame will occupy when serialized to
/// disk, including the header
std::size_t 
scribbu::tag_cloud::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_syncs(false);
  }
  return cb;
}

/// Return zero if this frame would not contain false syncs if serialized in
/// its present state; else return the number of false sync it would contain
std::size_t 
scribbu::tag_cloud::needs_unsynchronisation() const
{
  return count_syncs(true);
}

/// Serialize this frame to an output stream, perhaps applying the
/// unsynchronisation scheme if the caller so chooses ("unsynchronised" will
/// be updated accordingly)
std::size_t 
scribbu::tag_cloud::write(std::ostream &os) const
{
  const char ver[1] = { 0x01 };

  std::size_t cb  = 0;
  
  os.write(ver, 1); cb++;

  std::size_t cbw = own_.length() + 1;
  os.write(own_.c_str(), cbw); cb += cbw;
  
  for (auto kv: tags_) {

    // `kv' is a pair<const string, set<string>>
    const std::string &key = kv.first;
    cbw = key.length() + 1;
    os.write(key.c_str(), cbw); cb += cbw;
    
    uint32_t nval = 0;
    for (auto v: kv.second) {
      nval += v.length() + 1;
    }
    
    nval = htonl(nval);
    os.write((const char*)&nval, 4); cb += 4;
    
    for (auto v: kv.second) {
      cbw = v.length() + 1;
      os.write(v.c_str(), cbw); cb += cbw;
    }

  }
  
  return cb;
}

std::size_t 
scribbu::tag_cloud::count_syncs(bool false_only) const
{
  using namespace std;
  using namespace scribbu::detail;
  
  size_t nsync = 0;
  
  // The current version is 1, so no false sync.
  nsync += detail::count_syncs(own_.begin(), own_.end(), false_only);
  
  // Owner is null-terminated, so no false sync between owner & the first key.
  for (auto kv: tags_) {

    // `kv' is a pair<const string, set<string>>
    const string &key = kv.first;
    nsync += detail::count_syncs(key.begin(), key.end(), false_only);
    // Keys are null-terminated, so no false sync between owner & the first key.
    // Next up will be the combined length of all values (including trailing
    // nulls).
    uint32_t nval = 0;
    for (auto v: kv.second) {
      nval += v.length() + 1;
      nsync += detail::count_syncs(v.begin(), v.end(), false_only);
    }
    // `nval' itself may contain false syncs:
    nsync += detail::count_syncs(nval, false_only);
    
    // Now, if the last octet of `nval' happens to be 0xff _and_ false_only
    // is true, we _could_ have a false sync between the value byte count &
    // the first string.
    if (false_only) {
      unsigned char y = (unsigned char)(kv.second.begin()->front());
      if ((0xff == (0xff & nval)) && is_false_sync(0xff, y)) {
        ++nsync;
      }
    }
  }
  
  return nsync;
}

void 
scribbu::tag_cloud::parse_to_map(const std::string &text, 
                                 map_type &M)
{
  using namespace std;

  // Parse using a simple FSM:
  bool parsing_key = true;
  
  string token, key;
  for (size_t i = 0, n = text.size(); i < n; ++i) {

    char c = text[i];
    
    if (parsing_key) {
      if ('=' == c) {
        key = urldecode(token);
        token.clear();
        parsing_key = false;
      } else if ('&' == c) {
        token = urldecode(token);
        if (M.count(token)) {
          throw invalid_argument("duplicate tag " + token);
        }
        M[token] = set<string>();
        token.clear();
      } else {
        token += c;
      }
    } else {
      if (',' == c) {
        M[key].insert(urldecode(token));
        token.clear();
      } else if ('&' == c) {
        M[key].insert(urldecode(token));
        token.clear();
        key.clear();
        parsing_key = true;        
      } else {
        token += c;
      }
    }
    
  }
  
  // tidy up whatever's left in `token' & `key'
  if (parsing_key) {
    token = urldecode(token);
    if (M.count(token)) {
      throw invalid_argument("duplicate tag " + token);
    }
    M[token] = set<string>();
  } else {
    M[key].insert(urldecode(token));
  }
  
}



