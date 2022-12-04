/**
 * \file tbt-support.cc
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

#include <scribbu/tbt-support.hh>

#include <numeric>
#include <boost/algorithm/string.hpp>

#include <scribbu/winamp-genres.hh>


///////////////////////////////////////////////////////////////////////////////
//                    all_source_preference-related code                     //
///////////////////////////////////////////////////////////////////////////////

std::ostream&
scribbu::tbt_support::operator<<(std::ostream         &os,
                                 all_source_preference pref)
{
  switch (pref) {
  case all_source_preference::prefer_id3v2:
    os << "prefer ID3v2, fall back to ID3v1";
    break;
  case all_source_preference::prefer_id3v1:
    os << "prefer ID3v1, fall back to ID3v2";
    break;
  case all_source_preference::id3v2_only:
    os << "accept ID3v2 only";
    break;
  case all_source_preference::id3v1_only:
    os << "accept ID3v1 only";
    break;
  default:
    os << "all_source_preference " << (int) pref;
    break;
  }
  return os;
}

/*virtual*/
const char *
scribbu::tbt_support::missing_source_text::what() const noexcept
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << "Missing tag source (" << source_ << ")";
    if (const std::string *p = boost::get_error_info<tag_name_info>(*this)) {
      stm << " while evaluating tag '" << *p << "'";
    }
    pwhat_.reset(new std::string(stm.str()));
  }

  return pwhat_->c_str();
}


std::ostream&
scribbu::tbt_support::operator<<(std::ostream &os, size_opt opt)
{
  switch (opt) {
  case scribbu::tbt_support::size_opt::base:
    os << "base";
    break;
  case scribbu::tbt_support::size_opt::hex_case:
    os << "hex case";
    break;
  default:
    os << "size_opt " << (int) opt;
    break;
  }
  return os;
}

std::ostream&
scribbu::tbt_support::operator<<(std::ostream &os, ws_xform opt)
{
  switch (opt) {
  case scribbu::tbt_support::ws_xform::compress:
    os << "compress";
    break;
  case scribbu::tbt_support::ws_xform::replace:
    os << "replace";
    break;
  default:
    os << "ws_xform " << (int) opt;
    break;
  }
  return os;
}

std::ostream&
scribbu::tbt_support::operator<<(std::ostream &os, md5_opt opt)
{
  switch (opt) {
  case scribbu::tbt_support::md5_opt::base:
    os << "base";
    break;
  case scribbu::tbt_support::md5_opt::hex_case:
    os << "hex case";
    break;
  default:
    os << "md5 opt " << (int) opt;
    break;
  }
  return os;
}

std::ostream&
scribbu::tbt_support::operator<<(std::ostream &os, file_opt opt)
{
  switch (opt) {
  case scribbu::tbt_support::file_opt::the_xform:
    os << "the";
    break;
  case scribbu::tbt_support::file_opt::cap_xform:
    os << "cap";
    break;
  case scribbu::tbt_support::file_opt::ws_xform:
    os << "ws";
    break;
  default:
    os << "file_opt " << (int) opt;
    break;
  }
  return os;
}

std::ostream&
scribbu::tbt_support::operator<<(std::ostream &os, aacet_opt opt)
{
  switch (opt) {
  case scribbu::tbt_support::aacet_opt::all_source_preference:
    os << "(all)source prefs";
    break;
  case scribbu::tbt_support::aacet_opt::v1_encoding:
    os << "ID3v1 encoding";
    break;
  case scribbu::tbt_support::aacet_opt::the_xform:
    os << "the";
    break;
  case scribbu::tbt_support::aacet_opt::cap_xform:
    os << "cap";
    break;
  case scribbu::tbt_support::aacet_opt::ws_xforms:
    os << "ws";
    break;
  case scribbu::tbt_support::aacet_opt::output_encoding:
    os << "output encoding";
    break;
  default:
    os << "aacet_opt " << (int) opt;
    break;
  }
  return os;
}


std::string
scribbu::tbt_support::do_the_xform(const std::string &text,
                                   the_xform          the)
{
  size_t n = text.size();
  if (the_xform::make_prefix == the) {
    if (5 <= n && boost::iequals(", The", text.substr(n - 5))) {
      return "The " + text.substr(0, n - 5);
    }
  }
  else if (the_xform::make_suffix == the) {
    if (5 <= n && boost::iequals("The ", text.substr(0, 4))) {
      return text.substr(4) + ", The";
    }
  }
  return text;
}

std::string
scribbu::tbt_support::do_cap_xform(const std::string &text,
                                   capitalization     cap)
{
  using namespace std;
  using namespace boost;

  if (capitalization::capitalize == cap) {
    string result;
    result += toupper(text[0], locale(""));
    result += text.substr(1);
    return result;
  }
  else if (capitalization::all_upper == cap) {
    string buf(text);
    to_upper(buf);
    return buf;
  }
  else if (capitalization::all_lower == cap) {
    string buf(text);
    to_lower(buf);
    return buf;
  }

  return text;

}

std::string
scribbu::tbt_support::do_ws_xform(const std::string &text,
                                  bool               compress,
                                  const std::string &replace)
{
  using namespace std;
  using namespace boost;

  if (!compress && 0 == replace.length()) {
    return text;
  }

  vector<string> splits;
  split(splits, text, is_any_of(" \t"));
  if (0 != replace.length()) {
    return join(splits, replace);
  }
  else {
    return join(splits, " ");
  }

}

std::string
scribbu::tbt_support::encode(const std::string &text,
                             output_encoding    out)
{
  using std::string;

  // `text' is already in UTF-8; we just need to convert it to `out'. If `out'
  // is already UTF-8, then we're done:
  if (output_encoding::utf_8 == out) {
    return text;
  }

  encoding dst = encoding::ASCII;
  if (output_encoding::cp1252 == out) {
    dst = encoding::CP1252;
  }
  else if (output_encoding::iso8859_1 == out) {
    dst = encoding::ISO_8859_1;
  }

  return convert_encoding<string>((const unsigned char*)text.c_str(),
                                  text.size(), encoding::UTF_8, dst);
}


scribbu::tbt_support::duplicate_option::duplicate_option(scribbu::tbt_support::aacet_opt opt)
{
  std::stringstream stm;
  stm << "duplicate option " << opt;
  pwhat_.reset(new std::string(stm.str()));
}

scribbu::tbt_support::duplicate_option::duplicate_option(scribbu::tbt_support::file_opt opt)
{
  std::stringstream stm;
  stm << "duplicate option " << opt;
  pwhat_.reset(new std::string(stm.str()));
}

scribbu::tbt_support::duplicate_option::duplicate_option(scribbu::tbt_support::md5_opt opt)
{
  std::stringstream stm;
  stm << "duplicate option " << opt;
  pwhat_.reset(new std::string(stm.str()));
}

scribbu::tbt_support::duplicate_option::duplicate_option(scribbu::tbt_support::ws_xform opt)
{
  std::stringstream stm;
  stm << "duplicate option " << opt;
  pwhat_.reset(new std::string(stm.str()));
}

scribbu::tbt_support::duplicate_option::duplicate_option(scribbu::tbt_support::size_opt opt)
{
  std::stringstream stm;
  stm << "duplicate option " << opt;
  pwhat_.reset(new std::string(stm.str()));
}


///////////////////////////////////////////////////////////////////////////////
//                           class tab_based_term                            //
///////////////////////////////////////////////////////////////////////////////

std::string
scribbu::tbt_support::tag_based_term::v1_text_to_utf8(
  const unsigned char              *pbuf,
  std::size_t                       cbbuf,
  scribbu::tbt_support::v1_encoding v1enc) const
{
  using scribbu::encoding;
  using scribbu::convert_encoding;
  using scribbu::tbt_support::v1_encoding;

  const char * const ISO88591 = "ISO-8859-1";
  const char * const ASCII    = "ASCII";
  const char * const CP1252   = "CP1252";
  const char * const UTF8     = "UTF-8";
  const char * const UTF16BE  = "UCS-2BE";
  const char * const UTF16LE  = "UCS-2LE";
  const char * const UTF32    = "UTF-32";

  if (v1_encoding::automatic == v1enc) {
    if (3 <= cbbuf && 0xef == pbuf[0] &&
        0xbb == pbuf[1] && 0xbf == pbuf[2]) {
      v1enc = v1_encoding::utf_8;
    }
    else if (2 <= cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
      v1enc = v1_encoding::utf_16_be;
    }
    else if (2 <= cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
      v1enc = v1_encoding::utf_16_be;
    }
  }

  std::string result;
  if (v1_encoding::automatic == v1enc) {

    const std::vector<encoding> GUESSES({{
      encoding::ASCII, encoding::ISO_8859_1, encoding::CP1252,
      encoding::UTF_8, encoding::UTF_16BE, encoding::UTF_16LE,
      encoding::UTF_32
    }});

    for (auto g: GUESSES) {
      try {
        result = convert_encoding<std::string>(pbuf, cbbuf, g,
                                               encoding::UTF_8);
        break;
      } catch (const iconv_error&) {
        // Move on to the next guess...
      }
    }

    throw std::invalid_argument("unable to find encoding");

  }
  else {

    const std::map<v1_encoding, encoding> LOOKUP({
      {v1_encoding::ascii,     encoding::ASCII},
      {v1_encoding::iso8859_1, encoding::ISO_8859_1},
      {v1_encoding::cp1252,    encoding::CP1252},
      {v1_encoding::utf_8,     encoding::UTF_8},
      {v1_encoding::utf_16_be, encoding::UTF_16BE},
      {v1_encoding::utf_16_le, encoding::UTF_16LE},
      {v1_encoding::utf_32,    encoding::UTF_32}});

    encoding src = LOOKUP.at(v1enc);
    result = convert_encoding<std::string>(pbuf, cbbuf, src, encoding::UTF_8);

  }

  return result;

}


///////////////////////////////////////////////////////////////////////////////
//                              class subclause                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::tbt_support::
subclause::subclause(const std::vector<scribbu::tbt_support::term*> *pterms)
{
  for (auto p: *pterms) {
    terms_.push_back(std::shared_ptr<term>(p));
  }
}

/*virtual*/
std::string
scribbu::tbt_support::subclause::evaluate(const file_info  &fi,
                                          const id3v2_tag  *pid3v2,
                                          const track_data &ti,
                                          const id3v1_tag  *pid3v1) const
{
  std::string result;
  try {
    result = std::accumulate(terms_.begin(), terms_.end(), std::string(),
                             process_and_concatenate(fi, pid3v2, ti, pid3v1));
  }
  catch (const scribbu::tbt_support::error&) {
  }

  return result;

}


std::string
scribbu::tbt_support::aacet_term::xform_and_encode(const std::string &text) const
{
  std::string out = do_the_xform(text, the_);
  out = do_cap_xform(out, cap_);
  out = do_ws_xform(out, compress_ws_, replace_ws_);
  return encode(out, out_);
}



///////////////////////////////////////////////////////////////////////////////
//                              tag-based terms                              //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ std::string
scribbu::tbt_support::album::evaluate(const file_info  & /*fi    */,
                                      const id3v2_tag  *  pid3v2,
                                      const track_data & /*ti    */,
                                      const id3v1_tag  *  pid3v1) const
{
  using namespace std;

  vector<unsigned char> v1;
  if (pid3v1) {
    pid3v1->album(back_inserter(v1));
  }

  boost::optional<std::string> v2 = boost::none;
  if (pid3v2 && pid3v2->has_album()) {
    v2 = pid3v2->album();
  }

  std::string result;
  try {
    result = source_text(v1.begin(), v1.end(), v2);
    result = xform_and_encode(result);
  } catch (missing_source_text &ex) {
    throw ex << tag_name_info(std::string("album"));
  }

  return result;

}

/*virtual*/ std::string
scribbu::tbt_support::artist::evaluate(const file_info  & /*fi    */,
                                       const id3v2_tag  *  pid3v2,
                                       const track_data & /*ti    */,
                                       const id3v1_tag  *  pid3v1) const
{
  using namespace std;

  vector<unsigned char> v1;
  if (pid3v1) {
    pid3v1->artist(back_inserter(v1));
  }

  boost::optional<std::string> v2 = boost::none;
  if (pid3v2 && pid3v2->has_artist()) {
    v2 = pid3v2->artist();
  }

  std::string result;
  try {
    result = source_text(v1.begin(), v1.end(), v2);
    result = xform_and_encode(result);
  } catch (missing_source_text &ex) {
    throw ex << tag_name_info(std::string("artist"));
  }

  return result;
}

/*virtual*/ std::string
scribbu::tbt_support::content_type::evaluate(const file_info  & /*fi    */,
                                             const id3v2_tag  *  pid3v2,
                                             const track_data & /*ti    */,
                                             const id3v1_tag  *  pid3v1) const
{
  using namespace std;

  vector<unsigned char> v1;
  if (pid3v1) {
    // Try for extended ID3v1 info...
    pid3v1->enh_genre(back_inserter(v1));
    if (v1.empty()) {
      // but if we can't get that, fall back to ID3v1 genre.
      boost::optional<std::string> text = text_for_genre(pid3v1->genre());
      if (text) {
        for (auto c: *text) {
          v1.push_back((unsigned char)c);
        }
      }
    }
  }

  boost::optional<std::string> v2 = boost::none;
  if (pid3v2 && pid3v2->has_content_type()) {
    v2 = pid3v2->content_type();
  }

  std::string result;
  try {
    result = source_text(v1.begin(), v1.end(), v2);
    result = xform_and_encode(result);
  } catch (missing_source_text &ex) {
    throw ex << tag_name_info(std::string("content-type"));
  }

  return result;
}

/*virtual*/ std::string
scribbu::tbt_support::title::evaluate(const file_info  & /*fi    */,
                                      const id3v2_tag  *  pid3v2,
                                      const track_data & /*ti    */,
                                      const id3v1_tag  *  pid3v1) const
{
  using namespace std;

  vector<unsigned char> v1;
  if (pid3v1) {
    pid3v1->title(back_inserter(v1));
  }

  boost::optional<std::string> v2 = boost::none;
  if (pid3v2 && pid3v2->has_title()) {
    v2 = pid3v2->title();
  }

  std::string result;
  try {
    result = source_text(v1.begin(), v1.end(), v2);
    result = xform_and_encode(result);
  } catch (missing_source_text &ex) {
    throw ex << tag_name_info(std::string("title"));
  }

  return result;

}

/*virtual*/ std::string
scribbu::tbt_support::encoded_by::evaluate(const file_info  & /*fi    */,
                                           const id3v2_tag  * pid3v2,
                                           const track_data & /*ti    */,
                                           const id3v1_tag  * /*pid3v1*/) const
{
  using namespace std;

  if (!pid3v2 || !pid3v2->has_encoded_by()) {
    throw missing_source_text(all_source_preference::id3v2_only);
  }

  string text = pid3v2->encoded_by();
  text = do_the_xform(text, the_);
  text = do_cap_xform(text, cap_);
  return encode(text, out_);
}

/*virtual*/ std::string
scribbu::tbt_support::year::evaluate(const file_info  & /*fi    */,
                                     const id3v2_tag  * pid3v2,
                                     const track_data & /*ti    */,
                                     const id3v1_tag  * pid3v1) const
{
  using namespace std;

  vector<unsigned char> v1;
  if (pid3v1) {
    pid3v1->year(back_inserter(v1));
  }

  boost::optional<std::string> v2 = boost::none;
  if (pid3v2 && pid3v2->has_year()) {
    v2 = pid3v2->year();
  }

  std::string year = source_text(v1.begin(), v1.end(), v2,
                                 all_source_preference::prefer_id3v2,
                                 v1_encoding::automatic);

  if (year_format::two_digits == fmt_) {
    return year.substr(2);
  }
  else {
    return year;
  }

}


///////////////////////////////////////////////////////////////////////////////
//                             file-based terms                              //
///////////////////////////////////////////////////////////////////////////////

std::string
scribbu::tbt_support::file_term::transform(const std::filesystem::path &pth) const
{
  std::string out = do_the_xform(pth.string(), the_);
  out = do_cap_xform(out, cap_);
  out = do_ws_xform(out, compress_ws_, replace_ws_);
  return out;
}

/*virtual*/ std::string
scribbu::tbt_support::basename::evaluate(const file_info  & fi,
                                         const id3v2_tag  * /*pid3v2*/,
                                         const track_data & /*ti    */,
                                         const id3v1_tag  * /*pid3v1*/) const
{
  return file_term::transform(fi.filename().stem());
}

/*virtual*/ std::string
scribbu::tbt_support::extension::evaluate(const file_info  & fi,
                                          const id3v2_tag  * /*pid3v2*/,
                                          const track_data & /*ti    */,
                                          const id3v1_tag  * /*pid3v1*/) const
{
  return file_term::transform(fi.filename().extension());
}

/*virtual*/ std::string
scribbu::tbt_support::size::evaluate(const file_info  & fi,
                                     const id3v2_tag  * /*pid3v2*/,
                                     const track_data & /*ti    */,
                                     const id3v1_tag  * /*pid3v1*/) const
{
  using namespace std;

  size_t n = fi.size();

  stringstream buf;
  if (base::decimal == b_) {
    buf << n;
  }
  else {
    buf << hex;
    if (hex_case::upper == c_) {
      buf << setiosflags(ios::uppercase);
    }
  }

  return buf.str();

}

/*virtual*/ std::string
scribbu::tbt_support::md5::evaluate(const file_info  & /*fi    */,
                                    const id3v2_tag  * /*pid3v2*/,
                                    const track_data & ti,
                                    const id3v1_tag  * /*pid3v1*/) const
{
  using namespace std;

  vector<unsigned char> D;
  ti.get_md5(back_inserter(D));

  stringstream buf;
  if (base::hex == b_) {
    buf << hex;
    if (hex_case::upper == c_) {
      buf << setiosflags(ios::uppercase);
    }
  }
  for (auto x: D) {
    buf << x;
  }
  return buf.str();

}


// ID3 tag options:

// - decoding:
//   - source:
//     - prefer v2 (i.e. fail if neither is present)
//     - prefer v1 (i.e. fail if neither is present)
//     - v2 only (i.e. fail if v2 not present)
//     - v1 only (i.e. fail if v1 not present)
//   - v1 encoding:
//     - use BOM, if present
//     - treat as: ASCII, cp1252, utf-8, ...

// - transforms applicable to album, artist, content type, encoded by, title:
//   - if the value begins with "the", change to the suffix ", the"
//   - if the value ends with ", the", change the prefix to "the"
//   - all caps
//   - all lower
//   - capitalize each word (except the, is, and, &c)
//   - compress consecutive whitespace to a single space

// - transforms applicable to the genre:
//   - all caps
//   - all lower
//   - capitalize each word (except the, is, and, &c)

// - year formats:
//   - YYYY
//   - YY

// - encoding:
//   - utf-8 is default, any iconv-supported format is OK

// File-based options:

// - transforms applicable to basename & extension:
//   - all caps
//   - all lower
//   - capitalize each word (except the, is, and, &c)
//   - if the value begins with "the", change to the suffix ", the"
//   - if the value ends with ", the", change the prefix to "the"
//   - compress consecutive whitespace to a single space
//   - replace whitespace (with '-', '_', &c)

// - options applicable to size
//   - base 10 or base 16
//   - upper or lowercase hex digits
//   - units (bytes, K, M, G &c); suffix optional

//  - options applicable to MD5 checksum:
//   - base 10 or base 16
//   - upper or lowercase hex digits
//   - first/last N digits
