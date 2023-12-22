/**
 * \file scribbu.cc
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

#include <scribbu/scribbu.hh>

#include <type_traits>
#include <unordered_map>

#include <fstream>
#include <boost/log/common.hpp>

#include <sys/stat.h>

#include <openssl/evp.h>

#include <scribbu/id3v1.hh>
#include <scribbu/mp3.hh>
#include <scribbu/scheme.hh>

namespace fs  = std::filesystem;
namespace src = boost::log::sources;


///////////////////////////////////////////////////////////////////////////////
//                     library initialization & teardown                     //
///////////////////////////////////////////////////////////////////////////////

void
scribbu::static_initialize()
{
  OpenSSL_add_all_digests();
}

void
scribbu::static_cleanup()
{
  EVP_cleanup();
}


///////////////////////////////////////////////////////////////////////////////
//                              class file_info                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::file_info::file_info(fs::path pth):
    atime_(std::nullopt),
    mtime_(std::nullopt),
    ctime_(std::nullopt)
{
  if (pth.is_relative()) {
    pth = fs::absolute(pth);
  }

  parent_   = pth.parent_path();
  filename_ = pth.filename();
  size_     = fs::file_size(pth);

  struct stat st;
  if (0 == stat(pth.native().c_str(), &st)) {
    atime_ = st.st_atime;
    mtime_ = st.st_mtime;
    ctime_ = st.st_ctime;
  }
}

std::pair<std::ifstream, scribbu::file_info>
scribbu::open_file(fs::path pth)
{
  return std::make_pair(open_ifstream(pth.native()), file_info(pth));
}

///////////////////////////////////////////////////////////////////////////////
//                                open_ifstream                              //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
const char *
scribbu::file_open_error::what() const noexcept(true)
{
  // lazily format
  if ( ! pwhat_ ) {
    std::stringstream stm;
    stm << "Failed to open `" << name_ << "': " << strerror(errno_);
    pwhat_.reset(new std::string(stm.str()));
  }
  return pwhat_->c_str();
}

std::ifstream
scribbu::open_ifstream(const std::string &name,
                       std::ios_base::openmode mode /*= std::ios_base::in*/)
{
  errno = 0;
  std::ifstream ifs(name, mode | std::ios_base::in);
  if (!ifs.is_open()) {
    throw file_open_error(name, errno);
  }
  return ifs;
}

///////////////////////////////////////////////////////////////////////////////
//                            class openssl_error                            //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::openssl_error::what() const noexcept
{
  if ( ! pwhat_ ) {
    pwhat_.reset(new std::string(ERR_error_string(err_, 0)));
  }

  return pwhat_->c_str();
}

////////////////////////////////////////////////////////////////////////////////
//                               free functions                               //
////////////////////////////////////////////////////////////////////////////////

/// Locate the ID3v1 tag-- returns [here, there) where here is the current
/// stream position and there is either the first byte of the ID3v1 tag or the
/// one-past-the-end position, so that the track data is bracketed in [here,
/// there)
std::tuple<std::streampos, std::streampos>
scribbu::find_id3v1_tag(std::istream &is)
{
  const std::ios_base::iostate EXC_MASK = std::ios_base::eofbit|
    std::ios_base::failbit|std::ios_base::badbit;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(EXC_MASK);

  // The ID3v1 tag is 128 bytes long & begins with the sequence "TAG",
  // and the extended tag is 227 bytes & begins with the sequence
  // "TAG+". So, if there's an ID3v1 tag present, the sequence "TAG"
  // will be present at len - 128 or len - 355.
  char buf[4];

  scribbu::id3_v1_tag_type tag_type = id3_v1_tag_type::none;

  std::streampos here = is.tellg(), tag;

  try {
    is.seekg(-355, std::ios_base::end);
    is.read(buf, 4);
    if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2] && '+' == buf[3]) {
      // ID3v1 extended tag at `tag'; stream ptr at `tag' + 4.
      tag = is.tellg() - (std::streampos) 4;
      tag_type = id3_v1_tag_type::v_1_extended;
    }
  }
  catch (const std::ios_base::failure &ex) {
    // OK-- something went wrong. Clear the flag, set `tag' to EoS.
    // stream ptr at the same place.
    is.exceptions(std::ios_base::goodbit);
    is.clear();
    is.seekg(0, std::ios_base::end);
    tag = is.tellg();
    is.exceptions(exc_mask);
  }

  if (id3_v1_tag_type::none == tag_type) {

    try {
      is.seekg(-128, std::ios_base::end);
      is.read(buf, 3);
      if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2]) {
        // ID3v1 tag at `tag'; stream ptr at `tag' + 3
        tag = is.tellg() - (std::streampos) 3;
        tag_type = id3_v1_tag_type::v_1;
      } else {
        // No ID3v1 tag-- set `tag' to the EoS; stream ptr at same place.
        is.seekg(0, std::ios_base::end);
        tag = is.tellg();
      }
    }
    catch (const std::ios_base::failure &ex) {
      // OK-- something went wrong. Clear the flag, set `tag' to EoS.
      // stream ptr at the same place.
      is.exceptions(std::ios_base::goodbit);
      is.clear();
      is.seekg(0, std::ios_base::end);
      tag = is.tellg();
    }

  }

  is.exceptions(exc_mask);

  return std::make_tuple(here, tag);
}

std::array<unsigned char, scribbu::MD5_DIGEST_SIZE>
scribbu::compute_track_md5(std::istream &is, std::streampos hint /*= EOF*/)
{
  const std::ios_base::iostate EXC_MASK = std::ios_base::eofbit|
    std::ios_base::failbit|std::ios_base::badbit;

  std::array<unsigned char, MD5_DIGEST_SIZE> md5;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(EXC_MASK);

  std::streampos here, tag;
  if (hint == EOF) {
    std::tie(here, tag) = find_id3v1_tag(is);
  } else {
    here = is.tellg();
    tag = hint;
  }

  const std::size_t BUFSIZE = 4 * 1024 * 1024; // Four megabytes

  static unsigned char BUF[BUFSIZE];

  // Compute an MD5 checksum of the file contents from 'here' to 'tag'
  EVP_MD_CTX *mdctx = EVP_MD_CTX_create();
  if (! mdctx) {
    throw new openssl_error();
  }

  if (! EVP_DigestInit_ex(mdctx, EVP_md5(), 0)) {
    EVP_MD_CTX_destroy(mdctx);
    throw new openssl_error();
  }

  for (std::streamsize nleft = tag - here; nleft > 0; ) {

    std::streamsize nbytes = BUFSIZE > nleft ? nleft : BUFSIZE;

    is.read((char*)BUF, nbytes);
    if (! EVP_DigestUpdate(mdctx, BUF, nbytes)) {
      EVP_MD_CTX_destroy(mdctx);
      throw new openssl_error();
    }

    nleft -= nbytes;

  }

  unsigned int  md_len;
  EVP_DigestFinal_ex(mdctx, md5.begin(), &md_len);

  EVP_MD_CTX_destroy(mdctx);

  is.exceptions(exc_mask);

  return md5;
}

///////////////////////////////////////////////////////////////////////////////
//                             class track_data                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::track_data::track_data(std::istream &is) :
  size_bytes_(0),
  duration_secs_(std::nullopt)
{
  const std::ios_base::iostate EXC_MASK = std::ios_base::eofbit|
    std::ios_base::failbit|std::ios_base::badbit;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(EXC_MASK);

  // Find the ID3v1 tag (or the end of the file); if it's there, the ID3v1 tag
  // will be at one of two fixed offsets, so we can just seek (i.e.  this method
  // just manipulates the get ptr, it doesn't actually need to read the entire
  // track).
  std::streampos here, tag;
  std::tie(here, tag) = find_id3v1_tag(is);
  size_bytes_ = tag - here;

  // Now that we've ID'd the track data, reset the get ptr...
  is.seekg(here);
  // and compute the MD5 checksum (which *will* entail reading the entirety of
  // the audio data into memory)
  md5_ = compute_track_md5(is, tag);

  // Re-set the get ptr one more time...
  is.seekg(here, std::ios_base::beg);
  // and compute the duration of the audio data. This may or may not require
  // re-reading all of it, depending on whether or not VBRI is present.
  try {
    duration_secs_ = get_mp3_duration(is);
  } catch (const std::exception &) {
    // pass
  }

  // Finally, patch-up the istream according to this method's contract.
  is.seekg(tag, std::ios_base::beg);
  is.clear(); // `eof' may be set
  is.exceptions(exc_mask);
}

std::string
scribbu::urlencode(const std::string &text)
{
  using namespace std;


  string out;
  for (auto c: text) {

    if ('%' == c) {
      // % needs to be escaped
      out += "%25";
    } else if (('A' <= c && 'Z' >= c) ||
               ('a' <= c && 'z' >= c) ||
               ('0' <= c && '9' >= c) ||
               '-' == c || '_' == c || '.' == c || '~' == c) {
      // un-reserved characters never need to be escaped
      out += c;
    } else {
      // otherwise, escape
      stringstream stm;
      // Getting a char to print as a hex is surprisingly difficult:
      // https://stackoverflow.com/questions/673240/how-do-i-print-an-unsigned-char-as-hex-in-c-using-ostream
      stm << "%" << hex << setfill('0') << setw(2) <<
        ( ((unsigned short) c) & 0xff );
      out += stm.str();
    }

  }

  return out;
}

char
strtochar(const char *p)
{
  char out = 0;
  if ('0' <= p[0] && '9' >= p[0]) {
    out += 16 * (p[0] - '0');
  } else if ('a' <= p[0] && 'f' >= p[0]) {
    out += 16 * (p[0] - 'a' + 10);
  } else if ('A' <= p[0] && 'F' >= p[0]) {
    out += 16 * (p[0] - 'A' + 10);
  } else {
    throw std::invalid_argument("expected hex character");
  }

  if ('0' <= p[1] && '9' >= p[1]) {
    out += (p[1] - '0');
  } else if ('a' <= p[1] && 'f' >= p[1]) {
    out += (p[1] - 'a' + 10);
  } else if ('A' <= p[1] && 'F' >= p[1]) {
    out += (p[1] - 'A' + 10);
  } else {
    throw std::invalid_argument("expected hex character");
  }

  return out;
}

std::string
scribbu::urldecode(const std::string &text)
{
  using namespace std;

  string out;

  bool saw_pct = false;
  for (size_t i = 0, n = text.size(); i < n; ++i) {

    char c = text[i];
    if (saw_pct) {

      if ('%' == c) {
        out += c;
      } else if (i+2 > n) {
        throw std::invalid_argument("incomplete % sequence");
      } else {
        // we expect two hex digits
        out += strtochar(text.c_str() + i);
        i += 1;
      }

      saw_pct = false;

    } else {

      if ('%' == c) {
        saw_pct = true;
      } else {
        out += c;
      }

    }

  }

  return out;
}
