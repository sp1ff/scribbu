/**
 * \file id3v2.cc
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/id3v2.hh>

#include <arpa/inet.h>
#include <iconv.h>
#include <ios>
#include <zlib.h>

#include <boost/functional/hash.hpp>
#include <boost/optional.hpp>
#include <boost/shared_array.hpp>


///////////////////////////////////////////////////////////////////////////////
//                   free functions private to this module                   //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::detail::unsigned_from_sync_safe(unsigned char b0,
                                         unsigned char b1,
                                         unsigned char b2)
{
  return ( (b0 & 0x7f) << 14 ) | ( (b1 & 0x7f) << 7 ) | (b2 & 0x7f);
}

std::size_t
scribbu::detail::unsigned_from_sync_safe(unsigned char b0,
                                         unsigned char b1,
                                         unsigned char b2,
                                         unsigned char b3)
{
  return uint32_from_sync_safe(b0, b1, b2, b3);
}

std::size_t
scribbu::detail::unsigned_from_sync_safe(unsigned char b0,
                                         unsigned char b1,
                                         unsigned char b2,
                                         unsigned char b3,
                                         unsigned char b4)
{
  return uint32_from_sync_safe(b0, b1, b2, b3, b4);
}

std::uint32_t
scribbu::detail::uint32_from_sync_safe(unsigned char b0,
                                       unsigned char b1,
                                       unsigned char b2,
                                       unsigned char b3)
{
  return ( (b0 & 0x7f) << 21 ) |
         ( (b1 & 0x7f) << 14 ) |
         ( (b2 & 0x7f) <<  7 ) |
           (b3 & 0x7f);
}

std::uint32_t
scribbu::detail::uint32_from_sync_safe(unsigned char b0,
                                       unsigned char b1,
                                       unsigned char b2,
                                       unsigned char b3,
                                       unsigned char b4)
{
  return ( (b0 & 0x0f) << 28 ) |
         ( (b1 & 0x7f) << 21 ) |
         ( (b2 & 0x7f) << 14 ) |
         ( (b3 & 0x7f) <<  7 ) |
           (b4 & 0x7f);
}

std::size_t
scribbu::detail::unsigned_from_non_sync_safe(unsigned char b0,
                                             unsigned char b1,
                                             unsigned char b2)
{
  return uint32_from_non_sync_safe(b0, b1, b2);
}

std::size_t
scribbu::detail::unsigned_from_non_sync_safe(unsigned char b0,
                                             unsigned char b1,
                                             unsigned char b2,
                                             unsigned char b3)
{
  return uint32_from_non_sync_safe(b0, b1, b2,b3);
}

std::uint32_t
scribbu::detail::uint32_from_non_sync_safe(unsigned char b0,
                                           unsigned char b1,
                                           unsigned char b2)
{
  return ( b0 << 16 ) | ( b1 <<  8 ) | b2;
}

std::uint32_t
scribbu::detail::uint32_from_non_sync_safe(unsigned char b0,
                                           unsigned char b1,
                                           unsigned char b2,
                                           unsigned char b3)
{
  return ( b0 << 24 ) |
         ( b1 << 16 ) |
         ( b2 <<  8 ) |
         b3;
}

void
scribbu::detail::sync_safe_from_unsigned(std::size_t x, unsigned char b[])
{
  // N.B. This implementation assumes that the output is in big-endian (per the
  // ID3v2 specification) & will return the correct value regardless of the
  // host byte order.

  // Four sync-safe bytes contain 28 bits, so the maximum value so
  // representable is 2^28-1 = 268,435,455 = 0x10000000 - 1 =
  // 0xfffffff

  const size_t MAX_VALUE = 0xfffffff;

  if (x > MAX_VALUE) {
    throw std::invalid_argument("not representable in four sync-safe bytes");
  }

  b[3] =   x         & 127;
  b[2] = ( x >>  7 ) & 127;
  b[1] = ( x >> 14 ) & 127;
  b[0] = ( x >> 21 ) & 127;
}


///////////////////////////////////////////////////////////////////////////////
//                free functions exported from this function                 //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_info
scribbu::looking_at_id3v2(std::istream &is,
                          bool restore_get_if_found/*=true*/)
{
  using scribbu::detail::unsigned_from_sync_safe;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state.
  std::streampos here = is.tellg();

  unsigned char buf[ID3V2_HEADER_SIZE];

  id3v2_info H;
  H.present_ = false;
  try {
    is.read((char*)buf, ID3V2_HEADER_SIZE);
  } catch (const std::ios_base::failure &ex) {
    is.clear();
    is.exceptions(exc_mask);
    is.seekg(here, std::ios_base::beg);
    return H;
  }

  if ('I' == buf[0] && 'D' == buf[1] && '3' == buf[2]) {

    H.version_  = buf[3];
    H.revision_ = buf[4];

    // Cf. Sec. 3.1 of any ID3v2 spec.
    if (0xff != H.version_ && 0xff != H.revision_) {

      H.flags_ = buf[5];

      if (0x80 > buf[6] && 0x80 > buf[7] &&
          0x80 > buf[8] && 0x80 > buf[9]) {

        H.size_ = unsigned_from_sync_safe(buf[6], buf[7], buf[8], buf[9]);
        H.present_ = true;

      } // End if on bytes 6-9 being a sync-safe int.

    } // End if on valid version/revision bytes.

  } // End if on "ID3" magic number.

  if (!H.present_ || restore_get_if_found) {
    is.seekg(here, std::ios_base::beg);
  }

  // Restore the exception mask
  is.exceptions(exc_mask);

  return H;

} // End free function looking_at_id3v2.

/// Restore false sync signals in a buffer
std::size_t scribbu::resynchronise(unsigned char *p, std::size_t cb)
{
  ///////////////////////////////////////////////////////////////////////////
  // Example:
  //  0     1     2     3     4     5     6     7     8     9     10    11    12    13    14
  // [0x01, 0xff, 0x00, 0x02, 0x03, 0x04, 0xff, 0x00, 0x05, 0x06, 0xff, 0x00, 0x07, 0xff, 0x00]
  //  |         |       |                     |       |               |       |         |
  // So this is broken out into sub-intervals: [0,2), [3,7), [8,11), [12,14)
  // and the copies will be:
  // [3,7) => [2,6) (i.e. move each element in [3,7) back by one)
  // [8,11) => [6,9) (move each element in [8,11) back by two)
  // [12,14) => [9,11) (move each element in [12,14) back by three)
  // IOW, let the second byte in the first false sync be at index i0 (2 in this example).
  // Let the second byte in the next false sync after i0 + 1 (3) be index i1 (7).
  // move every element in [i0+1,i1) back by one element, let i0 be i1 (7)
  // Let the second byte in the next false sync after i0 + 1(8) be index i1 (11)
  // move ever element in [i0+1,11) back by two elements, let i0 be i1 (11)
  // Let the second byte in the next false sync after i0 + 1 (12) be index i1 (14)
  // move every element in [i0+1,i1) back by three elemnts.
  ///////////////////////////////////////////////////////////////////////////

  if (0 == cb) {
    return 0;
  }

  // Find the first false sync; let i0 be the index of the second byte therein.
  std::ptrdiff_t i0;
  for (i0 = 0; i0 < cb - 1; ++i0) {
    if (0xff == p[i0] && 0x00 == p[i0 + 1]) {
      break;
    }
  }

  ++i0;

  if (i0 == cb) {
    // No false sync's in this buffer-- we're done.
    return cb;
  }

  std::ptrdiff_t sync_no;
  for (sync_no = 1; i0 < cb; ++sync_no) {

    // Let the second byte in the next false sync after i0 be i1.
    std::ptrdiff_t i1;
    for (i1 = i0 + 1; i1 < cb - 1; ++i1) {
      if (0xff == p[i1] && 0x00 == p[i1 + 1]) {
        break;
      }
    }

    if (i1 < cb) {
      ++i1;
    }

    // Move every element in [i0+1,i1) back by num_sync elements.
    for (std::ptrdiff_t i = i0 + 1; i < i1; ++i) {
      p[i - sync_no] = p[i];
    }

    if (i1 == cb) {
      // That was the last one
      break;
    }

    i0 = i1;

  }

  return cb - sync_no;

} // End free function resynchronise.


///////////////////////////////////////////////////////////////////////////////
//                             class zlib_error                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::zlib_error::zlib_error(int status):
  std::runtime_error(zError(status))
{ }


///////////////////////////////////////////////////////////////////////////////
//                           id3v2_tag exceptions                            //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
const char *
scribbu::id3v2_tag::duplicate_frame_error::what() const noexcept(true)
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << "Frame ";
    if ( id3_.null() ) {
      stm << id4_;
    }
    else {
      stm << id3_;
    }
    stm << " was defined " << n_ << " times";
    pwhat_.reset( new std::string(stm.str()) );
  }
  return pwhat_->c_str();
}

/*virtual*/
const char *
scribbu::id3v2_tag::unknown_frame_error::what() const noexcept(true)
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << "Unknown frame ";
    if (id3_.null()) {
      stm << id4_;
    }
    else {
      stm << id3_;
    }
    pwhat_.reset( new std::string(stm.str()) );
  }
  return pwhat_->c_str();
}


///////////////////////////////////////////////////////////////////////////////
//                        class reserved_frame_error                         //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
const char *
scribbu::id3v2_tag::reserved_frame_error::what() const noexcept(true)
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << "frame ";
    if (id3_.null()) {
      stm << id4_;
    }
    else {
      stm << id3_;
    }
    stm << " is reserved; it's parser may not be replaced";
    pwhat_.reset( new std::string(stm.str()) );
  }
  return pwhat_->c_str();
}


/*virtual*/
const char *
scribbu::id3v2_tag::invalid_tag::what() const noexcept(true)
{
  return "invalid tag";
}


///////////////////////////////////////////////////////////////////////////////
//                               class no_tag                                //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::id3v2_tag::no_tag::what() const noexcept
{
  return "no tag";
}


///////////////////////////////////////////////////////////////////////////////
//                           class unknown_version                           //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/ const char *
scribbu::id3v2_tag::unknown_version::what() const noexcept
{
  if (!pwhat_) {
    std::stringstream stm;
    stm << "Unknown version " << (int) version_;
    pwhat_.reset( new std::string(stm.str()) );
  }
  return pwhat_->c_str();
}


///////////////////////////////////////////////////////////////////////////////
//                             class id3v2_tag                               //
///////////////////////////////////////////////////////////////////////////////

/// Initialize from the first five bytes of \a is
scribbu::id3v2_tag::id3v2_tag(std::istream &is)
{
  static const unsigned int HALF_ID3V2_HEADER_SIZE = 5;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state.
  std::streampos here = is.tellg();

  unsigned char buf[HALF_ID3V2_HEADER_SIZE];

  bool present = false;
  try {
    is.read((char*)buf, HALF_ID3V2_HEADER_SIZE);
  } catch (const std::ios_base::failure &ex) {
    is.exceptions(exc_mask);
    is.clear();
    is.seekg(here, std::ios_base::beg);
    throw;
  }

  if ('I' == buf[0] && 'D' == buf[1] && '3' == buf[2]) {

    version_  = buf[3];
    revision_ = buf[4];

    // Cf. Sec. 3.1 of any ID3v2 spec.
    present = 0xff != version_ && 0xff != revision_;
  }

  if (!present) {
    is.seekg(here, std::ios_base::beg);
  } // End if on "ID3" magic number.

  // Restore the exception mask
  is.exceptions(exc_mask);
}

/// Initialize from and id3v2_info
scribbu::id3v2_tag::id3v2_tag(const id3v2_info &H):
  version_ (H.version_     ),
  revision_(H.revision_    ),
  unsync_  (H.flags_ & 0x80)
{
  if (!H.present_) {
    throw no_tag();
  }
}

std::pair<unsigned char, std::size_t>
scribbu::id3v2_tag::parse_flags_and_size(std::istream &is)
{
  using scribbu::detail::unsigned_from_sync_safe;

  static const unsigned int HALF_ID3V2_HEADER_SIZE = 5;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state.
  std::streampos here = is.tellg();

  unsigned char buf[HALF_ID3V2_HEADER_SIZE];

  try {
    is.read((char*)buf, HALF_ID3V2_HEADER_SIZE);
  } catch (const std::ios_base::failure &ex) {
    is.exceptions(exc_mask);
    is.clear();
    is.seekg(here, std::ios_base::beg);
    throw;
  }

  unsigned char flags = buf[0];

  std::size_t cb = 0;
  if (0x80 > buf[1] && 0x80 > buf[2] &&
      0x80 > buf[3] && 0x80 > buf[4]) {

    cb = unsigned_from_sync_safe(buf[1], buf[2], buf[3], buf[4]);

  } // End if on bytes 6-9 being a sync-safe int.

  if (0 == cb) {
    is.seekg(here, std::ios_base::beg);
  }

  // Restore the exception mask
  is.exceptions(exc_mask);

  return std::make_pair(flags, cb);

} // End method parse_flags_and_size.
