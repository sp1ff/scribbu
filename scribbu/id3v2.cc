#include <scribbu/id3v2.hh>

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
  return ( (b0 & 0x7f) << 14 ) |
         ( (b1 & 0x7f) <<  7 ) |
           (b2 & 0x7f);
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
         ( (b2 & 0x7f) << 21 ) |
         ( (b3 & 0x7f) << 14 ) |
         ( (b4 & 0x7f) <<  7 ) |
         (b4 & 0x7f);
}

std::size_t
scribbu::detail::unsigned_from_sync_safe(unsigned char b0,
                                         unsigned char b1,
                                         unsigned char b2,
                                         unsigned char b3)
{
  return uint32_from_sync_safe(b0, b1, b2, b3);
}

std::uint32_t
scribbu::detail::uint32_from_non_sync_safe(unsigned char b0,
                                           unsigned char b1,
                                           unsigned char b2)
{
  return ( b0 << 16 ) | ( b1 <<  8 ) | b2;
}

std::size_t
scribbu::detail::unsigned_from_non_sync_safe(unsigned char b0,
                                             unsigned char b1,
                                             unsigned char b2)
{
  return uint32_from_non_sync_safe(b0, b1, b2);
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

std::size_t
scribbu::detail::unsigned_from_non_sync_safe(unsigned char b0,
                                             unsigned char b1,
                                             unsigned char b2,
                                             unsigned char b3)
{
  return uint32_from_non_sync_safe(b0, b1, b2,b3);
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


///////////////////////////////////////////////////////////////////////////////
//                free functions exported from this function                 //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_info
scribbu::looking_at_id3v2(std::istream &is,
                          bool          restore_get_if_found/*=true*/)
{
  using scribbu::detail::unsigned_from_sync_safe;

  const std::size_t HEADER_SIZE = 10;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state.
  std::istream::streampos here = is.tellg();

  unsigned char buf[HEADER_SIZE];

  id3v2_info H;
  H.present_ = false;
  try {
    is.read((char*)buf, HEADER_SIZE);
  } catch (const std::ios_base::failure &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
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
  // TODO: Working this out...
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

  std::ptrdiff_t num_syncs;
  for (num_syncs = 1; ; ++num_syncs) {

    // Let the second byte in the next false sync after i0 be i1.
    std::ptrdiff_t i1;
    for (i1 = i0 + 1; i1 < cb - 1; ++i1) {
      if (0xff == p[i1] && 0x00 == p[i1 + 1]) {
        break;
      }
    }

    if (i1 == cb) {
      return cb - num_syncs;
    }

    ++i1;

    // Move every element in [i0+1,i1) back by num_sync elements.
    // TODO: Unroll the loop for 'num_sync' > 1...
    for (std::ptrdiff_t i = i0 + 1; i < i1; ++i) {
      p[i - num_syncs] = p[i];
    }

    i0 = i1;

  }

  return cb - num_syncs;

} // End free function resynchronise.

std::string scribbu::to_utf8(unsigned char        id3v2_version,
                             unsigned char        encoding,
                             const unsigned char *pbuf,
                             std::size_t          cbbuf)
{
    const char * const ISO88591 = "ISO-8859-1";
    const char * const UCS2BE   = "UCS-2BE";
    const char * const UCS2LE   = "UCS-2LE";
    const char * const UCS2     = "UCS-2";
    const char * const UTF16    = "UTF-16";
    const char * const UTF16BE  = "UTF-16BE";

    const char * encoding_text = 0;
    if (2 == id3v2_version || 3 == id3v2_version) {

      if (0 == encoding) {
        encoding_text = ISO88591;
      }
      else if (1 == encoding) {
        if (1 < cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
          encoding_text = UCS2BE;
        } else if (1 < cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
          encoding_text = UCS2LE;
        } else {
          encoding_text = UCS2;
        }
      }

    }
    else if (4 == id3v2_version) {

      if (3 == encoding) {
        // TODO: Spec says this should be null-terminated; remove that null, if
        // present.
        return std::string(pbuf, pbuf + cbbuf);
      }

      if (0 == encoding) {
        encoding_text = ISO88591;
      }
      else if (1 == encoding) {
        encoding_text = UTF16;
      }
      else if (2 == encoding) {
        encoding_text = UTF16BE;
      }

    }
    else {
      throw id3v2_tag::unknown_version(id3v2_version);
    }

    if (!encoding_text) {
      throw std::runtime_error("Unknown encoding");
    }

    scribbu::detail::iconv_guard guard("UTF-8", encoding_text);
    return scribbu::detail::to_utf8(guard, pbuf, cbbuf);
}


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

/// Initialize from the first ten bytes of \a is
scribbu::id3v2_tag::id3v2_tag(std::istream &is)
{
  id3v2_info H = looking_at_id3v2(is, false);
  if (!H.present_) {
    throw no_tag();
  }

  version_  = H.version_;
  revision_ = H.revision_;
  flags_    = H.flags_;
  size_     = H.size_;
  unsync_   = H.flags_ & 0x80;
}

/// Initialize from and id3v2_info
scribbu::id3v2_tag::id3v2_tag(const id3v2_info &H):
  version_ (H.version_     ),
  revision_(H.revision_    ),
  flags_   (H.flags_       ),
  size_    (H.size_        ),
  unsync_  (H.flags_ & 0x80)
{
  if (!H.present_) {
    throw no_tag();
  }
}
