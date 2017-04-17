#include "framesv2.hh"

#include <boost/functional/hash.hpp>


///////////////////////////////////////////////////////////////////////////////
//                             class iconv_error                             //
///////////////////////////////////////////////////////////////////////////////

const char * scribbu::iconv_error::what() const noexcept
{
  if (! pwhat_) {
    std::stringstream stm;
    stm << "iconv failure: " << strerror(errno_);
    pwhat_.reset(new std::string(stm.str()));
  }
  return pwhat_->c_str();
}


///////////////////////////////////////////////////////////////////////////////
//                             utility functions                             //
///////////////////////////////////////////////////////////////////////////////

std::string scribbu::detail::to_utf8(iconv_t              cd,
                                     const unsigned char *pbuf,
                                     std::size_t          cbbuf)
{
  char *inbuf = const_cast<char*>(reinterpret_cast<const char*>(pbuf));
  std::size_t inbytesleft = cbbuf;

  // We can't know a priori how many octets the output buffer will require;
  // cf. 
  // http://stackoverflow.com/questions/13297458/simple-utf8-utf16-string-conversion-with-iconv
  std::size_t cbout = cbbuf << 2;
  std::unique_ptr<char []> poutbuf(new char[cbout]);

  // "The iconv function converts one multibyte character at a time, and for
  // each character conversion it increments *inbuf and decrements *inbytesleft
  // by the number of converted input bytes, it increments *outbuf and
  // decrements *outbytesleft by the number of converted output bytes, and it
  // updates the conversion state contained in cd."
  std::size_t outbytesleft = cbout;
  char *outbuf = poutbuf.get();
  std::size_t status = iconv(cd, &inbuf, &inbytesleft,
                             &outbuf, &outbytesleft);
  while (~0 == status && E2BIG == errno) {
    // If the "output buffer has no more room for the next converted
    // character. In this case it sets errno to E2BIG and returns
    // (size_t)(−1)." Try again with a bigger buffer :P
    cbout <<= 2;
    poutbuf.reset(new char[cbout]);

    inbuf = const_cast<char*>(reinterpret_cast<const char*>(pbuf));
    inbytesleft = cbbuf;
    outbytesleft = cbout;
    char *outbuf = poutbuf.get();
    status = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
  }

  if (~0 == status) {
    throw iconv_error(errno);
  }

  // If there's a UTF-8 BOM at the start, don't copy that
  outbuf = poutbuf.get();
  cbout -= outbytesleft;

  if (2 < cbout  &&
      0xef == (unsigned char)outbuf[0] &&
      0xbb == (unsigned char)outbuf[1] &&
      0xbf == (unsigned char)outbuf[2]) {
    outbuf += 3;
    cbout -= 3;
  }

  // If there are trailing nulls, don't copy them, either.
  if (0 < cbout) {
    while (0 == outbuf[cbout - 1]) {
      --cbout;
    }
  }

  return std::string(outbuf, outbuf + cbout);
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

bool scribbu::operator==(const frame_id3 &lhs,
                         const frame_id3 &rhs)
{
  unsigned char id_lhs[3], id_rhs[3];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] && id_lhs[2] == id_rhs[2];
}

std::ostream& operator<<(std::ostream &os, const scribbu::frame_id3 &x) {
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

bool scribbu::operator==(const frame_id4 &lhs,
                         const frame_id4 &rhs)
{
  unsigned char id_lhs[4], id_rhs[4];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] &&
         id_lhs[2] == id_rhs[2] && id_lhs[3] == id_rhs[3];
}

std::ostream& operator<<(std::ostream &os, const scribbu::frame_id4 &x) {
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

// TODO: Unit-test unique_file_id, once I find a test case.
// TODO: Unit test encryption_method, once I find a test case.
