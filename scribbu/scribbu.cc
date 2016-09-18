#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <boost/filesystem/fstream.hpp>
#include <boost/log/common.hpp>
#include <openssl/evp.h>

namespace fs  = boost::filesystem;
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

scribbu::file_info::file_info(fs::path pth) {

  if (pth.is_relative()) {
    pth = fs::absolute(pth);
  }

  parent_   = pth.parent_path();
  filename_ = pth.filename();
  size_     = fs::file_size(pth);

  // TODO: Figure out how to call stat portably to get more data...

}

std::pair<std::unique_ptr<std::istream>, scribbu::file_info>
scribbu::open_file(fs::path pth)
{
  file_info fi(pth);
  std::unique_ptr<std::istream> pis(
    new fs::ifstream(pth, std::ios_base::binary));
  return std::make_pair(std::move(pis), fi);
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


///////////////////////////////////////////////////////////////////////////////
//                             class track_data                              //
///////////////////////////////////////////////////////////////////////////////

/*static*/ const scribbu::standard_track_data_formatter
scribbu::track_data::DEF_FORMATTER(scribbu::file_size_units::megabytes);

scribbu::track_data::track_data(std::istream &is)
{
  const std::size_t BUFSIZE = 4 * 1024 * 1024; // Four megabytes

  static unsigned char BUF[BUFSIZE];

  // The ID3v1 tag is 128 bytes long & begins with the sequence "TAG",
  // and the extended tag is 227 bytes & begins with the sequence
  // "TAG+". So, if there's an ID3v1 tag present, the sequence "TAG"
  // will be present at len - 128 or len - 355.
  char buf[4];

  scribbu::id3_v1_tag_type tag_type = id3_v1_tag_type::none;

  std::streampos here = is.tellg(), tag;
  is.seekg(-355, std::ios_base::end);
  is.read(buf, 4);
  if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2] && '+' == buf[3]) {
    tag = is.tellg() - (std::streampos) 4;
    tag_type = id3_v1_tag_type::v_1_extended;
  } else {
    is.seekg(-128, std::ios_base::end);
    is.read(buf, 3);
    if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2]) {
      tag = is.tellg() - (std::streampos) 3;
      tag_type = id3_v1_tag_type::v_1;
    } else {
      is.seekg(std::ios_base::end);
      tag = is.tellg();
    }
  }

  // Compute an MD5 checksum of the file contents from 'here' to 'tag'
  size_ = tag - here;
  is.seekg(here, std::ios_base::beg);

  EVP_MD_CTX *mdctx = EVP_MD_CTX_create();
  if (! mdctx) {
    throw new openssl_error();
  }

  if (! EVP_DigestInit_ex(mdctx, EVP_md5(), 0)) {
    throw new openssl_error();
  }

  for (std::streamsize nleft = tag - here; nleft > 0; ) {

    std::streamsize nbytes = BUFSIZE > nleft ? nleft : BUFSIZE;

    is.read((char*)BUF, nbytes);
    if (! EVP_DigestUpdate(mdctx, BUF, nbytes)) {
      throw new openssl_error();
    }

    nleft -= nbytes;

  }

  unsigned int  md_len;
  EVP_DigestFinal_ex(mdctx, md5_.begin(), &md_len);
  EVP_MD_CTX_destroy(mdctx);

}


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
  // cf. http://stackoverflow.com/questions/13297458/simple-utf8-utf16-string-conversion-with-iconv
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
  while (0 < cbout && 0 == outbuf[cbout - 1]) {
    --cbout;
  }

  return std::string(outbuf, outbuf + cbout);
}

std::string
scribbu::escape_for_csv(const std::string &s,
                        char               sep)
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
