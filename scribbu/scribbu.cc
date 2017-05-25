#include <scribbu/scribbu.hh>

#include <type_traits>
#include <unordered_map>

#include <boost/filesystem/fstream.hpp>
#include <boost/log/common.hpp>

#include <openssl/evp.h>

#include <scribbu/id3v1.hh>

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

}

std::pair<std::unique_ptr<std::istream>, scribbu::file_info>
scribbu::open_file(fs::path pth)
{
  file_info fi(pth);
  std::unique_ptr<std::istream> pis(new fs::ifstream(pth, std::ios_base::binary));
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

scribbu::track_data::track_data(std::istream &is)
{
  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  const std::size_t BUFSIZE = 4 * 1024 * 1024; // Four megabytes

  static unsigned char BUF[BUFSIZE];

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
  }
  catch (const std::ios_base::failure &ex) {
    is.exceptions(exc_mask);
    is.clear();
    is.seekg(0, std::ios_base::end);
    tag = is.tellg();
    is.seekg(here, std::ios_base::beg);
  }

  // Compute an MD5 checksum of the file contents from 'here' to 'tag'
  size_ = tag - here;
  is.seekg(here, std::ios_base::beg);

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
  // unsigned char md_value[EVP_MAX_MD_SIZE];
  EVP_DigestFinal_ex(mdctx, _md5.begin(), &md_len);
  
  EVP_MD_CTX_destroy(mdctx);

}
