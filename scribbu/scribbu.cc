#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <boost/filesystem/fstream.hpp>
#include <boost/log/common.hpp>
#include <openssl/evp.h>

namespace fs  = boost::filesystem;
namespace src = boost::log::sources;


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
  std::unique_ptr<std::istream> pis(new fs::ifstream(pth, std::ios_base::binary));
  return std::make_pair(std::move(pis), fi);
}

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
  is.seekg(here, std::ios_base::beg);

  EVP_MD_CTX *mdctx = EVP_MD_CTX_create();
  if (! mdctx) {

  }

  if (! EVP_DigestInit_ex(mdctx, EVP_md5(), 0)) {

  }

  for (std::streamsize nleft = tag - here; nleft > 0; ) {

    std::streamsize nbytes = BUFSIZE > nleft ? nleft : BUFSIZE;

    is.read((char*)BUF, nbytes);
    if (! EVP_DigestUpdate(mdctx, BUF, nbytes)) {

    }

    nleft -= nbytes;

  }

  unsigned int  md_len;
  // unsigned char md_value[EVP_MAX_MD_SIZE];
  EVP_DigestFinal_ex(mdctx, _md5.begin(), &md_len);

  EVP_MD_CTX_destroy(mdctx);

}
