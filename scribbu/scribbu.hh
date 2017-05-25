#ifndef SCRIBBU_H_INCLUDED
#define SCRIBBU_H_INCLUDED 1
/**
 * \mainpage scribbu
 *
 *
 * \section references References
 *
 *  1. \anchor ref_01 Alexandrescu, Andrei, 2001: Modern C++ Design. Addison-Wesley, 323pp.
 *
 *  2. \anchor ref_02 Langer, Angelika and Kreft, Klaus, 1999: Standard C++ IOStrems and 
 *     Locales. Addison-Wesley, 640pp.
 *
 *
 */

#include <iostream>

#include <boost/filesystem.hpp>

#include <openssl/err.h>

namespace scribbu {

  /// Library initialization
  void static_initialize();

  // Library cleanup
  void static_cleanup();

  /**
   * \class file_info
   *
   * \brief Information about a music track independent of tags or
   * content
   *
   *
   * Instantiate this class with a path naming a musical track; the
   * ctor will gather assorted data about the filesystem entity (as
   * opposed to ID3 tags, or information derived from the track data
   * itself).
   *
   * \todo Add atime & mtime
   *
   *
   */

  class file_info {

  public:
    file_info()
    { }
    file_info(boost::filesystem::path pth);

  public:
    /// Absolute path of the directory containing this file
    boost::filesystem::path parent() const
    { return parent_; }
    /// Unqualified filename for this track
    boost::filesystem::path filename() const
    { return filename_; }
    /// The size, in bytes, of this file
    std::uintmax_t size() const
    { return size_; }

  private:
    boost::filesystem::path parent_;
    boost::filesystem::path filename_;
    std::uintmax_t size_;

  };

  /**
   * \brief Open a file & return a stream & a file_info instance
   * describing that file
   *
   *
   */

  std::pair<std::unique_ptr<std::istream>, file_info>
  open_file(boost::filesystem::path pth);

  class openssl_error: public virtual boost::exception,
                       public virtual std::runtime_error
  {
  public:
    openssl_error(): std::runtime_error(""), err_(ERR_get_error())
    { }
    virtual const char * what() const noexcept;
  private:
    unsigned long err_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /**
   * \brief Container for information about the track data itself
   *
   *
   * Construct with an istream positioned just after any ID3v2 tags.  It will
   * scan the track data from the current position to the ID3v1 tag, if any (or
   * the end if no ID3v1 tag is present). After construction, assorted
   * information about the track may be retrieved.
   *
   *
   */

  class track_data {

  public:
    /// mnemonic constant for the # of bytes in an MD5 checksum
    static const std::size_t DIGEST_SIZE = 16;
    /// Construct with an input stream positioned just after any ID3v2 tags; on
    /// return, the stream will be positioned to the first byte after all the
    /// track data has been consumed
    track_data(std::istream &is);
    template <typename forward_input_iterator>
    forward_input_iterator data(forward_input_iterator p0)
    { return std::copy(p0, p0 + DIGEST_SIZE, _md5.begin()); }
    template <typename forward_output_iterator>
    void get_md5(forward_output_iterator p) const
    { std::copy(_md5.begin(), _md5.end(), p); }
    /// Retrieve the # of bytes in the track data
    std::size_t size() const
    { return size_; }

  private:
    std::array<unsigned char, DIGEST_SIZE> _md5;
    std::size_t size_;

  };

}

#endif // not SCRIBBU_H_INCLUDED
