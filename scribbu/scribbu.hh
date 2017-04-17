#ifndef SCRIBBU_H_INCLUDED
#define SCRIBBU_H_INCLUDED 1

#include <iostream>
#include <boost/filesystem.hpp>

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

  class track_data {
  public:

    static const std::size_t DIGEST_SIZE = 16;

    track_data(std::istream &is);

    template <typename forward_input_iterator>
    track_data(forward_input_iterator p0)
    { std::copy(p0, p0 + DIGEST_SIZE, _md5.begin()); }

    template <typename forward_output_iterator>
    void get_md5(forward_output_iterator p) const
    { std::copy(_md5.begin(), _md5.end(), p); }

  private:
    std::array<unsigned char, DIGEST_SIZE> _md5;

  };

} // End namespace scribbu.

#endif // not SCRIBBU_H_INCLUDED
