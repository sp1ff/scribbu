#ifndef ID3V2_EDIT_HH_INCLUDED
#define ID3V2_EDIT_HH_INCLUDED

#include <boost/filesystem.hpp>

#include <scribbu/framesv22.hh>
#include <scribbu/framesv23.hh>
#include <scribbu/framesv24.hh>

// TODO(sp1ff): This is my first attempt at editing tags as opposed to simply
// reading them-- I don't see the solution, so I'm writing provisionally in the
// expectation of deleting this & re-writing.

namespace scribbu {

  void
  add_text_frame(const boost::filesystem::path &pth,
                 size_t idx,
                 const char *pframeid,
                 encoding enc,
                 const char *text,
                 bool replace = false);

  void
  add_frame(std::iostream &ios, std::size_t cb, const id3v2_2_frame &frame);

  void
  add_frame(std::iostream &ios, std::size_t cb, const id3v2_3_frame &frame);

  void
  add_frame(std::iostream &ios, std::size_t cb, const id3v2_4_frame &frame);

}

#endif // not ID3V2_EDIT_HH_INCLUDED
