#include <scribbu/id3v2-edit.hh>

#include "unit.hh"

#include <boost/test/unit_test.hpp>

#include <scribbu/framesv22.hh>
#include <scribbu/framesv23.hh>
#include <scribbu/framesv24.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v2-utils.hh>

#include <scribbu/pprinter.hh>

namespace fs = boost::filesystem;

BOOST_AUTO_TEST_CASE( test_add_frame )
{
  using namespace std;
  using namespace boost;
  using namespace scribbu;


  std::string buf;
  buf.reserve(5);
  buf[0] = 0xba;
  buf[1] = 0xbe;
  buf[2] = 0;
  buf[3] = 0xba;
  buf[4] = 0xbe;

  std::stringstream stm;
  stm.rdbuf()->str(buf);
  stm.seekg(0);

  char buf2[5];
  stm.read(buf2, 5);
  

  const std::string WINAMP("Winamp");

  fs::path datadir = get_data_directory();
  BOOST_CHECK(datadir == fs::path("/vagrant/test/data"));
  BOOST_TEST_MESSAGE(datadir.c_str());

  id3v2_2_text_frame F22("TEN",  WINAMP, encoding::ASCII, false);
  id3v2_3_text_frame F23("TENC", WINAMP, encoding::ASCII, false,
                         id3v2_3_text_frame::tag_alter_preservation::preserve,
                         id3v2_3_text_frame::file_alter_preservation::preserve,
                         id3v2_3_text_frame::read_only::clear,
                         boost::none, boost::none, boost::none);
  id3v2_4_text_frame F24("TENC", WINAMP, encoding::ASCII,
                         id3v2_4_text_frame::frame_encoding::UTF_8,
                         id3v2_4_text_frame::tag_alter_preservation::preserve,
                         id3v2_4_text_frame::file_alter_preservation::preserve,
                         id3v2_4_text_frame::read_only::clear,
                         boost::none, boost::none,
                         false, false,
                         boost::none);

  // fs::ifstream ifs(datadir / "id3v2.2.tag", fs::ifstream::binary);
  fs::ifstream ifs(get_data_directory() / "id3v2.2.tag", fs::ifstream::binary);
  BOOST_REQUIRE(ifs.is_open());

  // TODO(sp1ff): Doesn't work-- why?
  // unique_ptr<id3v2_tag> ptag = read_id3v2(ifs, 0);

  id3v2_2_tag tag22(ifs);

  std::stringstream stm0;
  stm0 << tag22;
  BOOST_TEST_MESSAGE("Start of test:");
  BOOST_TEST_MESSAGE(stm0.str());
  
  id3v2_2_tag::mutable_frame_iterator p0, p1;
  p0 = tag22.begin();

  /////////////////////////////////////////////////////////////////////////////
  // What can we do with a normal (mutable) iterator?
  
  // dereference to an lvalue then assign
  *p0++ = F22; // takes a copy of `F22'

  // random-access
  p0[1] = F22; // takes a copy

  // invoke through operator ->
  BOOST_CHECK(frame_id3("TEN") != p0->id());

  // dereference the iterator to a non-const reference
  id3v2_2_frame &R1 = *p0;
  BOOST_CHECK(frame_id3("TEN") != R1.id());

  // What about algorithms?
  id3v2_2_tag::mutable_frame_iterator ppp =
    find_if(tag22.begin(), tag22.end(),
            [](const id3v2_2_frame &x) { return x.id() == "TEN"; });
  BOOST_CHECK(ppp != tag22.end());

  std::ptrdiff_t nframes = tag22.end() - tag22.begin();
  BOOST_CHECK(nframes == 9);
  
  std::stringstream stm1;
  stm1 << tag22;
  BOOST_TEST_MESSAGE("Prior to remove_if:");
  BOOST_TEST_MESSAGE(stm1.str());
  
  ppp = remove_if(tag22.begin(), tag22.end(),
                  [](const id3v2_2_frame &x) { return x.id() == "TEN"; });
  nframes = ppp - tag22.begin();
  BOOST_CHECK(nframes == 6);

  tag22.erase(ppp, tag22.end());
  BOOST_CHECK(6 == tag22.num_frames());

  std::stringstream stm2;
  stm2 << tag22;
  BOOST_TEST_MESSAGE("After remove_if:");
  BOOST_TEST_MESSAGE(stm2.str());

  /////////////////////////////////////////////////////////////////////////////
  // Repeat with a const iterator

  id3v2_2_tag::const_frame_iterator p2 = tag22.cbegin();
  // invoke through operator ->
  BOOST_CHECK(frame_id3("TEN") != p2->id());

  // dereference the iterator to a const reference
  const id3v2_2_frame &R2 = *p2;
  BOOST_CHECK(frame_id3("TEN") != R2.id());
  BOOST_CHECK(frame_id3("TEN") != (*p2).id());

  // What about algorithms?
  id3v2_2_tag::const_frame_iterator pp =
    find_if(tag22.cbegin(), tag22.cend(),
            [](const id3v2_2_frame &x) { return x.id() == "TEN"; });
  BOOST_CHECK(tag22.cend() == pp);
  
  

  // add_frame(datadir / "/id3v2.2.tag", 0, &F22);   // ID3v2.2
  // add_frame(datadir / "opium.mp3",    0, &F23);   // ID3v2.3
  // add_frame(datadir / "红颜旧.mp3",    1, &F24);  // ID3v2.4
}
