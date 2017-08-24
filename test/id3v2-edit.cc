// #include <scribbu/id3v2-edit.hh>

#include "unit.hh"

#include <boost/filesystem/fstream.hpp>
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

  const string WINAMP("Winamp");

  fs::path datadir = get_data_directory();
  BOOST_TEST_MESSAGE("datadir is " << datadir.c_str());

  // Let's gin up some new text frames....
  id3v2_2_text_frame F22("TEN",  WINAMP, encoding::ASCII, false);
  id3v2_3_text_frame F23("TENC", WINAMP, encoding::ASCII, false);
  id3v2_4_text_frame F24("TENC", WINAMP, encoding::ASCII,
                         id3v2_4_text_frame::frame_encoding::UTF_8,
                         false);

  fs::ifstream ifs(get_data_directory() / "id3v2.2.tag", fs::ifstream::binary);
  BOOST_REQUIRE(ifs.is_open());

  id3v2_2_tag tag22(ifs);
  BOOST_TEST_MESSAGE("Start of test:\n" << tag22);

  /////////////////////////////////////////////////////////////////////////////
  // ID3v2.2(.0) Tag:
  // 2192 bytes, synchronised
  // flags: 0x00
  // Murley Braid Quartet - Sheep Walking
  // Mnemosyne's March (Demo), 2006
  // Content-type (8)
  // Encoded by iTunes v6.0.4
  // TT2: Sheep Walking
  // TP1: Murley Braid Quartet
  // TCM: Mike Murley
  // TAL: Mnemosyne's March (Demo)
  // TYE: 2006
  // TCO: (8)
  // TEN: iTunes v6.0.4
  // COM (iTunNORM):
  //  000006E1 000000D3 00004F8D 00001990 00006729 00001E1A 000064D1 00007E10 00005582 0000DF78
  // COM (iTunSMPB):
  //  00000000 00000210 000007A2 00000000004A6C4E 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000
  // 1802 bytes of padding
  /////////////////////////////////////////////////////////////////////////////

  id3v2_2_tag::mutable_frame_iterator p0, p1;
  p0 = tag22.begin();

  /////////////////////////////////////////////////////////////////////////////
  // What can we do with a normal (mutable) iterator?
  /////////////////////////////////////////////////////////////////////////////

  // dereference to an lvalue then assign
  *p0++ = F22; // takes a copy of `F22'
  BOOST_CHECK(WINAMP == tag22.encoded_by());

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

  BOOST_CHECK(!tag22.needs_unsynchronisation());

  // Test keeping the entire tag the same size
  tag22.padding(2192 - tag22.size() + tag22.padding());
  BOOST_CHECK(2192 == tag22.size());

  fs::path pth(fs::current_path() / "tag22.dat");
  {
    fs::ofstream ofs(pth, fs::ofstream::binary);
    tag22.write(ofs, false);
  }

  fs::ifstream ifs2(pth);
  id3v2_2_tag check(ifs2);
  BOOST_TEST_MESSAGE("After read:\n" << check);

  BOOST_CHECK(2192 == check.size());

  /////////////////////////////////////////////////////////////////////////////
  // Repeat with a const iterator

  id3v2_2_tag::const_iterator p2 = tag22.cbegin();
  // invoke through operator ->
  BOOST_CHECK(frame_id3("TEN") != p2->id());

  // dereference the iterator to a const reference
  const id3v2_2_frame &R2 = *p2;
  BOOST_CHECK(frame_id3("TEN") != R2.id());
  BOOST_CHECK(frame_id3("TEN") != (*p2).id());

  // What about algorithms?
  id3v2_2_tag::const_iterator pp =
    find_if(tag22.cbegin(), tag22.cend(),
            [](const id3v2_2_frame &x) { return x.id() == "TEN"; });
  BOOST_CHECK(tag22.cend() == pp);

  // add_frame(datadir / "opium.mp3",    0, &F23);   // ID3v2.3
  // add_frame(datadir / "红颜旧.mp3",    1, &F24);  // ID3v2.4
}

BOOST_AUTO_TEST_CASE( test_set_frame )
{
  using namespace std;
  using namespace boost;
  using namespace scribbu;

  const string WINAMP("Winamp");

  fs::path datadir = get_data_directory();
  BOOST_TEST_MESSAGE("datadir is " << datadir.c_str());

  fs::ifstream ifs(get_data_directory() / "id3v2.2.tag", fs::ifstream::binary);
  BOOST_REQUIRE(ifs.is_open());

  id3v2_2_tag tag22(ifs);
  BOOST_TEST_MESSAGE("Start of test:\n" << tag22);
  
  tag22.encoded_by(WINAMP);
  BOOST_CHECK(WINAMP == tag22.encoded_by());

}

BOOST_AUTO_TEST_CASE( test_write_frame )
{
  using namespace std;
  using namespace boost;
  using namespace scribbu;

  fs::path datadir = get_data_directory();
  BOOST_TEST_MESSAGE("datadir is " << datadir.c_str());

  fs::path in(get_data_directory() / "id3v2.2.tag");
  unsigned char md5in[16];
  compute_md5(in, md5in);

  fs::ifstream ifs(in, fs::ifstream::binary);
  BOOST_REQUIRE(ifs.is_open());

  id3v2_2_tag tag22(ifs);
  BOOST_TEST_MESSAGE("After read:\n" << tag22);

  fs::path out(fs::current_path() / "test.write.2.2.dat");

  {
    fs::ofstream ofs(out, fs::ofstream::binary);
    BOOST_REQUIRE(!tag22.needs_unsynchronisation());
    tag22.write(ofs, false);
  }

  unsigned char md5out[16];
  compute_md5(out, md5out);

  BOOST_CHECK(0 == memcmp(md5in, md5out, 16));
}



  
