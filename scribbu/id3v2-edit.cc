#include "id3v2-edit.hh"

#include <scribbu/id3v2.hh>

#include <boost/filesystem/fstream.hpp>

namespace fs = boost::filesystem;

// ID3v2.2
// -------

// UNSYNC := true iff the tag is unsynchronised
// REPLACE := true if the frame is to be used as a replacement, false to append
// NEWFRAME := new frame; NEWID is its ID
// FRAMES := lookup table describing the tag
// +----------+------+----------+
// | position | cb   | frame ID |
// +----------+------+----------+
// | p(0)     | 10   |   N/A    | <-- location & size of ID3v2.2 header
// +----------+------+----------+
// | p(1)     | c(1) |  id(1)   | <-- location, size & frame ID of frame #0
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(i)     | c(i) |  id(i)   | <-- location, size & frame ID of frame i-1
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(n)     | c(n) |  id(n)   | <-- location, size & frame ID of frame n-1
// +----------+------+----------+
// | p(n+1)   |c(n+1)|   N/A    | <-- location & size of the tag padding [1]
// +----------+------+----------+

// 1. If the tag has n frames then the table will have n+2 entries

// REPLACE-OR-APPEND
// -----------------

// 1. if not UNSYNC
//    a. serialize NEWFRAME; NEWCB := # of bytes and NEEDSUNSYNC is true
//       IFF NEWFRAME contains false syncs
//    b. if NEEDSUNSYNC, unsynchronise the entire tag in memory, stuff into
//       a stream to be used instead of the input stream
//       TODO: Note the fact that we've re-constructed the tag in memory
//    c. else, note the fact that we have an serialized version of NEWFRAME,
//       and that it had no false syncs
// 2. At this point, STREAM contains the tag we're going to be modifying; Form
//    FRAMES from STREAM
// 3. If UNSYNC, serialize NEWFRAME using unsynchronisation; NEWCB := # of bytes
// 4. Identify the index at which NEWFRAME will be inserted:
//    1) if REPLACE
//       - if there exists i such that 0 < i < n+1, id(i) == NEWID
//         and c(i) >= NEWCB set IDX := i & stop
//       - if NEWCB <= c(n+1) then set IDX := n+1 & stop
//       - if there exists i such that 0 < i < n+1, id(i) == NEWID
//         set IDX := i & stop
//       - set IDX := n+1 & stop
//    2) if not REPLACE, set IDX := n+1
//    If SUM[c(i),i!=IDX] + CBNEW > SUM[c(i),i=0..n+1] then
//    1) the new tag is going to be larger
//       1. if we constructed the source tag in memory...
//          a. re-build the new tag in memory, move the rest of the file back,
//             write the new tag
//       2. else
//          a. move the rest of the file back
//          b. construct the new tag in-place
//    2) we can write the new tag in the same space, perhaps adding padding
//       1. if we constructed the source tag in memory...
//          a. re-build the new tag in memory, adding padding
//          b. copy the new tag over the old one
//       2. else
//          a. construct the new tag in-place

// ID3v2.3
// -------

// UNSYNC := true iff the tag is unsynchronised
// EXTHDR := true iff the tag has an extended header
// CRC := true iff there is a CRC checksum present
// REPLACE := true if the frame is to be used as a replacement, false to append
// NEWFRAME := new frame; NEWID is its ID
// FRAMES := lookup table describing the tag
// +----------+------+----------+
// | position | cb   | frame ID |
// +----------+------+----------+
// | p(0)     | 10   |   N/A    | <-- location & size of ID3v2.2 header
// +----------+------+----------+
// | p(1)     | c(1) |   N/A    | <-- location & size of ID3v2.2 extended header [1]
// +----------+------+----------+
// | p(2)     | c(2) |  id(2)   | <-- location, size & frame ID of frame #0
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(i)     | c(i) |  id(i)   | <-- location, size & frame ID of frame i-2
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(n+1)   |c(n+1)| id(n+1)  | <-- location, size & frame ID of frame n-1
// +----------+------+----------+
// | p(n+2)   |c(n+2)|   N/A    | <-- location & size of the tag padding [2]
// +----------+------+----------+

// 1. If there is no extended header, p(1) shall be equal to p(2) & c(1) shall be zero
// 2. If the tag has n frames then the table will have n+3 entries

// Not so different from the ID3v2.2 case, with the following differences,
// in no particular order:

// - if CRC, we'll need to update it
// - any frame with tag alter preservation set to discard will be removed (and
//   so become a candidate for hosting NEWFRAME)
// - if replacing, and the RO bit is set, clear it.
// - the frame can be compressed, encrypted, and/or grouped

// ID3v2.4
// -------

// UNSYNC := true iff the tag is unsynchronised
// EXTHDR := true iff the tag has an extended header
// FOOTER := true iff there is a footer present
// CRC := true iff there is a CRC checksum present
// REPLACE := true if the frame is to be used as a replacement, false to append
// NEWFRAME := new frame; NEWID is its ID
// FRAMES := lookup table describing the tag
// +----------+------+----------+
// | position | cb   | frame ID |
// +----------+------+----------+
// | p(0)     | 10   |   N/A    | <-- location & size of ID3v2.2 header
// +----------+------+----------+
// | p(1)     | c(1) |   N/A    | <-- location & size of ID3v2.2 extended header [1]
// +----------+------+----------+
// | p(2)     | c(2) |  id(2)   | <-- location, size & frame ID of frame #0
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(i)     | c(i) |  id(i)   | <-- location, size & frame ID of frame i-2
// +----------+------+----------+
//              ...
// +----------+------+----------+
// | p(n+1)   |c(n+1)| id(n+1)  | <-- location, size & frame ID of frame n-1
// +----------+------+----------+
// | p(n+2)   |c(n+2)|   N/A    | <-- location & size of the tag padding/footer [2]
// +----------+------+----------+

// 1. If there is no extended header, p(1) shall be equal to p(2) & c(1) shall be zero
// 2. If the tag has n frames then the table will have n+3 entries

// Not so different from the ID3v2.2 case, with the following differences,
// in no particular order:

// - if CRC, we'll need to update it
// - any frame with tag alter preservation set to discard will be removed (and
//   so become a candidate for hosting NEWFRAME)
// - if replacing, and the RO bit is set, clear it.
// - the frame can be compressed, encrypted, and/or grouped
// - need to respect tag restrictions (extended header)
//   + size
//   + encoding
//   + text field size
//   + image encoding
//   + image size

// class id3v2_2_frame_info
// {
// public:
//   scribbu::frame_id3 id_;
//   std::istream::pos_type beg_;
//   std::size_t cb_;
// };
  
// void add_frame_to_id3v2_2(std::istream &is,
//                           std::size_t cbtag,
//                           bool unsync)
// {
//   using namespace std;

  // using basic_istream::pos_type = pos_type;
  
  // // `is' should have its get pointer positioned at the frame
  // pos_type hdrpos = is.tellg();
  // is.seekg(ID3V2_HEADER_SIZE, basic_istream::cur);

  // vector<id3v2_2_frame_info> frames;

  // // Walk the frames
  // istream::pos_type poshdr = is.tellg();
  // is.seek(ID3V2_HEADER_SIZE, istream::cur);

  // char buf[6];
  // size_t cb_frame = 0;
  // for (size_t cb = 0; cb < cbtag; cb += cb_frame + 6) {

  //   size_t left = cbtag - cb;

  //   size_t n = max(6, left);
  //   is.read(buf, n);

  //   if ( (1 == left && 0 == buf[0]) ||
  //        (2 == left && 0 == buf[0] && 0 == buf[1]) ) {
      

      

  //   if (cb + 6 > cbtag) {
  //   }


  // }
  

  


// }

// add frame `pframe' to the idx-th ID3v2 tag in `pth'
// TODO(sp1ff): Re-factor this interface-- the caller shouldn't be able
// to, say, attempt to add an ID3v2.3 frame to an ID3v2.2 tag.
// void
// scribbu::add_frame(const fs::path &pth, size_t idx, const id3v2_frame *pframe)
// {
//   using namespace scribbu;

//   fs::ifstream ifs(pth, fs::ifstream::binary);

//   // Copy off the stream's exception mask, in case the caller is
//   // counting on it...
//   std::ios_base::iostate exc_mask = ifs.exceptions();
//   // and set it to a value convenient for our use.
//   ifs.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

//   try {

//     // advance to the idx-th ID3v2 tag
//     for (std::size_t i = 0; i < idx; ++i) {
//       id3v2_info I = looking_at_id3v2(ifs, true);
//       if (!I.present_) {
//         throw std::invalid_argument("bad index");
//       }
//       ifs.seekg(I.size_ + scribbu::ID3V2_HEADER_SIZE, fs::ifstream::cur);
//     }

//     id3v2_info I = looking_at_id3v2(ifs, true);
//     if (!I.present_) {
//       throw std::invalid_argument("bad index");
//     }

//     // If we're here, the `ifs' get ptr points to the ID3v2 frame to be
//     // edited. The logic from here on in will depend on the tag version:
//     if (2 == I.version_) {
//       add_frame_to_id3v2_2(ifs, I.size_, 0 != (I.flags_ & 0x80));
//     }
//     else if (3 == I.version_) {
//       throw std::logic_error("not implemented");
//     }
//     else if (4 == I.version_) {
//       throw std::logic_error("not implemented");
//     }
//     else {
//       throw std::logic_error("bad ID3v2 version");
//     }

//   }
//   catch (const std::ios_base::failure &ex) {
//   }
  
//   ifs.exceptions(exc_mask);
//   ifs.clear();

// }


// std::tuple<unique_ptr<istream>,
//            unsigned char,
//            size_t>
// open_id3v2_tag(const fs::path &pth, std::size_t idx)
// {



  
// }

void
scribbu::add_text_frame(const boost::filesystem::path &pth,
                        size_t idx,
                        const char *pframeid,
                        encoding enc,
                        const char *ptext,
                        bool replace /*= false*/)
{
  using namespace std;

  // {pth, idx} -> {ifs, version, revision, cb}
  // unique_ptr<istream> is;
  // unsigned char ver;
  // size_t cb;
  // tie(is, ver, cb) = open_id3v2_tag(pth, idx);

  // if (2 == ver) {

  //   if (3 != strlen(pframeid)) {
  //     throw std::invalid_argument("bad frame ID");
  //   }

  //   frame_id3 id(pframeid);

  //   // TODO(sp1ff): To be (substantially) improved...
  //   bool ucs2 = false;

  //   id3v2_2_text_frame(id, text, enc, ucs2);
  //   add_frame(is, cb, frame);
  // }
  // else if (3 == ver) {
    
  //   // id3v2_3_text_frame(id, text, enc, ucs2,
  //   //                    tap, fap, ro, encmth, grid, decsz);
  //   // add_frame(is, cb, frame);
  //   throw std::logic_error("not implemented");
  // }
  // else if (4 == ver) {
    
  //   // id3v2_4_text_frame(id, text, srcenc, dstenc,
  //   //                    tap, fap, ro, enc, gid, cmp, unsync, dli);
  //   // add_frame(is, cb, frame);
  //   throw std::logic_error("not implemented");
  // }
  // else {
  //   throw std::range_error("unknown ID3v2 version");
  // }

}

