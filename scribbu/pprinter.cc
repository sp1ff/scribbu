/**
 * \file pprinter.cc
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#include "scribbu/pprinter.hh"

#include <typeindex>
#include <unordered_map>

#include <scribbu/ostream.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/framesv22.hh>
#include <scribbu/framesv23.hh>
#include <scribbu/framesv24.hh>

using scribbu::pprinter;


///////////////////////////////////////////////////////////////////////////////
//                              dual dispatcher                              //
///////////////////////////////////////////////////////////////////////////////

/**
 * \class pprint_dispatcher
 *
 * \brief Dual dispatcher for struct pprinter
 *
 *
 * When pretty-printing ID3v2 tags & frames, we would like to dispatch
 * on two parameters:
 *
 *   - the pprinter subtype
 *   - the id3v2_tag subtype
 *
 * C++, of course, only provides for dispatch on one type at a time,
 * through the virtual function mechanism. Using techniques I gleaned
 * from \ref ref_01 Alexandrescu, I've setup a dual dispatcher here.
 *
 * Constructing an instance of this class will register all ID3v2 tag
 * and frame overloads in struct pprinter. At runtime, invoke this
 * class (i.e. call operator()) with a type_info reference naming the
 * parameter type. You will receive in return a pointer to a function
 * returning void & taking a pointer to pprinter, a const reference to
 * the relevant type, and an ostream that dereferences to the proper
 * implementation.
 *
 *
 */

class pprint_dispatcher
{
public:

  typedef std::ostream&
  (*tag_dispatch_type)(pprinter*, const scribbu::id3v2_tag&, std::ostream&);

  typedef std::ostream&
  (*frame_v22_dispatch_type)(pprinter*, const scribbu::id3v2_2_frame&,
                             std::ostream&);

  typedef std::ostream&
  (*frame_v23_dispatch_type)(pprinter*, const scribbu::id3v2_3_frame&,
                             std::ostream&);

  typedef std::ostream&
  (*frame_v24_dispatch_type)(pprinter*, const scribbu::id3v2_4_frame&,
                             std::ostream&);

public:
  // Ctor will register all pointers-to-virtual members
  pprint_dispatcher();

  // Register tag pointers-to-virtual
  template <typename parameter,
            std::ostream& (pprinter::*pfn)(const parameter &,
                                                    std::ostream&)>
  void reg_tag()
  {
    using scribbu::id3v2_tag;
    struct local {
      static std::ostream&
      trampoline(pprinter *pp, const id3v2_tag &t, std::ostream &os) {
        return (pp->*pfn)(dynamic_cast<const parameter&>(t), os);
      }
    };
    tags_[std::type_index(typeid(parameter))] = &local::trampoline;
  }
  // Register v2.2 frame pointers-to-virtual
  template <typename parameter,
            std::ostream& (pprinter::*pfn)(const parameter &,
                                           std::ostream&)>
  void reg_v22_frame()
  {
    using scribbu::id3v2_2_frame;
    struct local {
      static std::ostream&
      trampoline(pprinter *pp, const id3v2_2_frame &f, std::ostream &os) {
        return (pp->*pfn)(dynamic_cast<const parameter&>(f), os);
      }
    };
    v22_frames_[std::type_index(typeid(parameter))] = &local::trampoline;
  }

  // Register v2.3 frame pointers-to-virtual
  template <typename parameter,
            std::ostream& (pprinter::*pfn)(const parameter &,
                                           std::ostream&)>
  void reg_v23_frame()
  {
    using scribbu::id3v2_3_frame;
    struct local {
      static std::ostream&
      trampoline(pprinter *pp, const id3v2_3_frame &f, std::ostream &os) {
        return (pp->*pfn)(dynamic_cast<const parameter&>(f), os);
      }
    };
    v23_frames_[std::type_index(typeid(parameter))] = &local::trampoline;
  }

  // Register v2.4 frame pointers-to-virtual
  template <typename parameter,
            std::ostream& (pprinter::*pfn)(const parameter &,
                                           std::ostream&)>
  void reg_v24_frame()
  {
    using scribbu::id3v2_4_frame;
    struct local {
      static std::ostream&
      trampoline(pprinter *pp, const id3v2_4_frame &f, std::ostream &os) {
        return (pp->*pfn)(dynamic_cast<const parameter&>(f), os);
      }
    };
    v24_frames_[std::type_index(typeid(parameter))] = &local::trampoline;
  }

  tag_dispatch_type
  tag_impl(const std::type_info &ti) const
  {
    return tags_.at(std::type_index(ti));
  }

  frame_v22_dispatch_type
  frame_v22_impl(const std::type_info &ti) const
  {
    return v22_frames_.at(std::type_index(ti));
  }

  frame_v23_dispatch_type
  frame_v23_impl(const std::type_info &ti) const
  {
    return v23_frames_.at(std::type_index(ti));
  }

  frame_v24_dispatch_type
  frame_v24_impl(const std::type_info &ti) const
  {
    return v24_frames_.at(std::type_index(ti));
  }

private:
  std::unordered_map<std::type_index, tag_dispatch_type> tags_;
  std::unordered_map<std::type_index, frame_v22_dispatch_type> v22_frames_;
  std::unordered_map<std::type_index, frame_v23_dispatch_type> v23_frames_;
  std::unordered_map<std::type_index, frame_v24_dispatch_type> v24_frames_;

}; // End class pprint_disptacher.

pprint_dispatcher::pprint_dispatcher()
{
  using namespace scribbu;

  reg_tag<id3v2_2_tag, &pprinter::pprint_v2_2_tag>();
  reg_tag<id3v2_3_tag, &pprinter::pprint_v2_3_tag>();
  reg_tag<id3v2_4_tag, &pprinter::pprint_v2_4_tag>();
  reg_v22_frame<unknown_id3v2_2_frame, &pprinter::pprint_unk_id3v2_2_frame>();
  reg_v22_frame<id3v2_2_text_frame, &pprinter::pprint_id3v2_2_text_frame>();
  reg_v22_frame<UFI, &pprinter::pprint_UFI>();
  reg_v22_frame<TXX, &pprinter::pprint_TXX>();
  reg_v22_frame<COM, &pprinter::pprint_COM>();
  reg_v22_frame<CNT, &pprinter::pprint_CNT>();
  reg_v22_frame<POP, &pprinter::pprint_POP>();
  reg_v22_frame<XTG, &pprinter::pprint_XTG>();
  reg_v23_frame<unknown_id3v2_3_frame, &pprinter::pprint_unk_id3v2_3_frame>();
  reg_v23_frame<id3v2_3_text_frame, &pprinter::pprint_id3v2_3_text_frame>();
  reg_v23_frame<UFID, &pprinter::pprint_UFID>();
  reg_v23_frame<ENCR, &pprinter::pprint_ENCR>();
  reg_v23_frame<TXXX, &pprinter::pprint_TXXX>();
  reg_v23_frame<COMM, &pprinter::pprint_COMM>();
  reg_v23_frame<PCNT, &pprinter::pprint_PCNT>();
  reg_v23_frame<POPM, &pprinter::pprint_POPM>();
  reg_v23_frame<XTAG, &pprinter::pprint_XTAG>();
  reg_v24_frame<unknown_id3v2_4_frame, &pprinter::pprint_unk_id3v2_4_frame>();
  reg_v24_frame<id3v2_4_text_frame, &pprinter::pprint_id3v2_4_text_frame>();
  reg_v24_frame<UFID_2_4, &pprinter::pprint_UFID_2_4>();
  reg_v24_frame<ENCR_2_4, &pprinter::pprint_ENCR_2_4>();
  reg_v24_frame<TXXX_2_4, &pprinter::pprint_TXXX_2_4>();
  reg_v24_frame<COMM_2_4, &pprinter::pprint_COMM_2_4>();
  reg_v24_frame<PCNT_2_4, &pprinter::pprint_PCNT_2_4>();
  reg_v24_frame<POPM_2_4, &pprinter::pprint_POPM_2_4>();
  reg_v23_frame<XTAG_2_4, &pprinter::pprint_XTAG_2_4>();

}

thread_local pprint_dispatcher D;


///////////////////////////////////////////////////////////////////////////////
//                     class pprint_manipulator                              //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
scribbu::pprint_manipulator::~pprint_manipulator()
{
  delete pp_;
}

/*friend*/ std::ostream&
scribbu::operator<<(std::ostream &os, const pprint_manipulator &x)
{
  typedef scribbu::detail::trivial_sentry<char> sentry_type;
  typedef manip_helper<const pprint_manipulator, std::ostream, sentry_type>
    impl_type;

  impl_type::do_manip(x, &pprint_manipulator::do_manip, os);

  return os;
}

scribbu::pprinter*
scribbu::pprint_manipulator::take() const
{
  pprinter *p = pp_;
  pp_ = nullptr;
  return p;
}

/*static*/ int
scribbu::pprint_manipulator::index()
{
  static const int idx = std::ios_base::xalloc();
  return idx;
}

/*static*/ void
scribbu::pprint_manipulator::do_manip(const pprint_manipulator &manip,
                                      std::ostream &os)
{
  int idx = index();
  // Retrieve the current pword value...
  void *&p = os.pword(idx);
  // if it's null, then this stream has not yet been imbued with a
  // pretty printer; register our callback.
  if (nullptr == p) {
    os.register_callback(pprint_manipulator::callback, idx);
  }
  // nil or otherwise, regard this as the "old" pretty printer &
  // make a copy..
  pprinter *pold = reinterpret_cast<pprinter*>(p);
  // copy the instance in `c' *into* the stream...
  p = manip.take();
  // and, finally, clean up the old pretty printer:
  delete pold;
}

/*static*/ void
scribbu::pprint_manipulator::callback(std::ios_base::event e,
                                      std::ios_base &ios,
                                      int idx)
{
  using namespace std;

  if (ios_base::erase_event == e) {
    try {
      delete reinterpret_cast<pprinter*>(ios.pword(idx));
    }
    catch (...) {
      // Nothing to be done-- either the stream is being destroyed, or
      // we're at the beginning of a copyfmt call. In the former case
      // there'll be no one to read the iword, and the latter it will
      // be overwritten before we can read it, anyway.
    }
  }
  else if (ios_base::copyfmt_event == e) {
    void *&p = ios.pword(idx);
    pprinter *pold = reinterpret_cast<pprinter*>(p);
    if (nullptr != pold) {
      try {
        p = pold->clone();
      }
      catch (...) {
        // We can't throw, and we have no return code. The only thing
        // we can do is use the iword:
        int old = ios.iword(idx);
        ios.iword(idx) = old | ios_base::badbit;
      }
    }
  }
}

namespace {

  pprinter*
  get_pretty_printer(std::ostream &os)
  {
    using namespace scribbu;

    static standard_pprinter SP;

    pprinter *pp = &SP;

    void *&p = os.pword(pprint_manipulator::index());
    if (nullptr != p) {
      pp = reinterpret_cast<pprinter*>(p);
    }

    return pp;
  }

}


///////////////////////////////////////////////////////////////////////////////
//                     class standard_pprinter                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ const scribbu::encoding
scribbu::standard_pprinter::DEFAULT_V1_ENCODING  = scribbu::encoding::ASCII;

/*static*/ const boost::optional<scribbu::encoding>
scribbu::standard_pprinter::DEFAULT_V2_ENCODING = boost::none;


/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_v2_2_tag(const id3v2_2_tag &tag,
                                            std::ostream &os)
{
  using namespace std;

  print_id3v2_tag(tag, os);
  for (auto& p: tag) {
    os << p;
  }

  return os << sin_ << dec << tag.padding() << " bytes of padding\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_v2_3_tag(const id3v2_3_tag &tag,
                                            std::ostream &os)
{
  using namespace std;

  print_id3v2_tag(tag, os);
  for (auto& p: tag) {
    os << p;
  }

  return os << sin_ << dec << tag.padding() << " bytes of padding\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_v2_4_tag(const id3v2_4_tag &tag,
                                            std::ostream &os)
{
  using namespace std;

  print_id3v2_tag(tag, os);
  for (auto& p: tag) {
    os << p;
  }

  return os << sin_ << dec << tag.padding() << " bytes of padding\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_track_data(const track_data &data,
                                              std::ostream &os)
{
  using namespace std;

  os << sin_ << dec << data.size() << " bytes of track data:\n" <<
    sin_ << "MD5: " << hex << setfill('0');

  vector<unsigned char> md5;
  data.get_md5(back_inserter(md5));

  for (auto x: md5) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_v1_tag(const id3v1_tag &tag,
                                          std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << sin_ << "ID3v1";
  if (tag.v1_1()) {
    os << ".1";
  }
  if (tag.enhanced()) {
    os << " (enhanced)";
  }
  os << ": " << tag.artist<string>(v1enc_, dst, rsp) << " - " <<
    tag.title<string>(v1enc_, dst, rsp) << "\n" << sin_<<
    tag.album<string>(v1enc_, dst, rsp);

  bool present;
  unsigned char trackno;
  std::tie(present, trackno) = tag.track_number();
  if (present) {
    os <<" (track " << dec << static_cast<unsigned int>(trackno) << ")";
  }
  os << ", " << tag.year<string>(v1enc_, dst, rsp) << "\n" << sin_<<
    tag.comment<string>(v1enc_, dst, rsp) << "\n" << sin_;

  if (tag.enhanced()) {
    os << tag.enh_genre<string>(v1enc_, dst, rsp) << "/";
    if (expand_genre_) {
      boost::optional<string> x = id3v1_tag::text_for_genre(tag.genre());
      if (x) {
        os << x.get();
      } else {
        os << "unrecognized genre";
      }
    }
    else {
      os << "genre " << dec << static_cast<unsigned int>(tag.genre());
    }
    os << "\n" << sin_;

    unsigned char speed;
    std::tie(present, speed) = tag.speed(); // present ignored
    os << tag.start_time<string>(v1enc_, dst, rsp) << "-" <<
      tag.end_time<string>(v1enc_, dst, rsp) << ": speed " << dec <<
      static_cast<unsigned int>(speed) << "\n";
  }
  else {
    if (expand_genre_) {
      boost::optional<string> x = id3v1_tag::text_for_genre(tag.genre());
      if (x) {
        os << x.get();
      } else {
        os << "unknown genre " << dec << static_cast<unsigned int>(tag.genre());
      }
    }
    else {
      os << "genre " << dec << static_cast<unsigned int>(tag.genre());
    }
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_unk_id3v2_2_frame(
  const unknown_id3v2_2_frame &frame,
  std::ostream &os)
{
  return os << sin_ << frame.id() << ": UNKNOWN\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_id3v2_2_text_frame(
  const id3v2_2_text_frame &frame,
  std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << sin_ << frame.id() << ": " << frame.as_str<string>(dst, rsp, v2enc_) <<
    "\n";

  return os;
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_UFI(const UFI &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner<string>() << "\n";

  vector<unsigned char> buf;
  frame.idb(back_inserter(buf));

  os << sin_ << "     " << hex << setfill('0');
  for (auto x: buf) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_TXX(const TXX &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << frame.description<string>() <<
    "\n" << sin_ << frame.text<string>() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_COM(const COM &frame, std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  string dsc = frame.description<string>(dst, rsp, v2enc_);
  string text = frame.text<string>(dst, rsp, v2enc_);

  if (dsc.empty()) {
    dsc = "<no description>";
  }

  if (text.empty()) {
    text = "<no text>";
  }

  return os << frame.id() << " (" << dsc << "):\n" << text << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_CNT(const CNT &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << dec <<
    (unsigned)frame.count() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_POP(const POP &frame, std::ostream &os)
{
  using namespace std;

  vector<unsigned char> counter;
  frame.counterb(back_inserter(counter));

  os << sin_ << frame.id() << ": " << frame.email<string>() << "\n" <<
    sin_ << "rating: " << dec << (unsigned)frame.rating() << "\n" <<
    sin_ << "counter: ";

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_XTG(const XTG &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner() << "\n" <<
    sin_ << "tags:\n";
  
  for (auto x: frame) {
    os << sin_ << x.first << ": ";
    bool first = true;
    for (auto val: x.second) {
      if (first) {
        first = false;
      } else {
        os << ", ";
      }
      os << val;
    }
    os << "\n";
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_unk_id3v2_3_frame(
  const unknown_id3v2_3_frame &frame,
  std::ostream &os)
{
  using namespace std;
  return os << "frame " << frame.id() << " (" << dec << frame.size() <<
    " bytes)\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_id3v2_3_text_frame(
  const id3v2_3_text_frame &frame,
  std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << sin_ << frame.id() << ": " << frame.as_str<string>(dst, rsp, v2enc_) <<
    "\n";

  return os;
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_UFID(const UFID &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner<string>() << "\n";

  vector<unsigned char> buf;
  frame.idb(back_inserter(buf));

  os << sin_ << "      " << hex << setfill('0');
  for (auto x: buf) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_ENCR(const ENCR &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.email<string>() << "\n" <<
    sin_ << "Method symbol: " << dec << frame.method_symbol() << "\n" <<
    sin_ << "Encryption data: " << hex << setfill('0');

  vector<unsigned char> data;
  frame.datab(back_inserter(data));
  for (auto x: data) {
    os << setw(2) << x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_TXXX(const TXXX &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << frame.description<string>() <<
    "\n" << sin_ << frame.text<string>() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_COMM(const COMM &frame, std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  string dsc = frame.description<string>(dst, rsp, v2enc_);
  string text = frame.text<string>(dst, rsp, v2enc_);

  if (dsc.empty()) {
    dsc = "<no description>";
  }

  if (text.empty()) {
    text = "<no text>";
  }

  return os << frame.id() << " (" << dsc << "):\n" << text << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_PCNT(const PCNT &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << dec <<
    (unsigned)frame.count() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_POPM(const POPM &frame, std::ostream &os)
{
  using namespace std;

  vector<unsigned char> counter;
  frame.counterb(back_inserter(counter));

  os << sin_ << frame.id() << ": " << frame.email<string>() << "\n" <<
    sin_ << "rating: " << dec << (unsigned)frame.rating() << "\n" <<
    sin_ << "counter: ";

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_XTAG(const XTAG &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner() << "\n" <<
    sin_ << "tags:\n";
  
  for (auto x: frame) {
    os << sin_ << x.first << ": ";
    bool first = true;
    for (auto val: x.second) {
      if (first) {
        first = false;
      } else {
        os << ", ";
      }
      os << val;
    }
    os << "\n";
  }

  return os;
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_unk_id3v2_4_frame(
  const unknown_id3v2_4_frame &frame, std::ostream &os)
{
  using namespace std;
  return os << "frame " << frame.id() << " (" << dec << frame.size() <<
    " bytes)\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_id3v2_4_text_frame(
  const id3v2_4_text_frame &frame,
  std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  os << sin_ << frame.id() << ": " << frame.as_str<string>(dst, rsp, v2enc_) <<
    "\n";

  return os;
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_UFID_2_4(const UFID_2_4 &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner<string>() << "\n";

  vector<unsigned char> buf;
  frame.idb(back_inserter(buf));

  os << sin_ << "      " << hex << setfill('0');
  for (auto x: buf) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_ENCR_2_4(const ENCR_2_4 &frame, std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.email<string>() << "\n" <<
    sin_ << "Method symbol: " << dec << frame.method_symbol() << "\n" <<
    sin_ << "Encryption data: " << hex << setfill('0');

  vector<unsigned char> data;
  frame.datab(back_inserter(data));
  for (auto x: data) {
    os << setw(2) << x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_TXXX_2_4(const TXXX_2_4 &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << frame.description<string>() <<
    "\n" << sin_ << frame.text<string>() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_COMM_2_4(const COMM_2_4 &frame, std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  string dsc = frame.description<string>(dst, rsp, v2enc_);
  string text = frame.text<string>(dst, rsp, v2enc_);

  if (dsc.empty()) {
    dsc = "<no description>";
  }

  if (text.empty()) {
    text = "<no text>";
  }

  return os << frame.id() << " (" << dsc << "):\n" << text << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_PCNT_2_4(const PCNT_2_4 &frame, std::ostream &os)
{
  using namespace std;
  return os << sin_ << frame.id() << ": " << dec <<
    (unsigned)frame.count() << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_POPM_2_4(const POPM_2_4 &frame, std::ostream &os)
{
  using namespace std;

  vector<unsigned char> counter;
  frame.counterb(back_inserter(counter));

  os << sin_ << frame.id() << ": " << frame.email<string>() << "\n" <<
    sin_ << "rating: " << dec << (unsigned)frame.rating() << "\n" <<
    sin_ << "counter: ";

  os << hex << setfill('0');
  for (auto x: counter) {
    os << setw(2) << (unsigned)x;
  }

  return os << "\n";
}

/*virtual*/ std::ostream&
scribbu::standard_pprinter::pprint_XTAG_2_4(const XTAG_2_4 &frame, 
                                            std::ostream &os)
{
  using namespace std;

  os << sin_ << frame.id() << ": " << frame.owner() << "\n" <<
    sin_ << "tags:\n";
  
  for (auto x: frame) {
    os << sin_ << x.first << ": ";
    bool first = true;
    for (auto val: x.second) {
      if (first) {
        first = false;
      } else {
        os << ", ";
      }
      os << val;
    }
    os << "\n";
  }

  return os;
}

void
scribbu::standard_pprinter::print_id3v2_tag(const id3v2_tag &tag,
                                            std::ostream &os)
{
  using namespace std;

  ienc stream_enc = ienc::retrieve(os);
  encoding dst = stream_enc.get_encoding();
  on_no_encoding rsp = stream_enc.get_on_no_encoding();

  // Boilerplate:
  //   - ID3v2 version & revision, size, flags, unsync
  //   - artist - title
  //   - album (track), year
  //   - content-type
  //   - encoded-by

  string artist = tag.has_artist() ? tag.artist(dst, rsp, v2enc_) :
    "<no artist>";
  string title = tag.has_title() ? tag.title(dst, rsp, v2enc_) :
    "<no title>";
  string album = tag.has_album() ? tag.album(dst, rsp, v2enc_) :
    "<no album>";
  string year = tag.has_year() ? tag.year(dst, rsp, v2enc_) :
    "<no year>";

  boost::optional<bool> unsync = tag.unsynchronised();
  os << sin_ << "ID3v2." << dec << (unsigned)tag.version() << "(." <<
    (unsigned)tag.revision() << ") Tag:\n" <<
    sin_ << tag.size() << " bytes, " <<
    ((unsync && *unsync) ? "unsynchronised" : "synchronised") << "\n" <<
    sin_ << "flags: 0x" << hex << setfill('0') << setw(2) <<
    (unsigned) tag.flags() << "\n" << sin_ << artist << " - " <<
    title << "\n" << sin_ << album;

  if (tag.has_track()) {
    os << " (track " << tag.track(dst, rsp, v2enc_) << ")";
  }

  os << ", " << year << "\n";

  if (tag.has_content_type()) {
    os << sin_ << "Content-type " << tag.content_type(dst, rsp, v2enc_) << "\n";
  }

  if (tag.has_encoded_by()) {
    os << sin_ << "Encoded by " << tag.encoded_by(dst, rsp, v2enc_) << "\n";
  }

}


std::ostream&
scribbu::operator<<(std::ostream &os, const id3v2_tag &tag)
{
  pprinter *pp = get_pretty_printer(os);
  return D.tag_impl(typeid(tag))(pp, tag, os);
}

std::ostream& scribbu::operator<<(std::ostream &os, const track_data &data)
{
  return get_pretty_printer(os)->pprint_track_data(data, os);
}

std::ostream& scribbu::operator<<(std::ostream &os, const id3v1_tag &tag)
{
  return get_pretty_printer(os)->pprint_v1_tag(tag, os);
}

std::ostream& scribbu::operator<<(std::ostream &os, const id3v2_2_frame &frame)
{
  pprinter *pp = get_pretty_printer(os);
  return D.frame_v22_impl(typeid(frame))(pp, frame, os);
}

std::ostream& scribbu::operator<<(std::ostream &os, const id3v2_3_frame &frame)
{
  pprinter *pp = get_pretty_printer(os);
  return D.frame_v23_impl(typeid(frame))(pp, frame, os);
}

std::ostream& scribbu::operator<<(std::ostream &os, const id3v2_4_frame &frame)
{
  pprinter *pp = get_pretty_printer(os);
  return D.frame_v24_impl(typeid(frame))(pp, frame, os);
}

/*static*/
std::tuple<scribbu::encoding, scribbu::on_no_encoding>
scribbu::pprinter::encoding_from_stream(std::ostream &os)
{
  ienc e = ienc::retrieve(os);
  scribbu::encoding enc = e.get_encoding();
  if (!scribbu::char_traits<char>::is_code_unit(enc)) {
    throw bad_code_unit(enc, 1);
  }
  return std::make_tuple(enc, e.get_on_no_encoding());
}

unsigned int
scribbu::pprinter::optional_to_uint(const boost::optional<bool> &x)
{
  if (x) {
    return *x ? 1 : 0;
  }
  else {
    return ~0;
  }
}
