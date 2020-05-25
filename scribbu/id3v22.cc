/**
 * \file id3v22.cc
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#include <id3v22.hh>

#include <algorithm>
#include <numeric>

const char PAD[] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

const size_t CBPAD = sizeof(PAD);


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_2_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef
std::unordered_map<scribbu::frame_id3,
                   scribbu::id3v2_2_tag::generic_frame_parser>
def_generic_reg_type;

typedef
std::unordered_map<scribbu::frame_id3,
                   scribbu::id3v2_2_tag::text_frame_parser>
def_text_reg_type;

static unsigned int nifty_counter_ = 0;

static typename
std::aligned_storage<sizeof(std::mutex), alignof(std::mutex)>::type
mutex_buf_;

static typename
std::aligned_storage< sizeof(def_generic_reg_type),
                      alignof(def_generic_reg_type)>::type
generic_map_buf_;

static typename
std::aligned_storage< sizeof(def_text_reg_type),
                      alignof(def_text_reg_type)>::type
text_map_buf_;

/*static*/ std::mutex&
scribbu::id3v2_2_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_generic_reg_type&
scribbu::id3v2_2_tag::default_generic_parsers_ =
  reinterpret_cast<def_generic_reg_type&>(generic_map_buf_);

/*static*/ def_text_reg_type&
scribbu::id3v2_2_tag::default_text_parsers_ =
  reinterpret_cast<def_text_reg_type&>(text_map_buf_);

scribbu::id3v2_2_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_2_tag::mutex_) std::mutex();
    new (&id3v2_2_tag::default_generic_parsers_) def_generic_reg_type();
    new (&id3v2_2_tag::default_text_parsers_) def_text_reg_type();

#   define REGG(id, tag)                           \
    id3v2_2_tag::default_generic_parsers_.insert(  \
     std::make_pair(frame_id3((id)), tag::create)) \

#   define REGT(id, tag)                           \
    id3v2_2_tag::default_text_parsers_.insert(     \
     std::make_pair(frame_id3((id)), tag::create)) \

    REGG("UFI", UFI);
    REGT("TT1", id3v2_2_text_frame);
    REGT("TT2", id3v2_2_text_frame);
    REGT("TT3", id3v2_2_text_frame);
    REGT("TP1", id3v2_2_text_frame);
    REGT("TP2", id3v2_2_text_frame);
    REGT("TP3", id3v2_2_text_frame);
    REGT("TP4", id3v2_2_text_frame);
    REGT("TCM", id3v2_2_text_frame);
    REGT("TXT", id3v2_2_text_frame);
    REGT("TLA", id3v2_2_text_frame);
    REGT("TCO", id3v2_2_text_frame);
    REGT("TAL", id3v2_2_text_frame);
    REGT("TPA", id3v2_2_text_frame);
    REGT("TRK", id3v2_2_text_frame);
    REGT("TRC", id3v2_2_text_frame);
    REGT("TYE", id3v2_2_text_frame);
    REGT("TDA", id3v2_2_text_frame);
    REGT("TIM", id3v2_2_text_frame);
    REGT("TRD", id3v2_2_text_frame);
    REGT("TMT", id3v2_2_text_frame);
    REGT("TFT", id3v2_2_text_frame);
    REGT("TBP", id3v2_2_text_frame);
    REGT("TCR", id3v2_2_text_frame);
    REGT("TPB", id3v2_2_text_frame);
    REGT("TEN", id3v2_2_text_frame);
    REGT("TSS", id3v2_2_text_frame);
    REGT("TOF", id3v2_2_text_frame);
    REGT("TLE", id3v2_2_text_frame);
    REGT("TSI", id3v2_2_text_frame);
    REGT("TDY", id3v2_2_text_frame);
    REGT("TKE", id3v2_2_text_frame);
    REGT("TOT", id3v2_2_text_frame);
    REGT("TOA", id3v2_2_text_frame);
    REGT("TOL", id3v2_2_text_frame);
    REGT("TOR", id3v2_2_text_frame);
    REGG("TXX", TXX);
    REGG("CNT", CNT);
    REGG("POP", POP);
    REGG("XTG", XTG);
    // N.B. COM intentionally omitted

#   undef REGT
#   undef REGG

  }
}

scribbu::id3v2_2_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_2_tag::mutex_)->~mutex();
    (&id3v2_2_tag::default_generic_parsers_)->~unordered_map();
    (&id3v2_2_tag::default_text_parsers_)->~unordered_map();
  }
}


///////////////////////////////////////////////////////////////////////////////
//                             class id3v2_2_tag                             //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream &is): id3v2_tag(is)
{
  get_default_generic_frame_parsers(
    std::inserter(generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(
    std::inserter(text_parsers_, text_parsers_.begin()));

  // id3v2_tag has consumed the first five bytes of the header-- this call will
  // consume the next five...
  std::size_t size;
  unsigned char flags;
  std::tie(flags, size) = parse_flags_and_size(is);

  unsynchronised(0 != (flags & 0x80));
  compression_ = flags & 0x40;
  parse(is, size);
}

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  compression_(H.flags_ & 0x40)
{
  get_default_generic_frame_parsers(
    std::inserter(generic_parsers_, generic_parsers_.begin()));
  get_default_text_frame_parsers(
    std::inserter(text_parsers_, text_parsers_.begin()));

  parse(is, H.size_);
}

scribbu::id3v2_2_tag::id3v2_2_tag(const id3v2_2_tag &that):
  id3v2_tag(that),
  text_parsers_(that.text_parsers_),
  generic_parsers_(that.generic_parsers_),
  compression_(that.compression_),
  padding_(that.padding_)
{
  for (auto &p: that.frames_) {
    frames_.push_back(std::unique_ptr<id3v2_2_frame>(p->clone()));
    add_frame_to_lookups( *frames_.back(), frames_.size() );
  }
}

scribbu::id3v2_2_tag& scribbu::id3v2_2_tag::operator=(const id3v2_2_tag &that)
{
  if (this != &that) {
    id3v2_tag::operator=(*this);
    text_parsers_ = that.text_parsers_;
    generic_parsers_ = that.generic_parsers_;
    compression_ = that.compression_;
    padding_ = that.padding_;

    coms_.clear();
    cnts_.clear();
    pops_.clear();
    frames_.clear();
    frame_map_.clear();
    text_map_.clear();
    for (auto &p: that.frames_) {
      frames_.push_back(std::unique_ptr<id3v2_2_frame>(p->clone()));
      add_frame_to_lookups( *frames_.back(), frames_.size() );
    }
  }
  return *this;
}

/*virtual*/ unsigned char
scribbu::id3v2_2_tag::flags() const
{
  unsigned char flags = 0;
  boost::optional<bool> unsync = unsynchronised();
  if (unsync && *unsync) {
    flags |= 0x80;
  }
  if (compression_) {
    flags |= 0x40;
  }
  return flags;
}

/////////////////////////////////////////////////////////////////////////////
//                          ID3v2 Serialization                            //
/////////////////////////////////////////////////////////////////////////////

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::size(bool unsync) const
{
  return std::accumulate(begin(), end(), padding(),
                        [unsync](size_t n, const id3v2_2_frame &f)
                        { return n + f.serialized_size(unsync); });
}

/*virtual*/
bool
scribbu::id3v2_2_tag::needs_unsynchronisation() const
{
  return std::any_of(begin(), end(), [](const id3v2_2_frame &f) { return f.needs_unsynchronisation(); });
}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::write(std::ostream &os, bool unsync) const
{
  using namespace std;

  // Write the header... taking care to add the "unsync" flag if we're
  // applying unsynchronisation
  unsigned char f = flags();
  if (unsync) f |= 0x80;
  write_header(os, f, size(unsync));
  // the frames (note we start the accumulation at 10)...
  size_t cb = accumulate(begin(), end(), 10,
                         [&os, unsync](size_t n, const id3v2_2_frame &f)
                         { return n + f.write(os, unsync); });
  // & the padding.
  size_t cbpad = padding();
  while (cbpad) {
    size_t towrite = min(cbpad, CBPAD);
    os.write(PAD, towrite);
    cb += towrite;
    cbpad -= towrite;
  }

  return cb;
}

/////////////////////////////////////////////////////////////////////////////
//                    Frames Common to all ID3v2 Tags                      //
/////////////////////////////////////////////////////////////////////////////

/*virtual*/ std::size_t
scribbu::id3v2_2_tag::play_count() const {
  switch (has_play_count()) {
  case 1:
    return cnts_.front().first->count();
  case 0:
    throw std::logic_error("no play counts");
  default:
    throw std::logic_error("multiple play counts");
  }
}

/*virtual*/
void
scribbu::id3v2_2_tag::add_comment(const std::string &text,
                                  language lang /*= language::from_locale*/,
                                  encoding src /*= encoding::UTF_8*/,
                                  use_unicode unicode /*= use_unicode::no*/,
                                  const std::string &description /*= std::string()*/,
                                  on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::unique_ptr<COM> pnew = std::make_unique<COM>(lang, text, src, unicode, description);

  std::ptrdiff_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(*pnew, d);

}

/*virtual*/
void
scribbu::id3v2_2_tag::add_user_defined_text(
    const std::string &text,
    encoding           src         /*= encoding::UTF_8     */,
    use_unicode        unicode     /*= use_unicode::no     */,
    const std::string &description /*= std::string()       */,
    on_no_encoding     rsp         /*= on_no_encoding::fail*/)
{
  using namespace std;

  unique_ptr<TXX> pnew = make_unique<TXX>(text, src, unicode, description);

  ptrdiff_t d = frames_.size();
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(*pnew, d);
}

/*virtual*/
std::string
scribbu::id3v2_2_tag::text(
    id3v2_text_frames                id,
    encoding                         dst /*= encoding::UTF_8*/,
    on_no_encoding                   rsp /*= on_no_encoding::fail*/,
    const boost::optional<encoding> &src /*= boost::none*/) const
{
  return text_frame_as_str(frame_id3(id), dst, rsp, src);
}

/// Set the contents of an arbitrary text frame
/*virtual*/
void
scribbu::id3v2_2_tag::text(
    id3v2_text_frames  id,
    const std::string &text,
    encoding           src     /*= encoding::UTF_8*/,
    bool               add_bom /*= false*/,
    on_no_encoding     rsp     /*= on_no_encoding::fail*/)
{
  set_text_frame(frame_id3(id), text, src, add_bom, rsp);
}

/// Delete an arbitrary text frame
/*virtual*/
void
scribbu::id3v2_2_tag::delete_frame(id3v2_text_frames id)
{
  frame_id3 id3(id);
  auto p = frame_map_.find(id3);
  if (p != frame_map_.end()) {
    std::ptrdiff_t idx = p->second;
    remove_frame_from_lookups(id3, idx);
  }
}


///////////////////////////////////////////////////////////////////////////////
//                             tag as container                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(mutable_frame_proxy &&that)
{
  using namespace scribbu;

  static const frame_id3 COMID("COM"), CNTID("CNT"), POPID("POP");

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(that.p_->frames_[that.idx_]);
  idx_ = that.idx_;

  id3v2_2_tag::frames_type::iterator pnew = p_->frames_.begin() + idx_;
  frame_id3 id = (**pnew).id();
  if (id == CNTID) {
    CNT &frame = dynamic_cast<CNT&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == COMID) {
    COM &frame = dynamic_cast<COM&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id == POPID) {
    POP &frame = dynamic_cast<POP&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else if (id.text_frame()) {
    id3v2_2_text_frame &frame = dynamic_cast<id3v2_2_text_frame&>(*(pnew->get()));
    p_->add_frame_to_lookups(frame, idx_);
  }
  else {
    id3v2_2_frame &frame = *(pnew->get());
    p_->add_frame_to_lookups(frame, idx_);
  }

  return *this;
}

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(const id3v2_2_frame &frame)
{
  std::unique_ptr<id3v2_2_frame> pnew(frame.clone());

  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  p_->frames_[idx_].swap(pnew);
  p_->add_frame_to_lookups(*(p_->frames_[idx_]), idx_);

  return *this;
}

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(const id3v2_2_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<id3v2_2_text_frame> pnew(new id3v2_2_text_frame(frame));
  // and saving a reference to that copy.
  id3v2_2_text_frame &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(const CNT &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<CNT> pnew(new CNT(frame));
  // and saving a reference to that copy.
  CNT &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(const COM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<COM> pnew(new COM(frame));
  // and saving a reference to that copy.
  COM &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_2_tag::mutable_frame_proxy&
scribbu::id3v2_2_tag::mutable_frame_proxy::operator=(const POP &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  // We begin by taking a deep copy of `frame'...
  std::unique_ptr<POP> pnew(new POP(frame));
  // and saving a reference to that copy.
  POP &F = *pnew;
  // We now remove our current frame from all our tag's lookup tables...
  p_->remove_frame_from_lookups(p_->frames_[idx_]->id(), idx_);
  // move the copy into the slot previously occupied by our frame...
  p_->frames_[idx_] = std::move(pnew);
  // and finally add the copy back into our lookups, but as the const reference we saved.
  p_->add_frame_to_lookups(F, idx_);

  return *this;
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::insert(const_iterator p, const id3v2_2_frame &frame)
{
  std::unique_ptr<id3v2_2_frame> pnew(frame.clone());

  auto p1 = frames_.emplace(frames_.cbegin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(*frames_[d], d);
  return iterator(this, p1);
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::insert(const_iterator p, const id3v2_2_text_frame &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<id3v2_2_text_frame> pnew = std::make_unique<id3v2_2_text_frame>(frame);
  id3v2_2_text_frame &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::insert(const_iterator p, const CNT &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<CNT> pnew = std::make_unique<CNT>(frame);
  CNT &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::insert(const_iterator p, const COM &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<COM> pnew = std::make_unique<COM>(frame);
  COM &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::insert(const_iterator p, const POP &frame)
{
  // This is a little twisted, but it accomplishes two things:
  // 1. Carries out one deep copy of our argument
  // 2. Avoids dynamic_cast

  std::unique_ptr<POP> pnew = std::make_unique<POP>(frame);
  POP &F = *pnew;

  auto p1 = frames_.emplace(frames_.begin() + p.index(), std::move(pnew));
  std::ptrdiff_t d = p1 - frames_.begin();
  add_frame_to_lookups(F, d);

  return iterator(this, p1);
}

void
scribbu::id3v2_2_tag::push_back(const id3v2_2_frame &frame)
{
  std::unique_ptr<id3v2_2_frame> pnew(frame.clone());

  frames_.emplace_back(std::move(pnew));
  std::size_t d = frames_.size() - 1;
  add_frame_to_lookups(*frames_[d], d);
}

void
scribbu::id3v2_2_tag::push_back(const id3v2_2_text_frame &frame)
{
  using namespace std;
  auto pnew = make_unique<id3v2_2_text_frame>(frame);
  id3v2_2_text_frame &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_2_tag::push_back(const CNT &frame)
{
  using namespace std;
  auto pnew = make_unique<CNT>(frame);
  CNT &F = *pnew;
  frames_.emplace_back(std::move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_2_tag::push_back(const COM &frame)
{
  using namespace std;
  auto pnew = make_unique<COM>(frame);
  COM &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

void
scribbu::id3v2_2_tag::push_back(const POP &frame)
{
  using namespace std;
  auto pnew = make_unique<POP>(frame);
  POP &F = *pnew;
  frames_.emplace_back(move(pnew));
  add_frame_to_lookups(F, frames_.size() - 1);
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::erase(const_iterator p)
{
  std::size_t idx = p - begin();
  remove_frame_from_lookups(p->id(), idx);
  return iterator(this, frames_.erase(frames_.begin() + idx));
}

scribbu::id3v2_2_tag::iterator
scribbu::id3v2_2_tag::erase(const_iterator p0, const_iterator p1)
{
  const_iterator p2 = p0;
  for (size_t i = p2 - begin(); p2 != p1; ++p2, ++i) {
    remove_frame_from_lookups(p2->id(), i);
  }

  return iterator(this, frames_.erase(frames_.begin() + p0.index(),
                                      frames_.begin() + p1.index()));
}

std::ostream&
scribbu::id3v2_2_tag::write_header(std::ostream &os,
                                   unsigned char flags,
                                   std::size_t cb) const
{
  unsigned char buf[] = { 'I', 'D', '3', 2, 0, flags };
  os.write((const char*)buf, sizeof(buf));
  detail::sync_safe_from_unsigned(cb, buf);
  os.write((const char*)buf, 4);
  return os;
}

void
scribbu::id3v2_2_tag::remove_frame_from_lookups(const frame_id3 &id, std::size_t idx)
{
  static const frame_id3 COM("COM"), CNT("CNT"), POP("POP");

  if (COM == id) {
    com_frame_lookup_type::iterator p =
      std::find_if(coms_.begin(), coms_.end(),
                   [idx](const com_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    coms_.erase(p);
  }
  else if (CNT == id) {
    cnt_frame_lookup_type::iterator p =
      std::find_if(cnts_.begin(), cnts_.end(),
                   [idx](const cnt_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    cnts_.erase(p);
  }
  else if (POP == id) {
    pop_frame_lookup_type::iterator p =
      std::find_if(pops_.begin(), pops_.end(),
                   [idx](const pop_frame_lookup_type::value_type &x)
                   { return x.second == idx; });
    pops_.erase(p);
  }
  else if (id.text_frame()) {
    const id3v2_2_frame *pf = frames_[idx].get();
    text_frame_lookup_type::iterator p =
      std::find_if(text_map_.begin(), text_map_.end(),
                   [pf, id](const text_frame_lookup_type::value_type &x)
                   { return x.first == id && x.second == pf; });
    if (text_map_.end() == p) {
      throw std::logic_error("Oops");
    }
    text_map_.erase(p);
  }

  frame_lookup_type::iterator pflu =
    std::find_if(frame_map_.begin(), frame_map_.end(),
                 [idx, id](const frame_lookup_type::value_type &x)
                 { return x.first == id && x.second == idx; });
  frame_map_.erase(pflu);
}

void
scribbu::id3v2_2_tag::add_frame_to_lookups(const id3v2_2_frame &frame, std::size_t idx)
{
  using namespace std;
  frame_map_.insert(make_pair(frame.id(), idx));
}

void
scribbu::id3v2_2_tag::add_frame_to_lookups(id3v2_2_text_frame &frame, std::size_t idx)
{
  using namespace std;
  text_map_.insert(make_pair(frame.id(), &frame));
  add_frame_to_lookups((const id3v2_2_frame&)frame, idx);
}

void
scribbu::id3v2_2_tag::add_frame_to_lookups(CNT &frame, std::size_t idx)
{
  using namespace std;
  cnts_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_2_frame&)frame, idx);
}

void
scribbu::id3v2_2_tag::add_frame_to_lookups(COM &frame, std::size_t idx)
{
  using namespace std;
  coms_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_2_frame&)frame, idx);
}

void
scribbu::id3v2_2_tag::add_frame_to_lookups(POP &frame, std::size_t idx)
{
  using namespace std;
  pops_.push_back(make_pair(&frame, idx));
  add_frame_to_lookups((const id3v2_2_frame&)frame, idx);
}


///////////////////////////////////////////////////////////////////////////////

/*static*/ bool
scribbu::id3v2_2_tag::register_default_generic_frame_parser(
  const frame_id3 &id,
  const generic_frame_parser &F)
{
  if (parsing_is_reserved(id)) {
    throw reserved_frame_error(id);
  }
  std::lock_guard<std::mutex> guard(mutex_);
  return default_generic_parsers_.insert(std::make_pair(id, F)).second;
}

/*static*/ bool
scribbu::id3v2_2_tag::register_default_text_frame_parser(
  const frame_id3 &id,
  const text_frame_parser &F)
{
  std::lock_guard<std::mutex> guard(mutex_);
  return default_text_parsers_.insert(std::make_pair(id, F)).second;
}

/*static*/ bool
scribbu::id3v2_2_tag::parsing_is_reserved(const frame_id3 &id)
{
  using namespace std;

  vector<frame_id3> RSVD{ "COM", "PCT", "POP" };

  return RSVD.end() != find(RSVD.begin(), RSVD.end(), id);
}

/**
 * \brief Parse an ID3v2.2 tag after the ID3v2 header
 *
 *
 * \param is [in] An input stream in binary mode whose get pointer is positioned
 * to the first byte of an ID3v2.2 tag after the common ten-byte header
 *
 * \pre The id3v2_tag sub-object has been constructed
 *
 *
 * Tags can be up to 256MB (since the size is 28 bits; cf. "ID3 tag version 2",
 * Sec. 3.1). A naive implementation would keep  two or three copies of the tag
 * in  memory  at  once  during  this  method's  execution:  one  read  buffer,
 * potentially one  re-synchronised copy, and  one set of  frames id3v2_2_frame
 * instances.  With a  little care,  we can  replace the  read buffer  with the
 * re-synchronised buffer before constructing frames, but we're *still* keeping
 * two complete copies of the tag in memory simultaneously. In a multi-threaded
 * situation, where  multiple tags  could be being  constructed simultaneously,
 * this  overhead could  become  significant (imagine  16 threads  constructing
 * 256MB tags-- the memory consumption would be 8Gb instead of 4Gb).
 *
 * Conversely, an implementation  that did multiple reads could  get the memory
 * footprint down  at the cost of  greater code complexity and  worse, multiple
 * file reads, impacting performance.
 *
 * I don't have enough  information to make a decision right  now, so I'm going
 * to opt for  the simpler solution-- read the entire  tag into memory, perhaps
 * re-synchronise, then construct the frames in memory.
 *
 *
 * \todo Re-consider my approach to parsing tags once I have some performance data
 *
 *
 */

void scribbu::id3v2_2_tag::parse(std::istream &is, std::size_t size)
{
  using scribbu::detail::unsigned_from_non_sync_safe;

  static const frame_id3 COM("COM");

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|
                std::ios_base::failbit|
                std::ios_base::badbit);
  // Also, save this so we can restore the stream to its original
  // state.
  std::streampos here = is.tellg();

  try {

    // Size, in bytes, of the tag *after* the header, *before*
    // resynchronisation
    std::size_t cb_tag = size;

    // std::array's size is fixed at compile time, which we can't do, and
    // std::vector is permitted to allocate additional memory to accomodate
    // later insertions.
    std::unique_ptr<unsigned char[]> pb(new unsigned char[cb_tag]);
    is.read((char*)pb.get(), cb_tag);

    // Re-synchronise, if needed. Take care to deallocate the old buffer before
    // constructing any frames.
    boost::optional<bool> unsync = unsynchronised();
    if (unsync && *unsync) {
      cb_tag = resynchronise(pb.get(), cb_tag);
    }

    // Walk the buffer at 'pb', contructing one tag at a time.
    std::size_t cb_frame;
    const unsigned char *p0 = pb.get();
    const unsigned char *p1 = p0 + cb_tag;

    static const frame_id3 PADDING(0, 0, 0);
    for ( ; p0 < p1; p0 += cb_frame + 6) {

      // If we're here, there are three possibilities:

      // 1. There is another frame to be consumed, in which case we should
      // have at least eleven bytes ahead of us: "A frame must be at least 1
      // byte big, excluding the header."-- "ID3 tag version 2.2",
      // sec. 3.2.

      // 2. The tag contains padding after all the frames & we've reached
      // that; "The tag consists of a header, frames and
      // optional padding" -- "ID3 tag version 2.2", sec. 2.0.

      // 3. The tag is corrupt

      std::size_t left = p1 - p0;
      if ( (1 == left && 0 == p0[0]) ||
           (2 == left && 0 == p0[0] && 0 == p0[1]) ) {
        padding_ = left;
        break;
      }

      // left > 2
      frame_id3 id(p0[0], p0[1], p0[2]);
      if (PADDING == id) {
        padding_ = left;
        break;
      }

      if (p0 + 7 > p1) {
        throw invalid_tag();
      }

      // OK-- unpack the frame size...
      cb_frame = unsigned_from_non_sync_safe(p0[3], p0[4], p0[5]);

      // `cb_tag' is the size of the tag on disk (i.e. after encryption,
      // compression & unsynchronisation), exclusive of header & footer (if
      // any). `cb_frame' is the size of the frame on disk (i.e. after...)
      // exclusive of the frame header. IOW, a minimal tag, one with only the
      // standard ID3v2 header, no padding, and this frame, would satisfy:

      //     cb_tag = cb_frame + 6

      // Therefore, we have cb_frame < cb_tag. If this relationship isn't
      // satisfied, it's a good bet that this tag is either corrupt or
      // was written incorrectly.
      if (cb_frame >= cb_tag) {
        throw invalid_tag();
      }

      // Finally, parse the frame (parse_frame will update all our internal
      // datastructures).
      parse_frame(id, p0 + 6, p0 + 6 + cb_frame);

    } // End iteration over frames.

  } catch (const scribbu::error &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw;
  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw;
  }

  is.exceptions(exc_mask);

} // End method id3v2_2_tag::parse.

void
scribbu::id3v2_2_tag::parse_frame(const frame_id3     &id,
                                  const unsigned char *p0,
                                  const unsigned char *p1)
{
  using namespace std;

  static const frame_id3 COMID("COM"), CNTID("CNT"), POPID("POP");

  // COM is handled specially-- the frame parser for "COM" may not
  // be replaced
  if (COMID == id) {
    COM *pcom = new COM(p0, p1);
    coms_.push_back(make_pair(pcom, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_2_frame>(pcom));
  }
  else if (CNTID == id) {
    CNT *pcnt = new CNT(p0, p1);
    cnts_.push_back(make_pair(pcnt, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_2_frame>(pcnt));
  }
  else if (POPID == id) {
    POP *ppop = new POP(p0, p1);
    pops_.push_back(make_pair(ppop, frames_.size()));
    frames_.push_back(unique_ptr<id3v2_2_frame>(ppop));
  }
  else {
    // check to see if we have a text frame parser registered for `id'...
    auto pfn0 = text_parsers_.find(id);
    if (text_parsers_.end() != pfn0) {

      // we do: treat this frame as a text frame. Create an
      // id3v2_2_text_frame...
      auto ptr = pfn0->second(id, p0, p1 - p0);
      // enter the address thereof into our text frame index...
      text_map_.insert(make_pair(id, ptr.get()));
      // and move our ptr-to-id3v2_2_text_frame into our frame collection
      // as a ptr-to-id3v2_2_frame.
      frames_.push_back(unique_ptr<id3v2_2_frame>(std::move(ptr)));

      // NB The spec states that there may only be one text frame in a tag for
      // each identifier; however, counter-examples do exist in the wild.
    }
    else {

      // We do not-- check to see if we have a generic parser registered...
      auto pfn1 = generic_parsers_.find(id);
      if (generic_parsers_.end() == pfn1) {
        // and again we do not-- insert an unknown frame.
        frames_.push_back(
          unique_ptr<id3v2_2_frame>(new unknown_id3v2_2_frame(id, p0, p1)));
      }
      else {
        // We do-- delegate.
        frames_.push_back(pfn1->second(id, p0, p1 - p0));
      }
    }
  }

  // and finally update our id index.
  frame_map_.insert(std::make_pair(id, frames_.size() - 1));
}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string
scribbu::id3v2_2_tag::text_frame_as_str(
  const frame_id3 &id,
  encoding dst /*= encoding::UTF_8*/,
  on_no_encoding rsp /*= on_no_encoding::fail*/,
  const boost::optional<encoding> &src /*= boost::none*/) const
{
  auto p = text_map_.find(id);
  if (p == text_map_.end()) {
    throw unknown_frame_error(id);
  }
  return p->second->as_str<std::string>(dst, rsp, src);
}

void
scribbu::id3v2_2_tag::set_text_frame(const frame_id3 &id,
                                     const std::string &text,
                                     encoding src /*= encoding::UTF_8*/,
                                     bool add_bom /*= false*/,
                                     on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  using namespace std;
  auto p = text_map_.find(id);
  if (p != text_map_.end()) {
    p->second->set(text, src, add_bom, rsp);
  }
  else {
    auto p = make_unique<id3v2_2_text_frame>(id, text, src, add_bom, rsp, false);
    id3v2_2_text_frame &F = *p;
    frames_.push_back(unique_ptr<id3v2_2_text_frame>(move(p)));
    add_frame_to_lookups(F, frames_.size() - 1);
  }
}
