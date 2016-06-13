#include <id3v22.hh>


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_2_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef std::unordered_map<scribbu::frame_id3, scribbu::id3v2_2_tag::frame_parser> def_reg_type;

static unsigned int nifty_counter_ = 0;

static typename
std::aligned_storage<sizeof(std::mutex), alignof(std::mutex)>::type
mutex_buf_;

static typename
std::aligned_storage< sizeof(def_reg_type), alignof(def_reg_type)>::type
map_buf_;

/*static*/ std::mutex&
scribbu::id3v2_2_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_reg_type&
scribbu::id3v2_2_tag::default_parsers_ =
  reinterpret_cast<def_reg_type&>(map_buf_);

scribbu::id3v2_2_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_2_tag::mutex_) std::mutex();
    new (&id3v2_2_tag::default_parsers_) def_reg_type();

#   define REG(id, tag) \
    id3v2_2_tag::default_parsers_.insert(std::make_pair(frame_id3((id)), tag::create)) \

    REG("UFI", UFI);
    REG("TT1", id3v2_2_text_frame);
    REG("TT2", id3v2_2_text_frame);
    REG("TT3", id3v2_2_text_frame);
    REG("TP1", id3v2_2_text_frame);
    REG("TP2", id3v2_2_text_frame);
    REG("TP3", id3v2_2_text_frame);
    REG("TP4", id3v2_2_text_frame);
    REG("TCM", id3v2_2_text_frame);
    REG("TXT", id3v2_2_text_frame);
    REG("TLA", id3v2_2_text_frame);
    REG("TCO", id3v2_2_text_frame);
    REG("TAL", id3v2_2_text_frame);
    REG("TPA", id3v2_2_text_frame);
    REG("TRK", id3v2_2_text_frame);
    REG("TRC", id3v2_2_text_frame);
    REG("TYE", id3v2_2_text_frame);
    REG("TDA", id3v2_2_text_frame);
    REG("TIM", id3v2_2_text_frame);
    REG("TRD", id3v2_2_text_frame);
    REG("TMT", id3v2_2_text_frame);
    REG("TFT", id3v2_2_text_frame);
    REG("TBP", id3v2_2_text_frame);
    REG("TCR", id3v2_2_text_frame);
    REG("TPB", id3v2_2_text_frame);
    REG("TEN", id3v2_2_text_frame);
    REG("TSS", id3v2_2_text_frame);
    REG("TOF", id3v2_2_text_frame);
    REG("TLE", id3v2_2_text_frame);
    REG("TSI", id3v2_2_text_frame);
    REG("TDY", id3v2_2_text_frame);
    REG("TKE", id3v2_2_text_frame);
    REG("TOT", id3v2_2_text_frame);
    REG("TOA", id3v2_2_text_frame);
    REG("TOL", id3v2_2_text_frame);
    REG("TOR", id3v2_2_text_frame);
    REG("TXX", TXX);
    REG("COM", COM);
    REG("CNT", CNT);
    REG("POP", POP);
#   undef REG

  }
}

scribbu::id3v2_2_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_2_tag::mutex_)->~mutex();
    (&id3v2_2_tag::default_parsers_)->~unordered_map();
  }
}


///////////////////////////////////////////////////////////////////////////////
//                             class id3v2_2_tag                             //
///////////////////////////////////////////////////////////////////////////////

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream &is):
  id3v2_tag(is),
  compression_(flags() & 0x40)
{
  get_default_frame_parsers(std::inserter(parsers_, parsers_.begin()));
  parse(is);
}

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  compression_(H.flags_ & 0x40)
{
  get_default_frame_parsers(std::inserter(parsers_, parsers_.begin()));
  parse(is);
}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_comments(std::vector<scribbu::comments> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id3 ID("COM");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const COM &F = dynamic_cast<const COM&>(*frames_[i]);
      out.push_back(F.data());
      ++nout;
    }
  }

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_play_counts(std::vector<scribbu::play_count> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id3 ID("CNT");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const CNT &F = dynamic_cast<const CNT&>(*frames_[i]);
      out.push_back(F.count());
      ++nout;
    }
  }

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_ufids(std::vector<scribbu::unique_file_id> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast

  const frame_id3 ID("UFI");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const UFI &F = dynamic_cast<const UFI&>(*frames_[i]);
      out.push_back(F.file_id());
      ++nout;
    }
  }

  return nout;

}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_udts(std::vector<scribbu::user_defined_text> &out) const
{

  // TODO: Re-evaluate so as to avoid the dynamic_cast
  using scribbu::frame_id3;
  using scribbu::TXX;

  const frame_id3 ID("TXX");

  std::size_t nout = 0;
  for (std::ptrdiff_t i = 0, n = frames_.size(); i < n; ++i) {
    if (ID == frames_[i]->id()) {
      const TXX &F = dynamic_cast<const TXX&>(*frames_[i]);
      out.push_back(F.udt());
      ++nout;
    }
  }

  return nout;

}

/*static*/ bool
scribbu::id3v2_2_tag::register_default_frame_parser(const frame_id3 &id, const frame_parser &F)
{
  std::lock_guard<std::mutex> guard(mutex_);
  return default_parsers_.insert(std::make_pair(id, F)).second;
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

void scribbu::id3v2_2_tag::parse(std::istream &is)
{
  using scribbu::detail::unsigned_from_non_sync_safe;

  static const frame_id3 COM("COM");

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);
  // Also, save this so we can restore the stream to its original
  // state.
  std::istream::streampos here = is.tellg();

  try {

    // Size, in bytes, of the tag *after* the header, *before*
    // resynchronisation
    std::size_t cb_tag = size();

    // std::array's size is fixed at compile time, which we can't do, and
    // std::vector is permitted to allocate additional memory to accomodate
    // later insertions.
    std::unique_ptr<unsigned char[]> pb(new unsigned char[cb_tag]);
    is.read((char*)pb.get(), cb_tag);

    // Re-synchronise, if needed. Take care to deallocate the old buffer before
    // constructing any frames.
    if (unsynchronised()) {
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
      // that; I"The tag consists of a header, frames and
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
      // use that to parse the frame & add the new frame to our collection...
      frames_.push_back(parse_frame(id, p0 + 6, p0 + 6 + cb_frame));
      // & note the location of that frame in our lookup table.
      frame_map_.insert(std::make_pair(id, frames_.size() - 1));

    } // End iteration over frames.

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
  }

} // End method id3v2_2_tag::parse.

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::parse_frame(const frame_id3     &id,
                                  const unsigned char *p0,
                                  const unsigned char *p1) const
{
  parser_map_type::const_iterator p = parsers_.find(id);
  if (parsers_.end() == p) {
    return std::unique_ptr<id3v2_2_frame>(new unknown_id3v2_2_frame(id, p0, p1));
  }
  else {
    return p->second(id, p0, p1 - p0);
  }
}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string scribbu::id3v2_2_tag::text_frame_as_utf8(const frame_id3 &id) const
{
  std::size_t n = frame_map_.count(id);
  if (0 == n) {
    throw unknown_frame_error(id);
  }
  else if (1 != n) {
    throw duplicate_frame_error(id, n);
  }

  std::ptrdiff_t idx = frame_map_.find(id)->second;

  // TODO: Re-evaluate this to see if I can re-structure in such a way as to
  // not need the dynamic cast...
  const id3v2_2_text_frame &F = dynamic_cast<const id3v2_2_text_frame&>( *frames_[idx].get() );

  return F.as_utf8();

}
