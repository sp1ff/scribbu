#include <id3v22.hh>


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

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream &is):
  id3v2_tag(is),
  compression_(flags() & 0x40)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                               text_parsers_.begin()));
  parse(is);
}

scribbu::id3v2_2_tag::id3v2_2_tag(std::istream     &is,
                                  const id3v2_info &H):
  id3v2_tag(H),
  compression_(H.flags_ & 0x40)
{
  get_default_generic_frame_parsers(std::inserter(generic_parsers_,
                                                  generic_parsers_.begin()));
  get_default_text_frame_parsers(std::inserter(text_parsers_,
                                               text_parsers_.begin()));
  parse(is);
}

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

void scribbu::id3v2_2_tag::parse(std::istream &is)
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

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
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
  return text_map_.find(id)->second->as_str<std::string>(dst, rsp, src);
}
