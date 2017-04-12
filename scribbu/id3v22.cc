#include <id3v22.hh>


///////////////////////////////////////////////////////////////////////////////
//                         id3v2_2_tag nifty counter                         //
///////////////////////////////////////////////////////////////////////////////

typedef
std::unordered_map<scribbu::frame_id3, scribbu::id3v2_2_tag::frame_parser>
def_reg_type;

typedef
std::unordered_map<scribbu::frame_id3,
                   scribbu::id3v2_2_tag::reserved_frame_parser>
reserved_reg_type;

static unsigned int nifty_counter_ = 0;

static typename
std::aligned_storage<sizeof(std::mutex), alignof(std::mutex)>::type
mutex_buf_;

static typename
std::aligned_storage< sizeof(def_reg_type), alignof(def_reg_type)>::type
map_buf_;

static typename
std::aligned_storage< sizeof(reserved_reg_type),
                      alignof(reserved_reg_type)>::type
reserved_buf_;

/*static*/ std::mutex&
scribbu::id3v2_2_tag::mutex_ =
  reinterpret_cast<std::mutex&>(mutex_buf_);

/*static*/ def_reg_type&
scribbu::id3v2_2_tag::default_parsers_ =
  reinterpret_cast<def_reg_type&>(map_buf_);

/*static*/ reserved_reg_type&
scribbu::id3v2_2_tag::reserved_parsers_ =
  reinterpret_cast<reserved_reg_type&>(reserved_buf_);

scribbu::id3v2_2_tag::static_initializer::static_initializer()
{
  if (0 == nifty_counter_++) {
    new (&id3v2_2_tag::mutex_) std::mutex();
    new (&id3v2_2_tag::default_parsers_) def_reg_type();
    new (&id3v2_2_tag::reserved_parsers_) reserved_reg_type();

#   define REG(id, tag) \
    id3v2_2_tag::default_parsers_.insert( \
        std::make_pair(frame_id3((id)), tag::create)) \

#   define RREG(id, pfn) \
    id3v2_2_tag::reserved_parsers_.insert( \
        std::make_pair(frame_id3((id)), &id3v2_2_tag::pfn)) \

    RREG("CNT", create_CNT);
    RREG("COM", create_COM);
    RREG("TXX", create_TXX);
    RREG("UFI", create_UFI);
    RREG("TT1", create_text_frame);
    RREG("TT2", create_text_frame);
    RREG("TT3", create_text_frame);
    RREG("TP1", create_text_frame);
    RREG("TP2", create_text_frame);
    RREG("TP3", create_text_frame);
    RREG("TP4", create_text_frame);
    RREG("TCM", create_text_frame);
    RREG("TXT", create_text_frame);
    RREG("TLA", create_text_frame);
    RREG("TCO", create_text_frame);
    RREG("TAL", create_text_frame);
    RREG("TPA", create_text_frame);
    RREG("TRK", create_text_frame);
    RREG("TRC", create_text_frame);
    RREG("TYE", create_text_frame);
    RREG("TDA", create_text_frame);
    RREG("TIM", create_text_frame);
    RREG("TRD", create_text_frame);
    RREG("TMT", create_text_frame);
    RREG("TFT", create_text_frame);
    RREG("TBP", create_text_frame);
    RREG("TCR", create_text_frame);
    RREG("TPB", create_text_frame);
    RREG("TEN", create_text_frame);
    RREG("TSS", create_text_frame);
    RREG("TOF", create_text_frame);
    RREG("TLE", create_text_frame);
    RREG("TSI", create_text_frame);
    RREG("TDY", create_text_frame);
    RREG("TKE", create_text_frame);
    RREG("TOT", create_text_frame);
    RREG("TOA", create_text_frame);
    RREG("TOL", create_text_frame);
    RREG("TOR", create_text_frame);

    REG("POP", POP);

#   undef RREG
#   undef REG

  }
}

scribbu::id3v2_2_tag::static_initializer::~static_initializer()
{
  if (0 == --nifty_counter_) {
    (&id3v2_2_tag::mutex_)->~mutex();
    (&id3v2_2_tag::default_parsers_)->~unordered_map();
    (&id3v2_2_tag::reserved_parsers_)->~unordered_map();
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

/*virtual*/ void
scribbu::id3v2_2_tag::accept_for_print(id3v2_acyclic_visitor &V,
                                       std::ostream          &os) const
{
  id3v2_2_tag_printer *p = dynamic_cast<id3v2_2_tag_printer*>(&V);
  if (p) {
    p->print_on(os, *this);
  }

  for (std::ptrdiff_t i = 0; i < frames_.size(); ++i) {

    if (comments_.count(i)) {
      comments_.at(i)->accept_for_print(V, os);
    }
    else if (play_counts_.count(i)) {
      play_counts_.at(i)->accept_for_print(V, os);
    }
    else if (ufids_.count(i)) {
      ufids_.at(i)->accept_for_print(V, os);
    }
    else if (udts_.count(i)) {
      udts_.at(i)->accept_for_print(V, os);
    }
    else if (texts_pos_.count(i)) {
      texts_pos_.at(i)->accept_for_print(V, os);
    }
    else {
      frames_.at(i)->accept_for_print(V, os);
    }

  } // End iteration over frames.

} // End id3v2_2_tag::accept_for_print.

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_comments(std::vector<scribbu::comments> &out) const
{
  for (auto p: comments_) {
    out.push_back(p.second->data());
  }
  return comments_.size();
}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_play_counts(std::vector<scribbu::play_count> &out) const
{
  for (auto p: play_counts_) {
    out.push_back(p.second->count());
  }
  return play_counts_.size();
}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_udts(std::vector<scribbu::user_defined_text> &out) const
{
  for (auto p: udts_) {
    out.push_back(p.second->udt());
  }
  return udts_.size();
}

/*virtual*/
std::size_t
scribbu::id3v2_2_tag::all_ufids(std::vector<scribbu::unique_file_id> &out) const
{
  for (auto p: ufids_) {
    out.push_back(p.second->file_id());
  }
  return ufids_.size();
}

/*static*/
bool
scribbu::id3v2_2_tag::has_framework_parser(const frame_id3 &id)
{
  return "COM" == id || "CNT" == id || "UFI" == id || "TXX" == id || id.text();
}

/*static*/
bool
scribbu::id3v2_2_tag::register_default_frame_parser(const frame_id3 &id,
                                                    const frame_parser &F)
{
  std::lock_guard<std::mutex> guard(mutex_);
  return default_parsers_.insert(std::make_pair(id, F)).second;
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::create_COM(const frame_id3   &/*id*/,
                                 std::ptrdiff_t       i,
                                 const unsigned char *p,
                                 std::size_t          cb)
{
  // TODO: Make this exception-safe
  COM *pf = new COM(p, p + cb);
  comments_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_2_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::create_CNT(const frame_id3   &/*id*/,
                                 std::ptrdiff_t       i,
                                 const unsigned char *p,
                                 std::size_t          cb)
{
  // TODO: Make this exception-safe
  CNT *pf = new CNT(p, p + cb);
  play_counts_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_2_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::create_UFI(const frame_id3     & /*id*/,
                                 std::ptrdiff_t       i,
                                 const unsigned char *p,
                                 std::size_t          cb)
{
  // TODO: Make this exception-safe
  UFI *pf = new UFI(p, p + cb);
  ufids_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_2_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::create_TXX(const frame_id3   &/*id*/,
                                 std::ptrdiff_t       i,
                                 const unsigned char *p,
                                 std::size_t          cb)
{
  // TODO: Make this exception-safe
  TXX *pf = new TXX(p, p + cb);
  udts_.insert(std::make_pair(i, pf));
  return std::unique_ptr<scribbu::id3v2_2_frame>( pf );
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::create_text_frame(const frame_id3     &id,
                                        std::ptrdiff_t       i,
                                        const unsigned char *p,
                                        std::size_t          cb)
{
  // TODO: Make this exception-safe
  id3v2_2_text_frame *pf = new id3v2_2_text_frame(id, p, p + cb);
  texts_pos_.insert(std::make_pair(i, pf));
  text_.insert(std::make_pair(id, pf));
  return std::unique_ptr<scribbu::id3v2_2_frame>( pf );
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
      // use that to parse the frame & add the new frame to our collection...
      std::size_t n = frames_.size() - 1;
      frames_.push_back(parse_frame(id, n, p0 + 6, p0 + 6 + cb_frame));
      // & note the location of that frame in our lookup table.
      frame_map_.insert(std::make_pair(id, n));
    } // End iteration over frames.

  } catch (const std::exception &ex) {
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw ex;
  }

} // End method id3v2_2_tag::parse.

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::id3v2_2_tag::parse_frame(const frame_id3     &id,
                                  std::ptrdiff_t       i,
                                  const unsigned char *p0,
                                  const unsigned char *p1)
{
  reserved_parser_map_type::const_iterator p = reserved_parsers_.find(id);
  if (reserved_parsers_.end() != p) {
    return (this->*(p->second))(id, i, p0, p1 - p0);
  }
  else {
    parser_map_type::const_iterator p = parsers_.find(id);
    if (parsers_.end() != p) {
      return p->second(id, p0, p1 - p0);
    }
    else {
      return std::unique_ptr<id3v2_2_frame>(new unknown_id3v2_2_frame(id, p0, p1));
    }
  }
}

/// Lookup a text frame, convert its data from its native encoding to
/// UTF-8, return as a string
std::string
scribbu::id3v2_2_tag::text_frame_as_utf8(const frame_id3 &id) const
{
  std::size_t n = text_.count(id);
  if (0 == n) {
    throw unknown_frame_error(id);
  }
  else if (1 != n) {
    throw duplicate_frame_error(id, n);
  }

  return text_.find(id)->second->as_utf8();

}
