#include "framesv2.hh"

#include <iconv.h>
#include <boost/functional/hash.hpp>


///////////////////////////////////////////////////////////////////////////////
//                              class frame_id3                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::frame_id3::frame_id3(unsigned char id0,
                              unsigned char id1,
                              unsigned char id2):
  experimental_('X' == id0 || 'Y' == id0 || 'Z' == id0)
{ id_[0] = id0; id_[1] = id1; id_[2] = id2; }

scribbu::frame_id3::frame_id3(const unsigned char id[3]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; }

scribbu::frame_id3::frame_id3(const char id[3]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; }

bool scribbu::operator==(const frame_id3 &lhs,
                         const frame_id3 &rhs)
{
  unsigned char id_lhs[3], id_rhs[3];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] && id_lhs[2] == id_rhs[2];
}

std::ostream& scribbu::operator<<(std::ostream &os, const scribbu::frame_id3 &x) {
  return os << x.as_string();
}

//template <>
std::size_t std::hash<scribbu::frame_id3>::operator()(const scribbu::frame_id3 &x) const
{
  unsigned char id[3];
  x.copy(id);

  std::size_t seed = 0;
  boost::hash_combine(seed, id[0]);
  boost::hash_combine(seed, id[1]);
  boost::hash_combine(seed, id[2]);

  return seed;
}


///////////////////////////////////////////////////////////////////////////////
//                              class frame_id4                              //
///////////////////////////////////////////////////////////////////////////////

scribbu::frame_id4::frame_id4(unsigned char id0,
                              unsigned char id1,
                              unsigned char id2,
                              unsigned char id3):
  experimental_('X' == id0 || 'Y' == id0 || 'Z' == id0)
{ id_[0] = id0; id_[1] = id1; id_[2] = id2; id_[3] = id3; }

scribbu::frame_id4::frame_id4(const unsigned char id[4]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; id_[3] = id[3]; }

scribbu::frame_id4::frame_id4(const char id[4]):
  experimental_('X' == id[0] || 'Y' == id[0] || 'Z' == id[0])
{ id_[0] = id[0]; id_[1] = id[1]; id_[2] = id[2]; id_[3] = id[3]; }

bool scribbu::operator==(const frame_id4 &lhs,
                         const frame_id4 &rhs)
{
  unsigned char id_lhs[4], id_rhs[4];
  lhs.copy(id_lhs);
  rhs.copy(id_rhs);
  return id_lhs[0] == id_rhs[0] && id_lhs[1] == id_rhs[1] &&
         id_lhs[2] == id_rhs[2] && id_lhs[3] == id_rhs[3];
}

std::ostream& scribbu::operator<<(std::ostream &os, const scribbu::frame_id4 &x) {
  return os << x.as_string();
}

std::size_t std::hash<scribbu::frame_id4>::operator()(const scribbu::frame_id4 &x) const
{
  unsigned char id[4];
  x.copy(id);

  std::size_t seed = 0;
  boost::hash_combine(seed, id[0]);
  boost::hash_combine(seed, id[1]);
  boost::hash_combine(seed, id[2]);
  boost::hash_combine(seed, id[3]);

  return seed;
}


///////////////////////////////////////////////////////////////////////////////
//                             class play_count                              //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::play_count::count() const
{
  if (1 == counter_.size()) {
    uint8_t x = counter_[0];
  }
  else if (2 == counter_.size()) {
   uint16_t x = ( counter_[0] << 8 ) | counter_[1];
  }
  else if (3 == counter_.size()) {
    uint32_t x = ( counter_[0] << 16 ) | ( counter_[1] << 8 ) | counter_[2];
  }
  else if (4 == counter_.size()) {
    uint32_t x = ( counter_[0] << 24 ) | ( counter_[1] << 16 ) |
      ( counter_[2] << 8 )  | counter_[3];
  }
  else if (4 < counter_.size()) {
    throw std::domain_error("CNT overflow");
  }
}
