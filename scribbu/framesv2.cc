#include "framesv2.hh"

#include <boost/functional/hash.hpp>
#include <boost/static_assert.hpp>

#include <arpa/inet.h>
#include <iconv.h>


bool
scribbu::detail::is_false_sync(unsigned char x, unsigned char y)
{
  return 255 == x && 233 < y;
}

bool
scribbu::detail::needs_unsync(unsigned char x, unsigned char y)
{
  return 255 == x && (233 < y || 0 == y);
}

std::size_t
scribbu::detail::count_false_syncs(std::size_t n)
{
  std::size_t count = 0;
  const unsigned char *p = (const unsigned char*)&n;
  for (std::ptrdiff_t i = 0; i < sizeof(std::size_t) - 1; ++i) {
    if (is_false_sync(p[i], p[i+1])) {
      ++count;
    }
  }
  return count;
}


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
//                           class unique_file_id                            //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::unique_file_id::size() const
{
  return owner_.size() + 1 + id_.size(); // Need to add the trailing NULL
}

std::size_t
scribbu::unique_file_id::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::unique_file_id::needs_unsynchronisation() const
{
  return count_ffs();
}

// std::size_t
// scribbu::unique_file_id::write(std::ostream &os, bool unsync) const
// {
//   const char zed = 0;

//   std::size_t cb_ffs = count_ffs();
//   if (unsync && cb_ffs) {
//     std::size_t cb = 0 ;
//     cb += detail::unsynchronise(os, owner_.begin(), owner_.end());
//     os.write(&zed, 1); cb += 1;
//     cb += detail::unsynchronise(os, id_.begin(), id_.end());
//     return cb;
//   }
//   else {
//     os.write((const char*)&(owner_[0]), owner_.size());
//     os.write(&zed, 1);
//     os.write((const char*)&(id_[0]), id_.size());
//     return owner_.size() + 1 + id_.size();
//   }
// }

std::size_t
scribbu::unique_file_id::write(std::ostream &os) const
{
  const char zed = 0;

  os.write((const char*)&(owner_[0]), owner_.size());
  os.write(&zed, 1);
  os.write((const char*)&(id_[0]), id_.size());
  return owner_.size() + 1 + id_.size();
}

std::size_t
scribbu::unique_file_id::count_ffs() const
{
  using namespace std;
  using namespace scribbu::detail;
  size_t cb = detail::count_ffs(owner_.begin(), owner_.end());
  // owner is null-terminated, so possibility of a false sync
  cb += detail::count_ffs(id_.begin(), id_.end());
  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                          class encryption_method                          //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::encryption_method::size() const
{
  // Need to add the trailing NULL on email_ + the method
  return email_.size() + 1 + 1 + data_.size();
}

std::size_t
scribbu::encryption_method::serialized_size(bool unsync) const
{
  // Need to add the trailing NULL on email_ + the method
  std::size_t cb = email_.size() + 1 + 1 + data_.size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::encryption_method::needs_unsynchronisation() const
{
  return count_ffs();
}

// std::size_t
// scribbu::encryption_method::write(std::ostream &os, bool unsync) const
// {
//   const char zed = 0;

//   using namespace std;
//   using namespace scribbu::detail;
//   std::size_t cb_ffs = count_ffs();
//   if (unsync && cb_ffs) {
//     std::size_t cb = 0 ;
//     cb += unsynchronise(os, email_.begin(), email_.end());
//     // email is null-terminated, so no possibilty of a false sync
//     os.write(&zed, 1); cb += 1;
//     os.write((const char*)&method_symbol_, 1); cb += 1;
//     if (!data_.empty() && is_false_sync(method_symbol_, data_.front())) {
//       os.write((const char*)&method_symbol_, 1); cb += 1;
//     }
//     cb += unsynchronise(os, data_.begin(), data_.end());
//     return cb;
//   }
//   else {
//     os.write((const char*)&(email_[0]), email_.size());
//     os.write((const char*)&method_symbol_, 1);
//     os.write((const char*)&(data_[0]), data_.size());
//     return email_.size() + 1 + data_.size();
//   }
// }

std::size_t
scribbu::encryption_method::write(std::ostream &os) const
{
  const char zed = 0;

  os.write((const char*)&(email_[0]), email_.size());
  os.write((const char*)&method_symbol_, 1);
  os.write((const char*)&(data_[0]), data_.size());
  return email_.size() + 1 + data_.size();

}

/// Return the number of bytes with 255 as their value when serialized
/// without unsynchronisation
std::size_t
scribbu::encryption_method::count_ffs() const
{
  using namespace std;
  using namespace scribbu::detail;
  size_t count = count_false_syncs(email_.begin(), email_.end());
  if (!email_.empty() && is_false_sync(email_.back(), method_symbol_)) {
    ++count;
  }
  // email_ is null-terminated, so no possibility of a false sync...
  // but there could be one between method_symbol_ & data_:
  if (!data_.empty() && is_false_sync(method_symbol_, data_.front())) {
    ++count;
  }
  count += count_false_syncs(data_.begin(), data_.end());
  return count;
}


///////////////////////////////////////////////////////////////////////////////
//                          class user_defined_text                          //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::user_defined_text::size() const
{
  return 1 + description_.size() + cbnil_ + text_.size();
}

std::size_t
scribbu::user_defined_text::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::user_defined_text::needs_unsynchronisation() const
{
  using namespace std;
  using namespace scribbu::detail;

  size_t count = 0;
  if (description_.size() && is_false_sync(unicode_, description_.front())) {
    ++count;
  }
  count += count_false_syncs(description_.begin(), description_.end());
  if (description_.size() &&
      text_.size() &&
      is_false_sync(description_.back(), text_.front())) {
    ++count;
  }
  count += count_false_syncs(text_.begin(), text_.end());
  
}

std::size_t
scribbu::user_defined_text::write(std::ostream &os) const
{
  const char zed = 0;
  const char uzed[2] = { 0, 0 };

  std::size_t cb = 0;
  os.write((const char*)&unicode_, 1);
  cb += 1;
  os.write((const char*)&(description_[0]), description_.size());
  cb += description_.size();
  if (2 == cbnil_) {
    os.write(uzed, 2);
    cb += 2;
  }
  else {
    os.write(&zed, 1);
    cb += 1;
  }
  os.write((const char*)&(text_[0]), text_.size());
  cb += text_.size();

  return cb;
}

std::size_t
scribbu::user_defined_text::count_ffs() const
{
  std::size_t cb = 0;
  if (255 == unicode_) {
    ++cb;
  }
  cb += detail::count_ffs(description_.begin(), description_.end());
  cb += detail::count_ffs(text_.begin(), text_.end());
  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                              class comments                               //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::comments::size() const
{
  return 4 + description_.size() + cbnil_ + text_.size();
}

std::size_t
scribbu::comments::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::comments::needs_unsynchronisation() const
{
  using namespace std;
  using namespace scribbu::detail;
  size_t count = is_false_sync(unicode_, lang_[0]) ? 1 : 0;
  count += count_false_syncs(lang_, lang_ + 3);
  if (description_.size() && is_false_sync(lang_[2], description_.front())) {
    ++count;
  }
  count += count_false_syncs(description_.begin(), description_.end());
  if (description_.size() &&
      text_.size() &&
      is_false_sync(description_.back(), text_.front())) {
    ++count;
  }
  count += count_false_syncs(text_.begin(), text_.end());
  return count; 
}

std::size_t
scribbu::comments::write(std::ostream &os) const
{
  const char zed[2] = { 0, 0 };

  std::size_t cb = 0;

  os.write((const char*)&unicode_, 1);
  cb += 1;
  os.write((const char*)lang_, 3);
  cb += 3;
  os.write((const char*)&(description_[0]), description_.size());
  cb += description_.size();
  os.write(zed, cbnil_);
  cb += cbnil_;
  os.write((const char*)&(text_[0]), text_.size());
  cb += text_.size();

  return cb;
}

std::size_t
scribbu::comments::count_ffs() const
{
  std::size_t cb = 0;
  if (255 == unicode_) {
    ++cb;
  }
  cb += detail::count_ffs(lang_, lang_ + 3);
  cb += detail::count_ffs(description_.begin(), description_.end());
  cb += detail::count_ffs(text_.begin(), text_.end());
  return cb;
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

std::size_t
scribbu::play_count::size() const
{
  return counter_.size();
}

std::size_t
scribbu::play_count::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::play_count::needs_unsynchronisation() const
{
  return scribbu::detail::count_false_syncs(counter_.begin(), counter_.end());
}

std::size_t
scribbu::play_count::write(std::ostream &os) const
{
  os.write((const char*)&(counter_[0]), counter_.size());
  return counter_.size();
}

std::size_t
scribbu::play_count::count_ffs() const
{
  return detail::count_ffs(counter_.begin(), counter_.end());
}


///////////////////////////////////////////////////////////////////////////////
//                            class popularimeter                            //
///////////////////////////////////////////////////////////////////////////////

std::size_t
scribbu::popularimeter::size() const
{
  return email_.size() + 2 + counter_.size(); // Need to add the trailing NULL
}

std::size_t
scribbu::popularimeter::serialized_size(bool unsync) const
{
  std::size_t cb = size();
  if (unsync) {
    cb += count_ffs();
  }
  return cb;
}

std::size_t
scribbu::popularimeter::needs_unsynchronisation() const
{
  using namespace std;
  using namespace scribbu::detail;

  std::size_t count = 0;
  count = count_false_syncs(email_.begin(), email_.end());
  count += count_false_syncs(counter_.begin(), counter_.end());

  if (email_.size() && is_false_sync(email_.back(), rating_)) ++count;
  if (counter_.size() && is_false_sync(rating_, counter_.back())) ++count;

  return count;
}

std::size_t
scribbu::popularimeter::write(std::ostream &os) const
{
  static const char zed = 0;

  os.write((const char*)&(email_[0]), email_.size());
  os.write(&zed, 1);
  os.write((const char*)&rating_, 1);
  os.write((const char*)&(counter_[0]), counter_.size());
  return email_.size() + 2 + counter_.size();
}

std::size_t
scribbu::popularimeter::count_ffs() const
{
  std::size_t cb = 0;
  cb += detail::count_ffs(email_.begin(), email_.end());
  if (255 == rating_) ++cb;
  cb += detail::count_ffs(counter_.begin(), counter_.end());
  return cb;
}
