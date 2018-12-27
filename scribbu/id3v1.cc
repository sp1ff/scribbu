/**
 * \file id3v1.cc
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

#include <scribbu/id3v1.hh>


//////////////////////////////////////////////////////////////////////////////
//                         class id3v1_tag                                  //
//////////////////////////////////////////////////////////////////////////////

/*static*/ const boost::optional<scribbu::encoding>
scribbu::id3v1_tag::DEF_SRC_ENCODING(encoding::ASCII);

/*static*/ const scribbu::encoding
scribbu::id3v1_tag::DEF_DST_ENCODING(encoding::UTF_8);

/*static*/ const scribbu::on_no_encoding
scribbu::id3v1_tag::DEF_ON_NO_ENCODING(on_no_encoding::fail);

/**
 * \brief Construct using an input stream known to be currently
 * pointing at an ID3V1 tag; the implementation will determine whether
 * the tag is V1 or V1.1, and whether the tag is enhanced or not. It
 * will throw if \a is is not pointing to a valid tag
 *
 *
 * \param is [in] an input stream from which this tag shall be read;
 * it is required that all the data remaining to be read in is makes
 * up the tag (IOW, the tag is at the end of stream is).
 *
 *
 * The tag header (i.e. the ASCII text "TAG" or "TAG+")cannot be used
 * to distinguish a standard ID3v1 tag from an extended ID3v1 tag
 * because there's no way of distinguishing between an extended tag,
 * or a standard tag where the first character of the song title is
 * '+' (since the first four bytes in either case will be the ASCII
 * text "TAG+").
 *
 * So, rather than use the header to distinguish the two cases, we use
 * the size of the tag. Since the ID3v1 tag is defined to be at the
 * end of the file, we measure that size by computing how many bytes
 * remain to be read in `is'.
 *
 *
 */

scribbu::id3v1_tag::id3v1_tag(std::istream &is)
{
  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state on error.
  std::streampos here = is.tellg();

  try {

    is.seekg(0, std::ios_base::end); // failbit or badbit
    std::streampos there = is.tellg();
    is.seekg(here, std::ios_base::beg); // failbit or badbit

    unsigned char buf[ID3V1_EXT_TAG_SIZE];
    std::size_t cb = there - here;
    if (ID3V1_TAG_SIZE == cb) {
      is.read((char*)buf, ID3V1_TAG_SIZE); // eofbit or failbit
      init_standard(buf);
    } else if (ID3V1_EXT_TAG_SIZE == cb) {
      is.read((char*)buf, ID3V1_EXT_TAG_SIZE); // eofbit or failbit
      init_extended(buf);
    } else {
      throw invalid_tag();
    }

    // Restore the exception mask
    is.exceptions(exc_mask);

  } catch (const std::istream::failure &ex) {
    is.exceptions(std::ios_base::goodbit);
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw invalid_tag();
  }

}

scribbu::id3v1_tag::id3v1_tag(std::istream &is,
                              bool          enhanced)
{
  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(std::ios_base::eofbit|std::ios_base::failbit|std::ios_base::badbit);

  // Also, save this so we can restore the stream to its original
  // state on error.
  std::streampos here = is.tellg();

  unsigned char buf[ID3V1_EXT_TAG_SIZE];

  try {

    if (enhanced) {
      is.read((char*)buf, ID3V1_EXT_TAG_SIZE); // eofbit or failbit
      init_extended(buf);
    } else {
      is.read((char*)buf, ID3V1_TAG_SIZE); // eofbit or failbit
      init_standard(buf);
    }

  } catch (const std::istream::failure &ex) {
    is.exceptions(std::ios_base::goodbit);
    is.seekg(here, std::ios_base::beg);
    is.exceptions(exc_mask);
    throw invalid_tag();;
  }

}

namespace scribbu {

  template<>
  std::string
  scribbu::id3v1_tag::album(const boost::optional<encoding> &src,
                            encoding dst,
                            on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(album_[0]), album_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::artist(const boost::optional<encoding> &src,
                             encoding dst,
                             on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(artist_[0]), artist_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::comment(const boost::optional<encoding> &src,
                              encoding dst,
                              on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(comment_[0]), comment_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::enh_genre(const boost::optional<encoding> &src,
                                encoding dst,
                                on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(ext_genre_[0]), ext_genre_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::start_time(const boost::optional<encoding> &src,
                                 encoding dst,
                                 on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(start_time_[0]), start_time_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::end_time(const boost::optional<encoding> &src,
                               encoding dst,
                               on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(end_time_[0]), end_time_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::title(const boost::optional<encoding> &src,
                            encoding dst,
                            on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(title_[0]), title_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

  template<>
  std::string
  scribbu::id3v1_tag::year(const boost::optional<encoding> &src,
                           encoding dst,
                           on_no_encoding rsp) const
  {
    using std::string;
    using namespace scribbu;
    if (!scribbu::char_traits<char>::is_code_unit(dst)) {
      throw bad_code_unit(dst, sizeof(char));
    }
    return convert_encoding<string>(&(year_[0]), year_.size(),
                                    src ? src.get() :
                                    encoding_from_system_locale(),
                                    dst, rsp);
  }

}

/*static*/ boost::optional<std::string>
scribbu::id3v1_tag::text_for_genre(unsigned char genre)
{
  using std::string;

  static const std::string GENRES[] = {
    string("Blues"),
    string("Classic Rock"),
    string("Country"),
    string("Dance"),
    string("Disco"),
    string("Funk"),
    string("Grunge"),
    string("Hip-Hop"),
    string("Jazz"),
    string("Metal"),
    string("New Age"),
    string("Oldies"),
    string("Other"),
    string("Pop"),
    string("R&B"),
    string("Rap"),
    string("Reggae"),
    string("Rock"),
    string("Techno"),
    string("Industrial"),
    string("Alternative"),
    string("Ska"),
    string("Death Metal"),
    string("Pranks"),
    string("Soundtrack"),
    string("Euro-Techno"),
    string("Ambient"),
    string("Trip-Hop"),
    string("Vocal"),
    string("Jazz+Funk"),
    string("Fusion"),
    string("Trance"),
    string("Classical"),
    string("Instrumental"),
    string("Acid"),
    string("House"),
    string("Game"),
    string("Sound Clip"),
    string("Gospel"),
    string("Noise"),
    string("AlternRock"),
    string("Bass"),
    string("Soul"),
    string("Punk"),
    string("Space"),
    string("Meditative"),
    string("Instrumental Pop"),
    string("Instrumental Rock"),
    string("Ethnic"),
    string("Gothic"),
    string("Darkwave"),
    string("Techno-Industrial"),
    string("Electronic"),
    string("Pop-Folk"),
    string("Eurodance"),
    string("Dream"),
    string("Southern Rock"),
    string("Comedy"),
    string("Cult"),
    string("Gangsta"),
    string("Top 40"),
    string("Christian Rap"),
    string("Pop/Funk"),
    string("Jungle"),
    string("Native American"),
    string("Cabaret"),
    string("New Wave"),
    string("Psychadelic"),
    string("Rave"),
    string("Showtunes"),
    string("Trailer"),
    string("Lo-Fi"),
    string("Tribal"),
    string("Acid Punk"),
    string("Acid Jazz"),
    string("Polka"),
    string("Retro"),
    string("Musical"),
    string("Rock & Roll"),
    string("Hard Rock"),
    string("Folk"),
    string("Folk-Rock"),
    string("National Folk"),
    string("Swing"),
    string("Fast Fusion"),
    string("Bebob"),
    string("Latin"),
    string("Revival"),
    string("Celtic"),
    string("Bluegrass"),
    string("Avantgarde"),
    string("Gothic Rock"),
    string("Progressive Rock"),
    string("Psychedelic Rock"),
    string("Symphonic Rock"),
    string("Slow Rock"),
    string("Big Band"),
    string("Chorus"),
    string("Easy Listening"),
    string("Acoustic"),
    string("Humour"),
    string("Speech"),
    string("Chanson"),
    string("Opera"),
    string("Chamber Music"),
    string("Sonata"),
    string("Symphony"),
    string("Booty Brass"),
    string("Primus"),
    string("Porn Groove"),
    string("Satire"),
    string("Slow Jam"),
    string("Club"),
    string("Tango"),
    string("Samba"),
    string("Folklore"),
    string("Ballad"),
    string("Power Ballad"),
    string("Rhytmic Soul"),
    string("Freestyle"),
    string("Duet"),
    string("Punk Rock"),
    string("Drum Solo"),
    string("A Capela"),
    string("Euro-House"),
    string("Dance Hall"),
  };

  if (sizeof(GENRES)/sizeof(GENRES[0]) <= genre) {
    return boost::none;
  }

  return boost::optional<string>(GENRES[genre]);
}

std::ostream&
scribbu::id3v1_tag::write(std::ostream &os) const
{
  static const char TAG [3] = { 'T', 'A', 'G' };
  static const char TAGP[4] = { 'T', 'A', 'G', '+' };
  static const char NIL [1] = { 0 };

  if (extended_) {
    os.write(TAGP, 4);
    os.write((const char*)&(title_[30]), 60);
    os.write((const char*)&(artist_[30]), 60);
    os.write((const char*)&(album_[30]), 60);
    os.write((const char*)&speed_, 1);
    os.write((const char*)&(ext_genre_[0]), 30);
    os.write((const char*)&(start_time_[0]), 6);
    os.write((const char*)&(end_time_[0]), 6);
  }

  os.write(TAG, 3);
  os.write((const char*)&(title_ [0]), 30);
  os.write((const char*)&(artist_[0]), 30);
  os.write((const char*)&(album_ [0]), 30);
  os.write((const char*)&(year_  [0]),  4);

  if (v1_1()) {
    os.write((const char *)&(comment_[0]), 28);
    os.write(NIL, 1);
    os.write((const char*)&track_number_, 1);
  }
  else {
    os.write((const char *)&(comment_[0]), 30);
  }

  os.write((const char*)&genre_, 1);

  return os;
}

void
scribbu::id3v1_tag::init_standard(unsigned char *p)
{
  if ('T' != p[0] || 'A' != p[1] || 'G' != p[2]) {
    throw invalid_tag();
  }

  p += 3;

  extended_ = false;

  title_.resize(30);
  artist_.resize(30);
  album_.resize(30);

  std::copy(p, p + 30, title_.begin());
  p += 30;
  std::copy(p, p + 30, artist_.begin());
  p += 30;
  std::copy(p, p + 30, album_.begin());
  p += 30;
  std::copy(p, p + 4, year_.begin());
  p += 4;

  // Check for ID3v1.1
  if (0 == p[28] && 0 != p[29]) {
    v1_1_ = true;
    comment_.resize(28);
    std::copy(p, p + 28, comment_.begin());
    track_number_ = p[29];
  } else {
    v1_1_ = false;
    comment_.resize(30);
    std::copy(p, p + 30, comment_.begin());
  }

  p += 30;

  genre_ = *p;

}

void
scribbu::id3v1_tag::init_extended(unsigned char *p)
{
  if ('T' != p[0] || 'A' != p[1] || 'G' != p[2] || '+' != p[3]) {
    throw invalid_tag();
  }

  p += 4;

  extended_ = true;

  title_.resize(90);
  artist_.resize(90);
  album_.resize(90);
  ext_genre_.resize(30);

  std::vector<unsigned char>::iterator pout;
  pout = std::copy(p + 226, p + 256, title_.begin());
  std::copy(p, p + 60, pout);

  p += 60;
  pout = std::copy(p + 196, p + 226, artist_.begin());
  std::copy(p, p + 60, pout);

  p += 60;
  pout = std::copy(p + 166, p + 196, album_.begin());
  std::copy(p, p + 60, pout);

  p += 60;
  speed_ = *p++;

  std::copy(p, p + 30, ext_genre_.begin());
  p += 30;

  std::copy(p, p + 6, start_time_.begin());
  p += 6;
  std::copy(p, p + 6, end_time_.begin());
  p += 6;

  if ('T' != p[0] || 'A' != p[1] || 'G' != p[2]) {
    throw invalid_tag();
  }

  // Skip past "TAG", title, artist & album (already copied)
  p += 93;

  std::copy(p, p + 4, year_.begin());
  p += 4;

  // Check for ID3v1.1
  if (0 == p[28] && 0 != p[29]) {
    v1_1_ = true;
    comment_.resize(28);
    std::copy(p, p + 28, comment_.begin());
    track_number_ = p[29];
  } else {
    v1_1_ = false;
    comment_.resize(30);
    std::copy(p, p + 30, comment_.begin());
  }

  p += 30;
  genre_ = *p;
}


//////////////////////////////////////////////////////////////////////////////
//                          free functions                                  //
//////////////////////////////////////////////////////////////////////////////

scribbu::id3v1_info scribbu::ends_in_id3v1(std::istream &is)
{
  const std::ios::iostate EXC_MASK = std::ios::eofbit | std::ios::failbit | std::ios::badbit;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(EXC_MASK);

  // Also, save this so we can restore the stream to its original
  // state.
  std::streampos here = is.tellg();

  // The ID3v1 tag is 128 bytes long & begins with the sequence "TAG",
  // and the extended tag is 227 bytes & begins with the sequence
  // "TAG+". So, if there's an ID3v1 tag present, the sequence "TAG"
  // will be present at len - 128 or len - 355.
  char buf[4];

  id3v1_info I;
  I.type_ = id3_v1_tag_type::none;

  try {

    is.seekg(0, std::ios_base::end);
    I.start_ = I.end_ = is.tellg();
    if (355 <= I.start_) {
      is.seekg(-355, std::ios_base::end);
      is.read(buf, 4);
      if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2] && '+' == buf[3]) {
        I.start_ = is.tellg() - (std::streampos) 4;
        I.type_ = id3_v1_tag_type::v_1_extended;
      }
    }
  }
  catch (const std::ios_base::failure &ex) {
    is.exceptions(std::ios_base::goodbit);
    is.clear();
    is.exceptions(EXC_MASK);
  }

  if (id3_v1_tag_type::none == I.type_) {

    try {
      is.seekg(-128, std::ios_base::end);
      is.read(buf, 3);
      if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2]) {
        I.start_ = is.tellg() - (std::streampos) 3;
        I.type_ = id3_v1_tag_type::v_1;
      }
    }
    catch (const std::ios_base::failure &ex) {
      is.exceptions(std::ios_base::goodbit);
      is.clear();
    }

  }

  // Restore the stream's state
  is.seekg(here, std::ios_base::beg);
  is.exceptions(exc_mask);

  return I;

}

std::unique_ptr<scribbu::id3v1_tag>
scribbu::process_id3v1(std::istream &is)
{
  std::unique_ptr<scribbu::id3v1_tag> p;
  scribbu::id3v1_info I = scribbu::ends_in_id3v1(is);
  if (id3_v1_tag_type::none != I.type_) {
    p.reset(new id3v1_tag(is));
  }
  return p;
}
