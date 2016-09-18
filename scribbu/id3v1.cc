#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>


//////////////////////////////////////////////////////////////////////////////
//                         class id3v1_tag                                  //
//////////////////////////////////////////////////////////////////////////////

/*static*/ const scribbu::compact_id3v1_formatter
scribbu::id3v1_tag::DEF_ID3V1_FORMATTER(scribbu::id3v1_encoding::automatic,
                                        scribbu::id3v1_genre_expansion::none,
                                        ',');

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
 * The tag header (i.e. the ASCII text "TAG" or "TAG+") cannot be used to
 * distinguish a standard ID3v1 tag from an extended ID3v1 tag because there's
 * no way of distinguishing between an extended tag, or a standard tag where
 * the first character of the song title is '+' (since the first four bytes in
 * either case will be the ASCII text "TAG+").
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
  std::istream::streampos here = is.tellg();

  try {

    is.seekg(0, std::ios_base::end); // failbit or badbit
    std::istream::streampos there = is.tellg();
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
  std::istream::streampos here = is.tellg();

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

std::string
scribbu::id3v1_text_to_utf8(const unsigned char *pbuf,
                            std::size_t          cbbuf,
                            id3v1_encoding       v1enc)
{
  using scribbu::id3v1_encoding;

  const char * const ISO88591 = "ISO-8859-1";
  const char * const ASCII    = "ASCII";
  const char * const CP1252   = "CP1252";
  const char * const UTF8     = "UTF-8";
  const char * const UTF16BE  = "UCS-2BE";
  const char * const UTF16LE  = "UCS-2LE";
  const char * const UTF32    = "UTF-32";

  if (id3v1_encoding::automatic == v1enc) {
    if (3 <= cbbuf && 0xef == pbuf[0] &&
        0xbb == pbuf[1] && 0xbf == pbuf[2]) {
      v1enc = id3v1_encoding::utf_8;
    }
    else if (2 <= cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
      v1enc = id3v1_encoding::utf_16_be;
    }
    else if (2 <= cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
      v1enc = id3v1_encoding::utf_16_be;
    }
  }

  std::string result;

  if (id3v1_encoding::automatic == v1enc) {

    const std::vector<const char*> GUESSES({{
      ISO88591, ASCII, CP1252, UTF8, UTF16BE, UTF16LE, UTF32
    }});

    for (auto g: GUESSES) {
      try {
        scribbu::detail::iconv_guard guard(UTF8, g);
        result = scribbu::detail::to_utf8(guard, pbuf, cbbuf);
        break;
      } catch (const iconv_error&) {
        // Move on to the next guess...
      }
    }

  }
  else {

    const std::map<id3v1_encoding, const char*> LOOKUP({
      {id3v1_encoding::iso8859_1, ISO88591},
      {id3v1_encoding::ascii,     ASCII},
      {id3v1_encoding::cp1252,    CP1252},
      {id3v1_encoding::utf_8,     UTF8},
      {id3v1_encoding::utf_16_be, UTF16BE},
      {id3v1_encoding::utf_16_le, UTF16LE},
      {id3v1_encoding::utf_32,    UTF32}});

    const char *E = LOOKUP.at(v1enc);
    scribbu::detail::iconv_guard guard(UTF8, E);
    result = scribbu::detail::to_utf8(guard, pbuf, cbbuf);

  }

  // While the spec asks that all fields be padded with null bytes (i.e. 0
  // value), some apps, notably WinAmp, pad with spaces (i.e. ASCII 32) (\ref
  // scribbu_id3v1_refs_2 "[2]"). Strip any trailing spaces here (if the caller
  // wants the raw value for a text field, that's available as part of the
  // id3v1_tag interface, too).
  std::size_t last = result.find_last_not_of(' ');
  if (std::string::npos != last) {
    result.erase(last + 1);
  }
  else {
    result.clear();
  }

  return result;

} // End free function v1_text_to_utf8.

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
  std::istream::streampos here = is.tellg();

  // The ID3v1 tag is 128 bytes long & begins with the sequence "TAG",
  // and the extended tag is 227 bytes & begins with the sequence
  // "TAG+". So, if there's an ID3v1 tag present, the sequence "TAG"
  // will be present at len - 128 or len - 355.
  char buf[4];

  id3v1_info I;
  I.type_ = id3_v1_tag_type::none;

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

  if (id3_v1_tag_type::none == I.type_) {
    is.seekg(-128, std::ios_base::end);
    is.read(buf, 3);
    if ('T' == buf[0] && 'A' == buf[1] && 'G' == buf[2]) {
      I.start_ = is.tellg() - (std::streampos) 3;
      I.type_ = id3_v1_tag_type::v_1;
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
  std::istream::streampos here = is.tellg();
  scribbu::id3v1_info I = scribbu::ends_in_id3v1(is);
  if (id3_v1_tag_type::none != I.type_ && here == I.start_) {
    p.reset(new id3v1_tag(is));
  }
  return p;
}
