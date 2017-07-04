#ifndef ID3V1_HH_INCLUDED
#define ID3V1_HH_INCLUDED 1
/**
 * \page scribbu_id3v1 ID3v1 Tags
 *
 * \section scribbu_id3v1_discuss Discussion
 *
 * The ID3v1 tag is attributed to NamkraD (AKA Eric Kemp), in the program
 * Studio3 in 1996 \ref scribbu_id3v1_refs_1 "[1]", and quickly became a de
 * facto standard for storing metadata in MP3s. It is a block of 128 bytes
 * appended to the audio data in the following format:
 *
 \code

  offset length contents

      0    3    the ASCII text "TAG"
      3   30    song title/track name
     33   30    artist
     63   30    album
     93    4    the year expressed as ASCII text
     97   30    comment
    127    1    genre

  \endcode
  *
  * ID3v1 defined a set of genres denoted by numerical codes (see \ref
  * scribbu_id3v1_genres "below"). Winamp extended that list, but support for
  * the extended Winamp list is not universal. In some cases, only the first 80
  * genres are supported.
  *
  * AFAICT, ID3v1 is very loosely specified, being more a collection of
  * conventions. For instance, according to \ref scribbu_id3v1_refs_2 "[2]",
  * "The specification asks for all fields to be padded with null character
  * (ASCII 0). However, not all applications respect this (an example is WinAmp
  * which pads fields with <space>, ASCII 32)."
  *
  * Michael Mutschier observed that since the standard calls for padding the
  * fields with zero bytes, it's a good bet that readers will stop when they
  * encounter any NULL value. Therefore, if the second-to-last byte of a field
  * is zero, a value may be stored in the last byte. He specifically proposed
  * adding the album track in this way to the comment field \ref
  * scribbu_id3v1_refs_1 "[1]". This modification is known as ID3 v1.1.
  *
  * Here is an example ID3v1 tag:
  *
  \code
   0000000 54 41 47 4c 6f 72 63 61 27 73 20 4e 6f 76 65 6e  >TAGLorca's Noven<
   0000020 61 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  >a...............<
   0000040 00 54 68 65 20 50 6f 67 75 65 73 00 00 00 00 00  >.The Pogues.....<
   0000060 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 48  >...............H<
   0000100 65 6c 6c 27 73 20 44 69 74 63 68 20 5b 45 78 70  >ell's Ditch [Exp<
   0000120 61 6e 64 65 64 5d 20 28 55 53 20 56 65 31 39 39  >anded] (US Ve199<
   0000140 30 41 6d 61 7a 6f 6e 2e 63 6f 6d 20 53 6f 6e 67  >0Amazon.com Song<
   0000160 20 49 44 3a 20 32 30 33 35 35 38 32 35 00 05 ff  > ID: 20355825...<
   0000200
  \endcode
  *
  * Broken out:
  *
  \code
   0000000 54 41 47                                         TAG
   0000003 4c 6f 72 63 61 27 73 20 4e 6f 76 65 6e 61 00 00  Lorca's Novena..
   0000013 00 00 00 00 00 00 00 00 00 00 00 00 00 00        ..............
   0000021 54 68 65 20 50 6f 67 75 65 73 00 00 00 00 00 00  The Pogues......
   0000031 00 00 00 00 00 00 00 00 00 00 00 00 00 00        ...............
   000003f 48 65 6c 6c 27 73 20 44 69 74 63 68 20 5b 45 78  Hell's Ditch [Ex
   000004f 70 61 6e 64 65 64 5d 20 28 55 53 20 56 65        panded] (US Ve
   000005d 31 39 39 30                                      1990
   0000061 41 6d 61 7a 6f 6e 2e 63 6f 6d 20 53 6f 6e 67 20  Amazon.com Song
   0000071 20 49 44 3a 20 32 30 33 35 35 38 32 35 00        ID: 20355825.
   000007f 05                                               Track #
   0000080 ff                                               no/unknown genre
  \endcode
  *
  * Notice that this is an ID3v1.1 tag.
  *
  * The thirty byte limitation in the ID3v1 fields soon became apparent,
  * leading to the Enhanced, or Extended (ID3v1) Tag \ref scribbu_id3v1_refs_3
  * "[3]". The ID3v1 Enhanced tag is even more loosely specified than the
  * ID3v1; it is formed by prepending an additional 227 bytes to the ID3v1 tag
  * (so the entire tag is a block of 355 bytes appended to the audio data). The
  * additional 227 bytes are laid out as follows:

 \code

  offset length contents

      0   4     the ASCII text "TAG+"
      4  60     song title/track name
     64  60     artist
    124  60     album
    184   1     speed
    185  30     genre
    215   6     start time
    221   6     end time

  \endcode
  *
  * For title, artist, and album, the value is written to the ID3v1 tag first,
  * and any text beyond thirty characters is written to the extended tag
  * (giving a total of 90 characters for these three fields).
  *
  * The "speed" field can be set to 1 (slow), 2 (medium), 3 (fast) or 4
  * (hardcore).
  *
  * The "genre" field is free-form text, and it's relationship to the ID3v1
  * genre field is unspecified. The start & end times are intended to refer to
  * the track being tagged, and are strings expressing timestamps in the form
  * "mmm:ss" (no further interpretation is given).
  *
  * If none of these fields are used, then the extended tag is
  * omitted. According to Wikipedia, the extended tag was never widely adopted
  * (significantly, neither XMMS nor Winamp support it).
  *
  *
  * \section scribbu_id3v1_charsets ID3v1 Character Encodings
  *
  * The ID3v1 \ref scribbu_id3v1_refs_1 "specification" makes no mention of
  * character encoding; the various fields are defined as fixed-length arrays
  * of characters, which to me suggests ASCII. However, as arrays of char, they
  * are in prinicple capable of containing text in any encoding, and there are
  * certainly other character encodings to be found in the wild (generally the
  * ANSI code page in use on the computer on which the tags were written).
  *
  * Furthermore, it is impossible to detect the encoding reliably. The presence
  * of a BOM is suggestive, but not dispositive, and in the case of the
  * European ISO-8859 code pages, a given byte sequence will likely have many
  * valid interpretations.
  *
  * Consequently, this class provides two accessors for each such field; one
  * which simply copies the constituent bytes into a buffer, and a second that
  * will attempt to perform an encoding conversion from the tag's native
  * encoding to whatever the caller desires. Furthermore, we depend on the
  * caller to *tell us* what the tag's internal encoding is:
  *
  * - if the caller says nothing, then ASCII is assumed
  *
  * - if the caller passes a member of the encoding enumeration, then that
  *   value will be used
  *
  * - if the caller passes boost::none, then the system locale will be used
  *
  *
  * \section scribbu_id3v1_geners ID3v1 Genres
  *
  * The original proposal defined 80 genres:
  *
  * - 0 Blues
  * - 1 Classic Rock
  * - 2 Country
  * - 3 Dance
  * - 4 Disco
  * - 5 Funk
  * - 6 Grunge
  * - 7 Hip-Hop
  * - 8 Jazz
  * - 9 Metal
  * - 10 New Age
  * - 11 Oldies
  * - 12 Other
  * - 13 Pop
  * - 14 R&B
  * - 15 Rap
  * - 16 Reggae
  * - 17 Rock
  * - 18 Techno
  * - 19 Industrial
  * - 20 Alternative
  * - 21 Ska
  * - 22 Death Metal
  * - 23 Pranks
  * - 24 Soundtrack
  * - 25 Euro-Techno
  * - 26 Ambient
  * - 27 Trip-Hop
  * - 28 Vocal
  * - 29 Jazz+Funk
  * - 30 Fusion
  * - 31 Trance
  * - 32 Classical
  * - 33 Instrumental
  * - 34 Acid
  * - 35 House
  * - 36 Game
  * - 37 Sound Clip
  * - 38 Gospel
  * - 39 Noise
  * - 40 AlternRock
  * - 41 Bass
  * - 42 Soul
  * - 43 Punk
  * - 44 Space
  * - 45 Meditative
  * - 46 Instrumental Pop
  * - 47 Instrumental Rock
  * - 48 Ethnic
  * - 49 Gothic
  * - 50 Darkwave
  * - 51 Techno-Industrial
  * - 52 Electronic
  * - 53 Pop-Folk
  * - 54 Eurodance
  * - 55 Dream
  * - 56 Southern Rock
  * - 57 Comedy
  * - 58 Cult
  * - 59 Gangsta
  * - 60 Top 40
  * - 61 Christian Rap
  * - 62 Pop/Funk
  * - 63 Jungle
  * - 64 Native American
  * - 65 Cabaret
  * - 66 New Wave
  * - 67 Psychadelic
  * - 68 Rave
  * - 69 Showtunes
  * - 70 Trailer
  * - 71 Lo-Fi
  * - 72 Tribal
  * - 73 Acid Punk
  * - 74 Acid Jazz
  * - 75 Polka
  * - 76 Retro
  * - 77 Musical
  * - 78 Rock & Roll
  * - 79 Hard Rock
  *
  * Winamp added the following:
  *
  * - 80 Folk
  * - 81 Folk-Rock
  * - 82 National Folk
  * - 83 Swing
  * - 84 Fast Fusion
  * - 85 Bebob
  * - 86 Latin
  * - 87 Revival
  * - 88 Celtic
  * - 89 Bluegrass
  * - 90 Avantgarde
  * - 91 Gothic Rock
  * - 92 Progressive Rock
  * - 93 Psychedelic Rock
  * - 94 Symphonic Rock
  * - 95 Slow Rock
  * - 96 Big Band
  * - 97 Chorus
  * - 98 Easy Listening
  * - 99 Acoustic
  * - 100 Humour
  * - 101 Speech
  * - 102 Chanson
  * - 103 Opera
  * - 104 Chamber Music
  * - 105 Sonata
  * - 106 Symphony
  * - 107 Booty Brass
  * - 108 Primus
  * - 109 Porn Groove
  * - 110 Satire
  * - 111 Slow Jam
  * - 112 Club
  * - 113 Tango
  * - 114 Samba
  * - 115 Folklore
  * - 116 Ballad
  * - 117 Power Ballad
  * - 118 Rhytmic Soul
  * - 119 Freestyle
  * - 120 Duet
  * - 121 Punk Rock
  * - 122 Drum Solo
  * - 123 A Capela
  * - 124 Euro-House
  * - 125 Dance Hall
  *
  * \section scribbu_id3v1_refs References
  *
  * \anchor scribbu_id3v1_refs_1 Unknown, cited 2015: ID3v1. [Available online
  * at http://id3.org/ID3v1.]
  *
  * \anchor scribbu_id3v1_refs_2 Unknown, 1999: MPEG Audio Tag ID3v1 [Available
  * online at http://mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm#MPEGTAG.]
  *
  * \anchor scribbu_id3v1_refs_3 Unknown, cited 2015: MP3 TAG &
  * Enhanced TAG description (english) [Originally available online at
  * http://www.fortunecity.com/underworld/sonic/3/id3tag.html, now
  * cached at
  * https://web.archive.org/web/20120310015458/http://www.fortunecity.com/underworld/sonic/3/id3tag.html]
  *
  * \anchor scribbu_id3v1_refs_4 Unknown, cited 2015: ID3v1 [Originally available online at
  * https://en.wikipedia.org/wiki/ID3#ID3v1]
  *
  *
  */

#include <exception>

#include <boost/exception/all.hpp>
#include <boost/optional.hpp>

#include <scribbu/scribbu.hh>
#include <scribbu/errors.hh>
#include <scribbu/charsets.hh>

namespace scribbu {

  /**
   *
   * \class id3v1_tag
   *
   * \brief An immutable class whose instances represent ID3v1 & ID3v1.1 tags
   * (enhanced or not)
   *
   *
   * Class id3v1_tag doesn't attempt any interpretation of "artist", "comment",
   * and so forth. Instances are immutable copies of what's in the tag, broken
   * out by field. So, for instance, "artist" is not represented as a string,
   * but as a block of 30 or 90 bytes. It is up to the caller to interpret it.
   *
   *
   * I can see a few situations when instantiating id3v1_tag:
   *
   * - you have an opaque block of data that *might* contain an ID3V1 tag: call
   *   process_id3v1, which will return null if there is not a tag at the start
   *   of the block
   *
   * - you have a block of data that you *know* contains an ID3V1 tag, but not
   *   whether it's extended or not: construct an id3v1_tag instance using a
   *   constructor overload that takes a block of bytes only
   *
   * - you have a block of data that you *know* contains an ID3V1 tag, and you
   *   know whether it's extended or not (perhaps because you've already peeked
   *   at the first three or four bytes): construct an id3v1_tag instance using
   *   a constructor overload that takes a block of bytes along with a boolean
   *   indicating whether it's extended or not
   *
   *
   */

  class id3v1_tag {

  public:

    class invalid_tag: public scribbu::error
    {
    public:
      invalid_tag()
      { }
    };

  public:
    /// Construct using an input stream known to be currently pointing at an
    /// ID3V1 tag; the implementation will determine whether the tag is V1 or
    /// V1.1, and whether the tag is enhanced or not. It will throw if \a is is
    /// not pointing to a valid tag
    id3v1_tag(std::istream &is);
    /// Construct using an input stream known to be currently pointing at an
    /// ID3V1 tag which is known a priori to be extended (or not); the
    /// implementation will determine whether the tag is V1 or V1.1.
    id3v1_tag(std::istream &is, bool enhanced);

    static boost::optional<std::string> text_for_genre(unsigned char genre);

	static const boost::optional<encoding> DEF_SRC_ENCODING;
	static const encoding                  DEF_DST_ENCODING;
	static const on_no_encoding            DEF_ON_NO_ENCODING;

  public:

    /// retrieve the 'album' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator album(forward_output_iterator p) const {
      return std::copy(album_.begin(), album_.end(), p);
    }
    /// retrieve the 'album' field (encoded)
    template <typename string_type>
    string_type
	album(const boost::optional<encoding> &src = DEF_SRC_ENCODING,
		  encoding dst = DEF_DST_ENCODING,
		  on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    /// retrieve the 'artist' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator artist(forward_output_iterator p) const {
      return std::copy(artist_.begin(), artist_.end(), p);
    }
    /// retrieve the 'artist' field (encoded)
    template<typename string_type>
    string_type
    artist(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
           encoding dst = DEF_DST_ENCODING,
           on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    /// retrieve the 'comment' field (raw bytes)
    template <typename forward_output_iterator>
    forward_output_iterator comment(forward_output_iterator p) const {
      return std::copy(comment_.begin(), comment_.end(), p);
    }
    /// retrieve the 'comment' field (encoded)
    template<typename string_type>
    string_type
    comment(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
			encoding dst = DEF_DST_ENCODING,
			on_no_encoding rsp = DEF_ON_NO_ENCODING) const;
    /// retrieve the one-byte genre field
    unsigned char genre() const {
      return genre_;
    }

    template <typename forward_output_iterator>
    forward_output_iterator enh_genre(forward_output_iterator p) const {
      return std::copy(ext_genre_.begin(), ext_genre_.end(), p);
    }

    template<typename string_type>
    string_type
    enh_genre(const boost::optional<encoding> & src,
              encoding dst = DEF_DST_ENCODING,
              on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    std::pair<bool, unsigned char> speed() const {
      return std::make_pair(extended(), speed_);
    }

    template <typename forward_output_iterator>
    forward_output_iterator start_time(forward_output_iterator p) const {
      return std::copy(start_time_.begin(), start_time_.end(), p);
    }

    template<typename string_type>
    string_type
    start_time(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
			   encoding dst = DEF_DST_ENCODING,
			   on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    template <typename forward_output_iterator>
    forward_output_iterator end_time(forward_output_iterator p) const {
      return std::copy(end_time_.begin(), end_time_.end(), p);
    }

    template<typename string_type>
    string_type
    end_time(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
			 encoding dst = DEF_DST_ENCODING,
			 on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    template <typename forward_output_iterator>
    forward_output_iterator title(forward_output_iterator p) const {
      return std::copy(title_.begin(), title_.end(), p);
    }

    template<typename string_type>
    string_type
    title(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
          encoding dst = DEF_DST_ENCODING,
          on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    std::pair<bool, unsigned char> track_number() const {
      return std::make_pair(v1_1(), track_number_);
    }

    template <typename forward_output_iterator>
    forward_output_iterator year(forward_output_iterator p) const {
      return std::copy(year_.begin(), year_.end(), p);
    }

    template<typename string_type>
    string_type year(const boost::optional<encoding> & src = DEF_SRC_ENCODING,
					 encoding dst = DEF_DST_ENCODING,
					 on_no_encoding rsp = DEF_ON_NO_ENCODING) const;

    bool enhanced() const {
      return extended();
    }

    bool extended() const {
      return extended_;
    }

    bool v1_1() const {
      return v1_1_;
    }

  private:

    static const std::size_t ID3V1_TAG_SIZE = 128U;
    static const std::size_t ID3V1_EXT_TAG_SIZE = 355U;

    void init_standard(unsigned char *p);
    void init_extended(unsigned char *p);

  private:

    bool                         extended_;
    bool                         v1_1_;
    std::vector<unsigned char>   album_;
    std::vector<unsigned char>   artist_;
    std::vector<unsigned char>   comment_;
    unsigned char                genre_;
    std::vector<unsigned char>   ext_genre_;
    std::array<unsigned char, 6> start_time_;
    std::array<unsigned char, 6> end_time_;
    unsigned char                speed_;
    std::vector<unsigned char>   title_;
    unsigned char                track_number_;
    std::array<unsigned char, 4> year_;


  };

  enum class id3_v1_tag_type { none, v_1, v_1_extended };

  struct id3v1_info {
    id3_v1_tag_type          type_;
    std::ios_base::streampos start_;
    std::ios_base::streampos end_;
  };

  /**
   * \brief Test an input stream to see if there is an ID3v1 tag at the end
   *
   *
   * \param is [in,out] An input stream which may or may not have an ID3v1 tag
   * appended to other content; the read pointer will always be restored on
   * exit, regardless of the result
   *
   * \return An id3v1_tag_info struct describing the ID3v1 tag, if any (on
   * which more below)
   *
   *
   * This method returns an id3v1_info struct:
   *
   * - if \a is does \em not have an ID3v1 tag at the end, the \c type_ field
   *   will be set to none and the start_ & end_ fields are undefined
   *
   * - else, the \c type_ field will be set to v_1 or v_1_extended as
   *   appropriate, and start_ and end_ will be set to offsets within the files
   *   such that the tag is contained in the range [start_, end_)
   *
   *
   */

  id3v1_info ends_in_id3v1(std::istream &is);

  /**
   * \brief Attempt to read an ID3v1 tag from a stream
   *
   *
   * \param is [in,out] An input stream which may or may not have an ID3v1 tag
   * appended to other content; the read pointer will always be restored on
   * exit, regardless of the result
   *
   * \return a (possibly nil) unique_ptr to an id3v1_tag indicating that the
   * caller now owns the memory associated with it
   *
   *
   */

  std::unique_ptr<id3v1_tag> process_id3v1(std::istream &is);

} // End namespace scribbu.

#endif // not ID3V1_HH_INCLUDED
