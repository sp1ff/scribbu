/**
 * \file winamp-genres.hh
 *
 * Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>
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
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 */

#ifndef WINAMP_GENRES_HH_INCLUDED
#define WINAMP_GENRES_HH_INCLUDED 1

/**
 * \page scribbu_winamp_genres
 *
 * \section scribbu_winamp_genres_intro Introduction
 *
 * The original ID3v1 proposal \ref scribbu_winamp_genres_refs_1 "[1]" defined
 * 80 genres. Winamp added several to the list when it was released, and
 * continued adding to that list over its life, finally resulting in a list
 * of a 192 genres, each with a normalized name and numeric identifier.
 *
 * ID3v2 of course allows free-form text to be used in the TCON/Content Type
 * frame, but the Winamp-defined list continues to be widely used. This module
 * provides a variety of utilities for working with it.
 *
 *
 * \section scribbu_winamp_id3v1_genres ID3v1 Genres
 *
 * The original ID3v1 list:
 *
 \code
 Genre           | Genre
 --------------- | -------------------
 0 Blues         | 40 AlternRock
 1 Classic Rock  | 41 Bass
 2 Country       | 42 Soul
 3 Dance         | 43 Punk
 4 Disco         | 44 Space
 5 Funk          | 45 Meditative
 6 Grunge        | 46 Instrumental Pop
 7 Hip-Hop       | 47 Instrumental Rock
 8 Jazz          | 48 Ethnic
 9 Metal         | 49 Gothic
 10 New Age      | 50 Darkwave
 11 Oldies       | 51 Techno-Industrial
 12 Other        | 52 Electronic
 13 Pop          | 53 Pop-Folk
 14 R&B          | 54 Eurodance
 15 Rap          | 55 Dream
 16 Reggae       | 56 Southern Rock
 17 Rock         | 57 Comedy
 18 Techno       | 58 Cult
 19 Industrial   | 59 Gangsta
 20 Alternative  | 60 Top 40
 21 Ska          | 61 Christian Rap
 22 Death Metal  | 62 Pop/Funk
 23 Pranks       | 63 Jungle
 24 Soundtrack   | 64 Native American
 25 Euro-Techno  | 65 Cabaret
 26 Ambient      | 66 New Wave
 27 Trip-Hop     | 67 Psychadelic
 28 Vocal        | 68 Rave
 29 Jazz+Funk    | 69 Showtunes
 30 Fusion       | 70 Trailer
 31 Trance       | 71 Lo-Fi
 32 Classical    | 72 Tribal
 33 Instrumental | 73 Acid Punk
 34 Acid         | 74 Acid Jazz
 35 House        | 75 Polka
 36 Game         | 76 Retro
 37 Sound Clip   | 77 Musical
 38 Gospel       | 78 Rock & Roll
 39 Noise        | 79 Hard Rock
 \endcode
 *
 * Winamp added the following, additional genres to the ID3v1 proposal:
 *
 \code
 Genre               | Genre
 ------------------------------------------------
 80 Folk             | 111 Slow Jam
 81 Folk-Rock        | 112 Club
 82 National Folk    | 113 Tango
 83 Swing            | 114 Samba
 84 Fast Fusion      | 115 Folklore
 85 Bebob            | 116 Ballad
 86 Latin            | 117 Power Ballad
 87 Revival          | 118 Rhytmic Soul
 88 Celtic           | 119 Freestyle
 89 Bluegrass        | 120 Duet
 90 Avantgarde       | 121 Punk Rock
 91 Gothic Rock      | 122 Drum Solo
 92 Progressive Rock | 123 A Capela
 93 Psychedelic Rock | 124 Euro-House
 94 Symphonic Rock   | 125 Dance Hall
 95 Slow Rock        | 126 Goa
 96 Big Band         | 127 Drum & Bass
 97 Chorus           | 128 Club-House
 98 Easy Listening   | 129 Hardcore Techno
 99 Acoustic         | 130 Terror
 100 Humour          | 131 Indie
 101 Speech          | 132 BritPop
 102 Chanson         | 133 Negerpunk
 103 Opera           | 134 Polsk Punk
 104 Chamber Musice  | 135 Beat
 105 Sonata          | 136 Christian Gangsta Rap
 106 Symphony        | 137 Heavy Metal
 107 Booty Brass     | 138 Black Metal
 108 Primus          | 139 Crossover
 109 Porn Groove     | 140 Contemporary Christian
 110 Satire          | 141 Christian rock
 \endcode
 *
 * Winamp 1.91 (released in June 1998) further added:
 *
 \code
 Genre
 -------------------------
 142 Merengue
 143 Salsa
 144 Thrash Metal
 145 Anime
 146 Jpop
 147 Synthpop
 \endcode
 *
 * and Winamp 5.6 (November 2010) finally added:
 *
 \code
 Genre             | Genre
 ---------------------------------------------
 148 Abstract      | 170 Leftfield
 149 Art Rock      | 171 Lounge
 150 Baroque       | 172 Math Rock
 151 Bhangra       | 173 New Romantic
 152 Big beat      | 174 Nu-Breakz
 153 Breakbeat     | 175 Post-Punk
 154 Chillout      | 176 Post-Rock
 155 Downtempo     | 177 Psytrance
 156 Dub           | 178 Shoegaze
 157 EBM           | 179 Space Rock
 158 Eclectic      | 180 Trop Rock
 159 Electro       | 181 World Music
 160 Electroclash  | 182 Neoclassical
 161 Emo           | 183 Audiobook
 162 Experimental  | 184 Audio theatre
 163 Garage        | 185 Neue Deutsche Welle
 164 Global        | 186 Podcast
 165 IDM           | 187 Indie-Rock
 166 Illbient      | 188 G-Funk
 167 Industro-Goth | 189 Dubstep
 168 Jam Band      | 190 Garage Rock
 169 Krautrock     | 191 Psybient
 \endcode
 *
 *
 * \section scribbu_winamp_genres_refs References
 *
 * 1. \anchor scribbu_winamp_genres_refs_1 [1] unknown. ID3v1 http://id3.org/ID3v1
 * (updated September 1, 2019)
 *
 * 2. \anchor scribbu_winamp_genres_refs_2 [2] Michael Herstine. "Comparing
 * Damerau-Levenshtein Implementations"
 * https://github.com/sp1ff/damerau-levenshtein (updated June 25, 2020)
 *
 * 3. \anchor scribbu_winamp_genres_refs_3 [3] Hal Berghel and David Roach,
 * "An Extension of Ukkonen's Enhanced Dyanmic Programming ASM Algorithm"
 * ACM Transactions on Information Systems, 14 (1996) No. 1, 94-106.
 *
 * 4. \anchor scribbu_winamp_genres_refs_4 [4] Ken Whistler. "Unicode
 * Normalizatoin Forms" http://www.unicode.org/reports/tr15/ (updated June 27,
 * 2020)
 *
 *
 */

#include <tuple>
#include <vector>

#include <boost/optional.hpp>

namespace scribbu {

  /// Retrieve the textual name of an ID3v1 genre, encoded as ASCII
  /// \sa match_genre
  boost::optional<std::string>
  text_for_genre(unsigned char genre);

  /// Sentinel value for an ID3v1 genre that is "undefined"
  const unsigned char UNDEFINED_GENRE = 255;

  enum class id3v1_genre_generation {
    /// the 80 genres defined in the original ID3v1 standard
    standard,
    /// the 62 genres initially added by Winamp
    winamp,
    /// the 6 genres added in Winamp 1.91
    winamp_1_91,
    /// the 44 genres added in Winamp 5.6
    winamp_5_6
  };

  namespace detail {
    extern const std::vector<std::string> GENRES;
  }

  /// Retrieve the numeric IDs and textual names for ID3v1 genres
  template <typename FOI> // Forward Output Iterator => tuple<unsigned char, string>
  FOI get_id3v1_genre_list(id3v1_genre_generation gen, FOI pout)
  {
    unsigned char count = 80;
    if (id3v1_genre_generation::winamp_5_6 == gen) {
      count = 192;
    } else if (id3v1_genre_generation::winamp_1_91 == gen) {
      count = 148;
    } else if (id3v1_genre_generation::winamp == gen) {
      count = 142;
    }

    for (unsigned char i = 0; i < count; ++i) {
      *pout++ = std::make_tuple(i, detail::GENRES[i]);
    }

    return pout;
}

  /**
   * \brief Fuzzily match an ID3v1 genre
   *
   *
   * \param text [in] text to be fuzzily matched; the string shall be UTF-8
   * encoded
   *
   * \return a three-tuple consisting of the Winamp genre that best matches
   * \a text, the numeric value correspodning to that genre, and the Damerau-
   * Levenshtein distance between \a text & the best match
   *
   *
   * This function accepts a UTF-8-encoded string, computes the Damerau-
   * Levenshtien between that string & the Winamp-defined list of genres,
   * and returns the best match.
   *
   * This function uses the method of Berghel & Roach
   * \ref scribbu_winamp_genres_refs_2 "[2]". I have posted a reference
   * implementation on Github \ref scribbu_winamp_genres_refs_3 "[3]".
   *
   * Note that the match will be carried in a modified case-insensitive manner:
   *
   * 1. the input UTF-8 string will be converted to UCS-4
   *
   * 2. each character will then be converted to lower case according to the
   *    system locale
   *
   * 3. the Damerau-Levenshtein distance from the resulting string to each
   *    of the 192 Winamp-defined ID3v1 genres will be computed & the closest
   *    match returned
   *
   * Working in UCS-4 greatly reduces the work involved in comparing
   * code-points, rather than bytes, but doesn't completely solve the
   * problem. What we really want to be comparing are graphemes (or is it
   * grapheme clusters?), not code points, since they are what most of us
   * picture when we think of a "character". The problem is that a single
   * grapheme can be represented in multiple ways. For instance, ê can be
   * represented as U+00EA (LATIN SMALL LETTER E WITH CIRCUMFLEX) or as U+0065,
   * U+0302 (LATIN SMALL LETTER E followed by COMBINING CIRCUMFLEX ACCENT).
   *
   * The right thing to do would be to scan the string & normalize it \ref
   * scribbu_winamp_genres_refs_4 "[4]" but I'm not going to go to that level of
   * effort until this becomes a problem.
   *
   *
   */

  std::tuple<std::string, unsigned char, std::size_t>
  match_winamp_genre(const std::string &text);

} // End namespace scribbu.

#endif // WINAMP_GENRES_HH_INCLUDED
