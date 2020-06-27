/**
 * \file winamp-genres.cc
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

#include "config.h"
#include <scribbu/winamp-genres.hh>

#include <codecvt>
#include <locale>

const char32_t *GENRESW[] = {
  /*  0*/ U"blues",
  /*  1*/ U"classic rock",
  /*  2*/ U"country",
  /*  3*/ U"dance",
  /*  4*/ U"disco",
  /*  5*/ U"funk",
  /*  6*/ U"grunge",
  /*  7*/ U"hip-hop",
  /*  8*/ U"jazz",
  /*  9*/ U"metal",
  /* 10*/ U"new age",
  /* 11*/ U"oldies",
  /* 12*/ U"other",
  /* 13*/ U"pop",
  /* 14*/ U"r&b",
  /* 15*/ U"rap",
  /* 16*/ U"reggae",
  /* 17*/ U"rock",
  /* 18*/ U"techno",
  /* 19*/ U"industrial",
  /* 20*/ U"alternative",
  /* 21*/ U"ska",
  /* 22*/ U"death metal",
  /* 23*/ U"pranks",
  /* 24*/ U"soundtrack",
  /* 25*/ U"euro-techno",
  /* 26*/ U"ambient",
  /* 27*/ U"trip-hop",
  /* 28*/ U"vocal",
  /* 29*/ U"jazz+funk",
  /* 30*/ U"fusion",
  /* 31*/ U"trance",
  /* 32*/ U"classical",
  /* 33*/ U"instrumental",
  /* 34*/ U"acid",
  /* 35*/ U"house",
  /* 36*/ U"game",
  /* 37*/ U"sound clip",
  /* 38*/ U"gospel",
  /* 39*/ U"noise",
  /* 40*/ U"alternrock",
  /* 41*/ U"bass",
  /* 42*/ U"soul",
  /* 43*/ U"punk",
  /* 44*/ U"space",
  /* 45*/ U"meditative",
  /* 46*/ U"instrumental pop",
  /* 47*/ U"instrumental rock",
  /* 48*/ U"ethnic",
  /* 49*/ U"gothic",
  /* 50*/ U"darkwave",
  /* 51*/ U"techno-industrial",
  /* 52*/ U"electronic",
  /* 53*/ U"pop-folk",
  /* 54*/ U"eurodance",
  /* 55*/ U"dream",
  /* 56*/ U"southern rock",
  /* 57*/ U"comedy",
  /* 58*/ U"cult",
  /* 59*/ U"gangsta",
  /* 60*/ U"top 40",
  /* 61*/ U"christian rap",
  /* 62*/ U"pop/funk",
  /* 63*/ U"jungle",
  /* 64*/ U"native american",
  /* 65*/ U"cabaret",
  /* 66*/ U"new wave",
  /* 67*/ U"psychadelic",
  /* 68*/ U"rave",
  /* 69*/ U"showtunes",
  /* 70*/ U"trailer",
  /* 71*/ U"lo-fi",
  /* 72*/ U"tribal",
  /* 73*/ U"acid punk",
  /* 74*/ U"acid jazz",
  /* 75*/ U"polka",
  /* 76*/ U"retro",
  /* 77*/ U"musical",
  /* 78*/ U"rock & roll",
  /* 79*/ U"hard rock",
  /* 80*/ U"folk",
  /* 81*/ U"folk-rock",
  /* 82*/ U"national folk",
  /* 83*/ U"swing",
  /* 84*/ U"fast fusion",
  /* 85*/ U"bebob",
  /* 86*/ U"latin",
  /* 87*/ U"revival",
  /* 88*/ U"celtic",
  /* 89*/ U"bluegrass",
  /* 90*/ U"avantgarde",
  /* 91*/ U"gothic rock",
  /* 92*/ U"progressive rock",
  /* 93*/ U"psychedelic rock",
  /* 94*/ U"symphonic rock",
  /* 95*/ U"slow rock",
  /* 96*/ U"big band",
  /* 97*/ U"chorus",
  /* 98*/ U"easy listening",
  /* 99*/ U"acoustic",
  /*100*/ U"humour",
  /*101*/ U"speech",
  /*102*/ U"chanson",
  /*103*/ U"opera",
  /*104*/ U"chamber musice",
  /*105*/ U"sonata",
  /*106*/ U"symphony",
  /*107*/ U"booty brass",
  /*108*/ U"primus",
  /*109*/ U"porn groove",
  /*110*/ U"satire",
  /*111*/ U"slow jam",
  /*112*/ U"club",
  /*113*/ U"tango",
  /*114*/ U"samba",
  /*115*/ U"folklore",
  /*116*/ U"ballad",
  /*117*/ U"power ballad",
  /*118*/ U"rhytmic soul",
  /*119*/ U"freestyle",
  /*120*/ U"duet",
  /*121*/ U"punk rock",
  /*122*/ U"drum solo",
  /*123*/ U"a capela",
  /*124*/ U"euro-house",
  /*125*/ U"dance hall",
  /*126*/ U"goa",
  /*127*/ U"drum & bass",
  /*128*/ U"club-house",
  /*129*/ U"hardcore techno",
  /*130*/ U"terror",
  /*131*/ U"indie",
  /*132*/ U"britpop",
  /*133*/ U"negerpunk",
  /*134*/ U"polsk punk",
  /*135*/ U"beat",
  /*136*/ U"christian gangsta rap",
  /*137*/ U"heavy metal",
  /*138*/ U"black metal",
  /*139*/ U"crossover",
  /*140*/ U"contemporary christian",
  /*141*/ U"christian rock",
  /*142*/ U"merengue",
  /*143*/ U"salsa",
  /*144*/ U"thrash metal",
  /*145*/ U"anime",
  /*146*/ U"jpop",
  /*147*/ U"synthpop",
  /*148*/ U"abstract",
  /*149*/ U"art rock",
  /*150*/ U"baroque",
  /*151*/ U"bhangra",
  /*152*/ U"big beat",
  /*153*/ U"breakbeat",
  /*154*/ U"chillout",
  /*155*/ U"downtempo",
  /*156*/ U"dub",
  /*157*/ U"ebm",
  /*158*/ U"eclectic",
  /*159*/ U"electro",
  /*160*/ U"electroclash",
  /*161*/ U"emo",
  /*162*/ U"experimental",
  /*163*/ U"garage",
  /*164*/ U"global",
  /*165*/ U"idm",
  /*166*/ U"illbient",
  /*167*/ U"industro-goth",
  /*168*/ U"jam band",
  /*169*/ U"krautrock",
  /*170*/ U"leftfield",
  /*171*/ U"lounge",
  /*172*/ U"math rock",
  /*173*/ U"new romantic",
  /*174*/ U"nu-breakz",
  /*175*/ U"post-punk",
  /*176*/ U"post-rock",
  /*177*/ U"psytrance",
  /*178*/ U"shoegaze",
  /*179*/ U"space rock",
  /*180*/ U"trop rock",
  /*181*/ U"world music",
  /*182*/ U"neoclassical",
  /*183*/ U"audiobook",
  /*184*/ U"audio theatre",
  /*185*/ U"neue deutsche welle",
  /*186*/ U"podcast",
  /*187*/ U"indie-rock",
  /*188*/ U"g-funk",
  /*189*/ U"dubstep",
  /*190*/ U"garage rock",
  /*191*/ U"psybient",
};

const size_t MAX_GENRE_LENGTH = 22; // 22: Contemporary Christian

const size_t NUM_GENRES = sizeof(GENRESW)/sizeof(const char32_t*);

namespace {

  ptrdiff_t
  f(ptrdiff_t k,
    ptrdiff_t p,
    const std::u32string &A,
    const std::u32string &B,
    size_t m,
    size_t n,
    size_t max_k,
    size_t max_p,
    std::ptrdiff_t *fkp,
    ptrdiff_t zero_k,
    size_t inf) {

    ptrdiff_t t = -inf;
    if (p >= 0) {
      t = fkp[(k + zero_k)*max_p+p] + 1;
    }
    ptrdiff_t t2 = t;
    if (t > 0 && t < m && k+t-1 >= 0 && k + t < n) {
      // t-1 >=0
      // t < m
      // k+t-1 >= 0
      // k+t < n
      if (A[t-1] == B[k+t] && A[t] == B[k+t-1]) {
        t2 = t + 1;
      }
    }
    ptrdiff_t ta = -inf;
    if (p >= 0) {
      ta = fkp[(k - 1 + zero_k)*max_p+p];
    }
    ptrdiff_t tb = -inf;
    if (p >= 0) {
      tb = fkp[(k + 1 + zero_k)*max_p+p] + 1;
    }
    if (ta > t) t = ta;
    if (tb > t) t = tb;
    if (t2 > t) t = t2;
    while (t < (ptrdiff_t)std::min(m, n - k) && A[t] == B[t+k]) ++t;
    if (k + zero_k >= 0 && p > -2) {
      fkp[(k + zero_k)*max_p+p+1] = t;
    }
    return t;
  }

  size_t damerau_levenshtein(const std::u32string &A,
                             const std::u32string &B,
                             size_t n,
                             std::ptrdiff_t *fkp,
                             size_t max_k,
                             size_t max_p,
                             ptrdiff_t zero_k,
                             size_t inf)
  {
    using namespace std;

    size_t m = A.length();
    // The minmal p will be at the end of diagonal k
    ptrdiff_t k = n - m;
    ptrdiff_t p = k;

    do {
      ptrdiff_t inc = p;
      for (ptrdiff_t temp_p = 0; temp_p < p; ++temp_p) {
        ptrdiff_t x = n - m - inc;
        if (abs(x) <= temp_p) {
          f(x, temp_p, A, B, m, n, max_k, max_p, fkp, zero_k, inf);
        }
        x = n - m + inc;
        if (abs(x) <= temp_p) {
          f(x, temp_p, A, B, m, n, max_k, max_p, fkp, zero_k, inf);
        }
        --inc;
      }
      f(n - m, p, A, B, m, n, max_k, max_p, fkp, zero_k, inf);
      ++p;
    } while (fkp[(n - m + zero_k)*max_p+p] != m);

    return p - 1;
  }

}

const std::vector<std::string>
scribbu::detail::GENRES = {{
  std::string("Blues"),
  std::string("Classic Rock"),
  std::string("Country"),
  std::string("Dance"),
  std::string("Disco"),
  std::string("Funk"),
  std::string("Grunge"),
  std::string("Hip-Hop"),
  std::string("Jazz"),
  std::string("Metal"),
  std::string("New Age"),
  std::string("Oldies"),
  std::string("Other"),
  std::string("Pop"),
  std::string("R&B"),
  std::string("Rap"),
  std::string("Reggae"),
  std::string("Rock"),
  std::string("Techno"),
  std::string("Industrial"),
  std::string("Alternative"),
  std::string("Ska"),
  std::string("Death Metal"),
  std::string("Pranks"),
  std::string("Soundtrack"),
  std::string("Euro-Techno"),
  std::string("Ambient"),
  std::string("Trip-Hop"),
  std::string("Vocal"),
  std::string("Jazz+Funk"),
  std::string("Fusion"),
  std::string("Trance"),
  std::string("Classical"),
  std::string("Instrumental"),
  std::string("Acid"),
  std::string("House"),
  std::string("Game"),
  std::string("Sound Clip"),
  std::string("Gospel"),
  std::string("Noise"),
  std::string("AlternRock"),
  std::string("Bass"),
  std::string("Soul"),
  std::string("Punk"),
  std::string("Space"),
  std::string("Meditative"),
  std::string("Instrumental Pop"),
  std::string("Instrumental Rock"),
  std::string("Ethnic"),
  std::string("Gothic"),
  std::string("Darkwave"),
  std::string("Techno-Industrial"),
  std::string("Electronic"),
  std::string("Pop-Folk"),
  std::string("Eurodance"),
  std::string("Dream"),
  std::string("Southern Rock"),
  std::string("Comedy"),
  std::string("Cult"),
  std::string("Gangsta"),
  std::string("Top 40"),
  std::string("Christian Rap"),
  std::string("Pop/Funk"),
  std::string("Jungle"),
  std::string("Native American"),
  std::string("Cabaret"),
  std::string("New Wave"),
  std::string("Psychadelic"),
  std::string("Rave"),
  std::string("Showtunes"),
  std::string("Trailer"),
  std::string("Lo-Fi"),
  std::string("Tribal"),
  std::string("Acid Punk"),
  std::string("Acid Jazz"),
  std::string("Polka"),
  std::string("Retro"),
  std::string("Musical"),
  std::string("Rock & Roll"),
  std::string("Hard Rock"),
  std::string("Folk"),
  std::string("Folk-Rock"),
  std::string("National Folk"),
  std::string("Swing"),
  std::string("Fast Fusion"),
  std::string("Bebob"),
  std::string("Latin"),
  std::string("Revival"),
  std::string("Celtic"),
  std::string("Bluegrass"),
  std::string("Avantgarde"),
  std::string("Gothic Rock"),
  std::string("Progressive Rock"),
  std::string("Psychedelic Rock"),
  std::string("Symphonic Rock"),
  std::string("Slow Rock"),
  std::string("Big Band"),
  std::string("Chorus"),
  std::string("Easy Listening"),
  std::string("Acoustic"),
  std::string("Humour"),
  std::string("Speech"),
  std::string("Chanson"),
  std::string("Opera"),
  std::string("Chamber Music"),
  std::string("Sonata"),
  std::string("Symphony"),
  std::string("Booty Brass"),
  std::string("Primus"),
  std::string("Porn Groove"),
  std::string("Satire"),
  std::string("Slow Jam"),
  std::string("Club"),
  std::string("Tango"),
  std::string("Samba"),
  std::string("Folklore"),
  std::string("Ballad"),
  std::string("Power Ballad"),
  std::string("Rhytmic Soul"),
  std::string("Freestyle"),
  std::string("Duet"),
  std::string("Punk Rock"),
  std::string("Drum Solo"),
  std::string("A Capela"),
  std::string("Euro-House"),
  std::string("Dance Hall"),
  }};

boost::optional<std::string>
scribbu::text_for_genre(unsigned char genre)
{
  using std::string;

  if (scribbu::detail::GENRES.size() <= genre) {
    return boost::none;
  }

  return boost::optional<string>(scribbu::detail::GENRES[genre]);
}

std::tuple<std::string, unsigned char, std::size_t>
scribbu::match_winamp_genre(const std::string &text)
{
  using namespace std;
  using scribbu::text_for_genre;

  // `text' is presumed to be UTF-8 encoded; this method works in UTF-32 (so
  // that each code point is one string element).
  wstring_convert<codecvt_utf8<char32_t>, char32_t> u32conv;
  u32string wtext = u32conv.from_bytes(text);
  // make lower-case using the system locale
  // NB seems sketchy to me, but appears to work; the problem is that the std lib provides
  // no ctype specialization for char32_t, just char & wchar_t.
  for_each(wtext.begin(), wtext.end(), [](char32_t &c) { c = tolower((wchar_t)c, locale("")); });
  // Throughout, `n' will be the length of `wtext'
  size_t n = wtext.length();

  // This function uses the algorithm of Berhel & Roach to, which works in terms
  // of a single two-dimensional array FKP that is re-used over all
  // comparisons. `inf' need only be greater than or equal to the length of the
  // longest string we will be comparing.
  size_t inf = max(MAX_GENRE_LENGTH, n);
  // We can build FKP here & re-use it for each individual comparison. FKP is a
  // two-dimensional array consisting of `max_k' rows & `max_p' columns. In the
  // exposition, the indicies run over -m to n (all diagonals) and from -1 to
  // the maxium p (max(m,n) = `inf') respectivey.
  size_t max_k = inf + inf + 1;
  size_t max_p = inf + 2;

  // That means that to lookup the value for f(k,p), we need to index as:
  // FKP[k + zero_k][p+1] (i.e. the exposition is incorrect in this regard).
  ptrdiff_t zero_k = inf;

  // In testing, a naked 2D array far out-performed a vector of vectors. I'm not
  // entirely sure why; profiling showed a lot of time being spent in operator[]
  // for the vector-of-vectors implementation. For development purposes, I'm
  // going to use the C99 feature & gcc extension of Variable Length Array. Once
  // I have a working implementation with unit tests, I'll replace it with an
  // equivalent & standard-compliant 1D array into which I compute my own
  // indexing.

  // TODO(sp1ff): replace FKP with a one-dimensional array

# ifdef HAVE_C_VARARRAYS
  ptrdiff_t FKP[max_k][max_p];
  for (ptrdiff_t i = 0; i < max_k; ++i) {
    for (ptrdiff_t j = 0; j < max_p; ++j) {
      FKP[i][j] = -inf;
    }
  }
# else
#   error "initial implementation requires Variable Length Array support"
# endif

  for (ptrdiff_t k = - zero_k; k <= zero_k; ++k) {
    ptrdiff_t abs_k = k;
    if (k < 0) abs_k = -k;
    for (ptrdiff_t p = -1; p <= (ptrdiff_t)inf; ++p) {
      if (p == abs_k - 1) {
        if (k < 0) {
          FKP[k + zero_k][p+1] = abs_k - 1;
        } else {
          FKP[k + zero_k][p+1] = -1;
        }
      }
    }
  }

  // OK-- `FKP' is initialized. Now iterate over all genres, comparing `wtext'
  // to each; return the match with minimal Damerau-Levenshtein distance.
  size_t least_dl = inf;
  unsigned char best_genre;
  for (size_t i = 0; i < NUM_GENRES; ++i) {
    size_t DL = damerau_levenshtein(GENRESW[i], wtext, n, (ptrdiff_t*)FKP, max_k, max_p, zero_k, inf);
    if (DL < least_dl) {
      least_dl = DL;
      best_genre = i;
    }
  }

  return make_tuple(text_for_genre(best_genre).get(), best_genre, least_dl);

}
