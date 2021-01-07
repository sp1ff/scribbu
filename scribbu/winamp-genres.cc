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
#include <string>

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
    /*  0*/ std::string("Blues"),
    /*  1*/ std::string("Classic Rock"),
    /*  2*/ std::string("Country"),
    /*  3*/ std::string("Dance"),
    /*  4*/ std::string("Disco"),
    /*  5*/ std::string("Funk"),
    /*  6*/ std::string("Grunge"),
    /*  7*/ std::string("Hip-Hop"),
    /*  8*/ std::string("Jazz"),
    /*  9*/ std::string("Metal"),
    /* 10*/ std::string("New Age"),
    /* 11*/ std::string("Oldies"),
    /* 12*/ std::string("Other"),
    /* 13*/ std::string("Pop"),
    /* 14*/ std::string("R&B"),
    /* 15*/ std::string("Rap"),
    /* 16*/ std::string("Reggae"),
    /* 17*/ std::string("Rock"),
    /* 18*/ std::string("Techno"),
    /* 19*/ std::string("Industrial"),
    /* 20*/ std::string("Alternative"),
    /* 21*/ std::string("Ska"),
    /* 22*/ std::string("Death Metal"),
    /* 23*/ std::string("Pranks"),
    /* 24*/ std::string("Soundtrack"),
    /* 25*/ std::string("Euro-Techno"),
    /* 26*/ std::string("Ambient"),
    /* 27*/ std::string("Trip-Hop"),
    /* 28*/ std::string("Vocal"),
    /* 29*/ std::string("Jazz+Funk"),
    /* 30*/ std::string("Fusion"),
    /* 31*/ std::string("Trance"),
    /* 32*/ std::string("Classical"),
    /* 33*/ std::string("Instrumental"),
    /* 34*/ std::string("Acid"),
    /* 35*/ std::string("House"),
    /* 36*/ std::string("Game"),
    /* 37*/ std::string("Sound Clip"),
    /* 38*/ std::string("Gospel"),
    /* 39*/ std::string("Noise"),
    /* 40*/ std::string("AlternRock"),
    /* 41*/ std::string("Bass"),
    /* 42*/ std::string("Soul"),
    /* 43*/ std::string("Punk"),
    /* 44*/ std::string("Space"),
    /* 45*/ std::string("Meditative"),
    /* 46*/ std::string("Instrumental Pop"),
    /* 47*/ std::string("Instrumental Rock"),
    /* 48*/ std::string("Ethnic"),
    /* 49*/ std::string("Gothic"),
    /* 50*/ std::string("Darkwave"),
    /* 51*/ std::string("Techno-Industrial"),
    /* 52*/ std::string("Electronic"),
    /* 53*/ std::string("Pop-Folk"),
    /* 54*/ std::string("Eurodance"),
    /* 55*/ std::string("Dream"),
    /* 56*/ std::string("Southern Rock"),
    /* 57*/ std::string("Comedy"),
    /* 58*/ std::string("Cult"),
    /* 59*/ std::string("Gangsta"),
    /* 60*/ std::string("Top 40"),
    /* 61*/ std::string("Christian Rap"),
    /* 62*/ std::string("Pop/Funk"),
    /* 63*/ std::string("Jungle"),
    /* 64*/ std::string("Native American"),
    /* 65*/ std::string("Cabaret"),
    /* 66*/ std::string("New Wave"),
    /* 67*/ std::string("Psychadelic"),
    /* 68*/ std::string("Rave"),
    /* 69*/ std::string("Showtunes"),
    /* 70*/ std::string("Trailer"),
    /* 71*/ std::string("Lo-Fi"),
    /* 72*/ std::string("Tribal"),
    /* 73*/ std::string("Acid Punk"),
    /* 74*/ std::string("Acid Jazz"),
    /* 75*/ std::string("Polka"),
    /* 76*/ std::string("Retro"),
    /* 77*/ std::string("Musical"),
    /* 78*/ std::string("Rock & Roll"),
    /* 79*/ std::string("Hard Rock"),
    /* 80*/ std::string("Folk"),
    /* 81*/ std::string("Folk-Rock"),
    /* 82*/ std::string("National Folk"),
    /* 83*/ std::string("Swing"),
    /* 84*/ std::string("Fast Fusion"),
    /* 85*/ std::string("Bebob"),
    /* 86*/ std::string("Latin"),
    /* 87*/ std::string("Revival"),
    /* 88*/ std::string("Celtic"),
    /* 89*/ std::string("Bluegrass"),
    /* 90*/ std::string("Avantgarde"),
    /* 91*/ std::string("Gothic Rock"),
    /* 92*/ std::string("Progressive Rock"),
    /* 93*/ std::string("Psychedelic Rock"),
    /* 94*/ std::string("Symphonic Rock"),
    /* 95*/ std::string("Slow Rock"),
    /* 96*/ std::string("Big Band"),
    /* 97*/ std::string("Chorus"),
    /* 98*/ std::string("Easy Listening"),
    /* 99*/ std::string("Acoustic"),
    /*100*/ std::string("Humour"),
    /*101*/ std::string("Speech"),
    /*102*/ std::string("Chanson"),
    /*103*/ std::string("Opera"),
    /*104*/ std::string("Chamber Musice"),
    /*105*/ std::string("Sonata"),
    /*106*/ std::string("Symphony"),
    /*107*/ std::string("Booty Brass"),
    /*108*/ std::string("Primus"),
    /*109*/ std::string("Porn Groove"),
    /*110*/ std::string("Satire"),
    /*111*/ std::string("Slow Jam"),
    /*112*/ std::string("Club"),
    /*113*/ std::string("Tango"),
    /*114*/ std::string("Samba"),
    /*115*/ std::string("Folklore"),
    /*116*/ std::string("Ballad"),
    /*117*/ std::string("Power Ballad"),
    /*118*/ std::string("Rhytmic Soul"),
    /*119*/ std::string("Freestyle"),
    /*120*/ std::string("Duet"),
    /*121*/ std::string("Punk Rock"),
    /*122*/ std::string("Drum Solo"),
    /*123*/ std::string("A Capela"),
    /*124*/ std::string("Euro-House"),
    /*125*/ std::string("Dance Hall"),
    /*126*/ std::string("Goa"),
    /*127*/ std::string("Drum & Bass"),
    /*128*/ std::string("Club-House"),
    /*129*/ std::string("Hardcore Techno"),
    /*130*/ std::string("Terror"),
    /*131*/ std::string("Indie"),
    /*132*/ std::string("BritPop"),
    /*133*/ std::string("Negerpunk"),
    /*134*/ std::string("Polsk Punk"),
    /*135*/ std::string("Beat"),
    /*136*/ std::string("Christian Gangsta Rap"),
    /*137*/ std::string("Heavy Metal"),
    /*138*/ std::string("Black Metal"),
    /*139*/ std::string("Crossover"),
    /*140*/ std::string("Contemporary Christian"),
    /*141*/ std::string("Christian rock"),
    /*142*/ std::string("Merengue"),
    /*143*/ std::string("Salsa"),
    /*144*/ std::string("Thrash Metal"),
    /*145*/ std::string("Anime"),
    /*146*/ std::string("Jpop"),
    /*147*/ std::string("Synthpop"),
    /*148*/ std::string("Abstract"),
    /*149*/ std::string("Art Rock"),
    /*150*/ std::string("Baroque"),
    /*151*/ std::string("Bhangra"),
    /*152*/ std::string("Big beat"),
    /*153*/ std::string("Breakbeat"),
    /*154*/ std::string("Chillout"),
    /*155*/ std::string("Downtempo"),
    /*156*/ std::string("Dub"),
    /*157*/ std::string("EBM"),
    /*158*/ std::string("Eclectic"),
    /*159*/ std::string("Electro"),
    /*160*/ std::string("Electroclash"),
    /*161*/ std::string("Emo"),
    /*162*/ std::string("Experimental"),
    /*163*/ std::string("Garage"),
    /*164*/ std::string("Global"),
    /*165*/ std::string("IDM"),
    /*166*/ std::string("Illbient"),
    /*167*/ std::string("Industro-Goth"),
    /*168*/ std::string("Jam Band"),
    /*169*/ std::string("Krautrock"),
    /*170*/ std::string("Leftfield"),
    /*171*/ std::string("Lounge"),
    /*172*/ std::string("Math Rock"),
    /*173*/ std::string("New Romantic"),
    /*174*/ std::string("Nu-Breakz"),
    /*175*/ std::string("Post-Punk"),
    /*176*/ std::string("Post-Rock"),
    /*177*/ std::string("Psytrance"),
    /*178*/ std::string("Shoegaze"),
    /*179*/ std::string("Space Rock"),
    /*180*/ std::string("Trop Rock"),
    /*181*/ std::string("World Music"),
    /*182*/ std::string("Neoclassical"),
    /*183*/ std::string("Audiobook"),
    /*184*/ std::string("Audio theatre"),
    /*185*/ std::string("Neue Deutsche Welle"),
    /*186*/ std::string("Podcast"),
    /*187*/ std::string("Indie-Rock"),
    /*188*/ std::string("G-Funk"),
    /*189*/ std::string("Dubstep"),
    /*190*/ std::string("Garage Rock"),
    /*191*/ std::string("Psybient"),
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
  // for the vector-of-vectors implementation. For development purposes, I used
  // the C99 feature & gcc extension of Variable Length Array. Once I had a
  // working implementation with unit tests, I replaced it with an equivalent &
  // standard-compliant 1D array into which I compute my own indexing.
  ptrdiff_t FKP[max_k*max_p];
  for (ptrdiff_t i = 0; i < max_k*max_p; ++i) {
    FKP[i] = -inf;
  }

  for (ptrdiff_t k = - zero_k; k <= zero_k; ++k) {
    ptrdiff_t abs_k = k;
    if (k < 0) abs_k = -k;
    for (ptrdiff_t p = -1; p <= (ptrdiff_t)inf; ++p) {
      if (p == abs_k - 1) {
        if (k < 0) {
          FKP[(k + zero_k)*max_p+p+1] = abs_k - 1; // FKP[k + zero_k][p+1]
        } else {
          FKP[(k + zero_k)*max_p+p+1] = -1;        // FKP[k + zero_k][p+1]
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
