/**
 * \file scheme-serde.cc
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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

#include "scheme-serde.hh"

#include "dynwind-context.hh"

const scribbu::frame_id3 TAL("TAL");
const scribbu::frame_id3 TBP("TBP");
const scribbu::frame_id3 TCM("TCM");
const scribbu::frame_id3 TCO("TCO");
const scribbu::frame_id3 TCR("TCR");
const scribbu::frame_id3 TDA("TDA");
const scribbu::frame_id3 TDY("TDY");
const scribbu::frame_id3 TEN("TEN");
const scribbu::frame_id3 TFT("TFT");
const scribbu::frame_id3 TIM("TIM");
const scribbu::frame_id3 TKE("TKE");
const scribbu::frame_id3 TLA("TLA");
const scribbu::frame_id3 TLE("TLE");
const scribbu::frame_id3 TMT("TMT");
const scribbu::frame_id3 TOA("TOA");
const scribbu::frame_id3 TOF("TOF");
const scribbu::frame_id3 TOL("TOL");
const scribbu::frame_id3 TOR("TOR");
const scribbu::frame_id3 TOT("TOT");
const scribbu::frame_id3 TP1("TP1");
const scribbu::frame_id3 TP2("TP2");
const scribbu::frame_id3 TP3("TP3");
const scribbu::frame_id3 TP4("TP4");
const scribbu::frame_id3 TPA("TPA");
const scribbu::frame_id3 TPB("TPB");
const scribbu::frame_id3 TRC("TRC");
const scribbu::frame_id3 TRD("TRD");
const scribbu::frame_id3 TRK("TRK");
const scribbu::frame_id3 TSI("TSI");
const scribbu::frame_id3 TSS("TSS");
const scribbu::frame_id3 TT1("TT1");
const scribbu::frame_id3 TT2("TT2");
const scribbu::frame_id3 TT3("TT3");
const scribbu::frame_id3 TXT("TXT");
const scribbu::frame_id3 TYE("TYE");

const scribbu::frame_id3 IDCOM("COM");
const scribbu::frame_id3 IDTXX("TXX");
const scribbu::frame_id3 IDCNT("CNT");
const scribbu::frame_id3 IDPOP("POP");
const scribbu::frame_id3 IDXTG("XTG");

const scribbu::frame_id4 TALB("TALB");
const scribbu::frame_id4 TBPM("TBPM");
const scribbu::frame_id4 TCOM("TCOM");
const scribbu::frame_id4 TCON("TCON");
const scribbu::frame_id4 TCOP("TCOP");
const scribbu::frame_id4 TDAT("TDAT");
const scribbu::frame_id4 TDLY("TDLY");
const scribbu::frame_id4 TENC("TENC");
const scribbu::frame_id4 TEXT("TEXT");
const scribbu::frame_id4 TFLT("TFLT");
const scribbu::frame_id4 TIME("TIME");
const scribbu::frame_id4 TIT1("TIT1");
const scribbu::frame_id4 TIT2("TIT2");
const scribbu::frame_id4 TIT3("TIT3");
const scribbu::frame_id4 TKEY("TKEY");
const scribbu::frame_id4 TLAN("TLAN");
const scribbu::frame_id4 TLEN("TLEN");
const scribbu::frame_id4 TMED("TMED");
const scribbu::frame_id4 TOAL("TOAL");
const scribbu::frame_id4 TOFN("TOFN");
const scribbu::frame_id4 TOLY("TOLY");
const scribbu::frame_id4 TOPE("TOPE");
const scribbu::frame_id4 TORY("TORY");
const scribbu::frame_id4 TOWN("TOWN");
const scribbu::frame_id4 TPE1("TPE1");
const scribbu::frame_id4 TPE2("TPE2");
const scribbu::frame_id4 TPE3("TPE3");
const scribbu::frame_id4 TPE4("TPE4");
const scribbu::frame_id4 TPOS("TPOS");
const scribbu::frame_id4 TPUB("TPUB");
const scribbu::frame_id4 TRCK("TRCK");
const scribbu::frame_id4 TRDA("TRDA");
const scribbu::frame_id4 TRSN("TRSN");
const scribbu::frame_id4 TRSO("TRSO");
const scribbu::frame_id4 TSIZ("TSIZ");
const scribbu::frame_id4 TSRC("TSRC");
const scribbu::frame_id4 TSSE("TSSE");
const scribbu::frame_id4 TYER("TYER");

const scribbu::frame_id4 IDCOMM("COMM");
const scribbu::frame_id4 IDTXXX("TXXX");
const scribbu::frame_id4 IDPCNT("PCNT");
const scribbu::frame_id4 IDPOPM("POPM");
const scribbu::frame_id4 IDXTAG("XTAG");

SCM sym_unknown_frame;              // 'unknown-frame

SCM sym_album_frame;                // 'album-frame, TAL/TALB
SCM sym_artist_frame;               // 'artist-frame, TP1/TPE1
SCM sym_band_frame;                 // 'band-frame, TP2/TPE2
SCM sym_bpm_frame;                  // 'bpm-frame, TBP/TBPM
SCM sym_composer_frame;             // 'composer-frame, TCM/TCOM
SCM sym_conductor_frame;            // 'conductor-frame, TP3/TPE3
SCM sym_content_group_frame;        // 'content-group-frame, TT1/TIT1
SCM sym_copyright_frame;            // 'copyright-frame, TCR/TCOP
SCM sym_date_frame;                 // 'date-frame, TDA/TDAT
SCM sym_encoded_by_frame;           // 'encoded-by-frame, TEN/TENC
SCM sym_file_owner_frame;           // 'file-owner-frame, TOWN
SCM sym_file_type_frame;            // 'file-type-frame, TFT/TFLT
SCM sym_genre_frame;                // 'genre-frame, TCO/TCON
SCM sym_initial_key_frame;          // 'initial-key-frame, TKE/TKEY
SCM sym_interpreted_by_frame;       // 'interpreted-by-frame, TP4/TPE4
SCM sym_isrc_frame;                 // 'isrc-frame, TRC/TSRC
SCM sym_langs_frame;                // 'langs-frame, TLA/TLAN
SCM sym_length_frame;               // 'length-frame, TLE/TLEN
SCM sym_lyricist_frame;             // 'lyricist-frame, TXT/TEXT
SCM sym_media_type_frame;           // 'media-type-frame, TMT/TMED
SCM sym_original_album_frame;       // 'original-album-frame, TOT/TOAL
SCM sym_original_artist_frame;      // 'original-artist-frame, TOA/TOPE
SCM sym_original_filename_frame;    // 'original-filename-frame, TOF/TOFN
SCM sym_original_lyricist_frame;    // 'original-lyricist-frame, TOL/TOLY
SCM sym_original_release_year_frame;// 'original-release-year-frame, TOR/TORY
SCM sym_part_of_a_set_frame;        // 'part-of-a-set-frame, TPA/TPOS
SCM sym_playlist_delay_frame;       // 'playlist-delay-frame, TDY/TDLY
SCM sym_publisher_frame;            // 'publisher-frame, TPB/TPUB
SCM sym_recording_dates_frame;      // 'recording-dates-frame, TRD/TRDA
SCM sym_settings_frame;             // 'settings-frame, TSS/TSSE
SCM sym_size_frame;                 // 'size-frame, TSI/TSIZ
SCM sym_station_name_frame;         // 'station-name-frame, TRSN
SCM sym_station_owner_frame;        // 'station-owner-frame, TRSO
SCM sym_subtitle_frame;             // 'subtitle-frame, TT3/TIT3
SCM sym_time_frame;                 // 'time-frame, TIM/TIME
SCM sym_title_frame;                // 'title-frame, TT2/TIT2
SCM sym_track_frame;                // 'track-frame, TRK/TRCK
SCM sym_year_frame;                 // 'year-frame, TYE/TYER

SCM sym_comment_frame;              // 'comment-frame, COM/COMM
SCM sym_udt_frame;                  // 'udt-frame, TXX/TXXX
SCM sym_play_count_frame;           // 'play-count-frame, CNT/PCNT
SCM sym_popm_frame;                 // 'popm-frame, POP/POPM
SCM sym_tag_cloud_frame;            // 'tag-cloud-frame, XTG/XTAG

SCM scribbu::sym_as_needed;
SCM scribbu::kw_apply_unsync;
SCM scribbu::kw_copy;

std::unordered_map<scribbu::frame_id3, SCM> id3lu;
std::unordered_map<scribbu::frame_id4, SCM> id4lu;
std::unordered_map<std::string, scribbu::frame_id3> id3rlu;
std::unordered_map<std::string, scribbu::frame_id4> id4rlu;

void scribbu::init_symbols() {

  sym_unknown_frame = sym_for_utf8("unknown-frame");

# define DEFSYMX(var, sym, id3, id4)                                \
  sym_ ## var = scm_string_to_symbol(scm_from_utf8_string(#sym));   \
  id3lu[id3] = sym_ ## var;                                         \
  id4lu[id4] = sym_ ## var;                                         \
  id3rlu[#sym] = id3;                                               \
  id4rlu[#sym] = id4

# define DEFSYM4(var, sym, id4)                                     \
  sym_ ## var = scm_string_to_symbol(scm_from_utf8_string(#sym));   \
  id4lu[id4] = sym_ ## var;                                         \
  id4rlu[#sym] = id4

  DEFSYM4(file_owner_frame,            file-owner-frame,                 TOWN);
  DEFSYM4(station_name_frame,          station-name-frame,               TRSN);
  DEFSYM4(station_owner_frame,         station-owner-frame,              TRSO);
  DEFSYMX(album_frame,                 album-frame,                 TAL, TALB);
  DEFSYMX(artist_frame,                artist-frame,                TP1, TPE1);
  DEFSYMX(band_frame,                  band-frame,                  TP2, TPE2);
  DEFSYMX(bpm_frame,                   bpm-frame,                   TBP, TBPM);
  DEFSYMX(composer_frame,              composer-frame,              TCM, TCOM);
  DEFSYMX(conductor_frame,             conductor-frame,             TP3, TPE3);
  DEFSYMX(content_group_frame,         content-group-frame,         TT1, TIT1);
  DEFSYMX(copyright_frame,             copyright-frame,             TCR, TCOP);
  DEFSYMX(date_frame,                  date-frame,                  TDA, TDAT);
  DEFSYMX(encoded_by_frame,            encoded-by-frame,            TEN, TENC);
  DEFSYMX(file_type_frame,             file-type-frame,             TFT, TFLT);
  DEFSYMX(genre_frame,                 genre-frame,                 TCO, TCON);
  DEFSYMX(initial_key_frame,           initial-key-frame,           TKE, TKEY);
  DEFSYMX(interpreted_by_frame,        interpreted-by-frame,        TP4, TPE4);
  DEFSYMX(isrc_frame,                  isrc-frame,                  TRC, TSRC);
  DEFSYMX(langs_frame,                 langs-frame,                 TLA, TLAN);
  DEFSYMX(length_frame,                length-frame,                TLE, TLEN);
  DEFSYMX(lyricist_frame,              lyricist-frame,              TXT, TEXT);
  DEFSYMX(media_type_frame,            media-type-frame,            TMT, TMED);
  DEFSYMX(original_album_frame,        original-album-frame,        TOT, TOAL);
  DEFSYMX(original_artist_frame,       original-artist-frame,       TOA, TOPE);
  DEFSYMX(original_filename_frame,     original-filename-frame,     TOF, TOFN);
  DEFSYMX(original_lyricist_frame,     original-lyricist-frame,     TOL, TOLY);
  DEFSYMX(original_release_year_frame, original-release-year-frame, TOR, TORY);
  DEFSYMX(part_of_a_set_frame,         part-of-a-set-frame,         TPA, TPOS);
  DEFSYMX(playlist_delay_frame,        playlist-delay-frame,        TDY, TDLY);
  DEFSYMX(publisher_frame,             publisher-frame,             TPB, TPUB);
  DEFSYMX(recording_dates_frame,       recording-dates-frame,       TRD, TRDA);
  DEFSYMX(settings_frame,              settings-frame,              TSS, TSSE);
  DEFSYMX(size_frame,                  size-frame,                  TSI, TSIZ);
  DEFSYMX(subtitle_frame,              subtitle-frame,              TT3, TIT3);
  DEFSYMX(time_frame,                  time-frame,                  TIM, TIME);
  DEFSYMX(title_frame,                 title-frame,                 TT2, TIT2);
  DEFSYMX(track_frame,                 track-frame,                 TRK, TRCK);
  DEFSYMX(year_frame,                  year-frame,                  TYE, TYER);
  DEFSYMX(year_frame,                  year-frame,                  TYE, TYER);

  DEFSYMX(comment_frame,               comment-frame,               IDCOM, IDCOMM);
  DEFSYMX(udt_frame,                   udt-frame,                   IDTXX, IDTXXX);
  DEFSYMX(play_count_frame,            play-count-frame,            IDCNT, IDPCNT);
  DEFSYMX(popm_frame,                  popm-frame,                  IDPOP, IDPOPM);
  DEFSYMX(tag_cloud_frame,             tag-cloud-frame,             IDXTG, IDXTAG);

# undef DEFSYM4
# undef DEFSYMX

  sym_as_needed = scm_from_utf8_symbol("as-needed");
  kw_apply_unsync = scm_from_utf8_keyword("apply-unsync");
  kw_copy = scm_from_utf8_keyword("copy");

}

////////////////////////////////////////////////////////////////////////////////
//                                utility code
////////////////////////////////////////////////////////////////////////////////

namespace {

  SCM
  init_frame(const std::string &cls, SCM id)
  {
    // NB. on failure, the scm_* functions do not return
    SCM x = scm_make(scm_list_1(scm_c_public_ref("scribbu", cls.c_str())));
    SCM_SET_SLOT(x, 0, id);
    return x;
  }

  SCM init_frame(const std::string &cls,
                 SCM id,
                 scribbu::id3v2_3_plus_frame::tag_alter_preservation  tap,
                 scribbu::id3v2_3_plus_frame::file_alter_preservation fap,
                 scribbu::id3v2_3_plus_frame::read_only ro)
  {
    typedef scribbu::id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef scribbu::id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef scribbu::id3v2_3_plus_frame::read_only read_only;

    SCM x = init_frame(cls, id);
    SCM_SET_SLOT(x, 1, tag_alter_preservation::preserve == tap ?
                 SCM_BOOL_T : SCM_BOOL_F);
    SCM_SET_SLOT(x, 2, file_alter_preservation::preserve == fap ?
                 SCM_BOOL_T : SCM_BOOL_F);
    SCM_SET_SLOT(x, 3, ro == read_only::set ? SCM_BOOL_T : SCM_BOOL_F);
    return x;
  }

  SCM init_frame(const std::string &cls,
                 SCM id,
                 scribbu::id3v2_3_plus_frame::tag_alter_preservation  tap,
                 scribbu::id3v2_3_plus_frame::file_alter_preservation fap,
                 scribbu::id3v2_3_plus_frame::read_only  ro,
                 bool unsync)
  {
    SCM x = init_frame(cls, id, tap, fap, ro);
    SCM_SET_SLOT(x, 4, unsync ? SCM_BOOL_T : SCM_BOOL_F);
    return x;
  }

  std::string
  ser_frame_2_2(const std::string &cls, SCM scm,
                scribbu::dynwind_context &ctx)
  {
    SCM scm_cls = scm_class_of(scm);
    SCM scm_txt_cls = scm_c_public_ref("scribbu", cls.c_str());
    if (SCM_BOOL_T != scm_equal_p(scm_cls, scm_txt_cls)) {
      throw std::logic_error("not a text frame");
    }

    return ctx.free_utf8_string(scm_symbol_to_string(SCM_SLOT(scm, 0)));
  }

  std::tuple<std::string,
             scribbu::id3v2_3_plus_frame::tag_alter_preservation,
             scribbu::id3v2_3_plus_frame::file_alter_preservation,
             scribbu::id3v2_3_plus_frame::read_only>
  ser_frame_2_3(const std::string &cls, SCM scm,
                scribbu::dynwind_context &ctx)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    string id = ser_frame_2_2(cls, scm, ctx);

    tag_alter_preservation tap = tag_alter_preservation::preserve;
    if (SCM_BOOL_F == SCM_SLOT(scm, 1)) {
      tap = tag_alter_preservation::discard;
    }
    file_alter_preservation fap = file_alter_preservation::preserve;
    if (SCM_BOOL_F == SCM_SLOT(scm, 2)) {
      fap = file_alter_preservation::discard;
    }
    read_only ro = read_only::clear;
    if (SCM_BOOL_T == SCM_SLOT(scm, 3)) {
      ro = read_only::set;
    }

    return std::make_tuple(id, tap, fap, ro);
  }

  std::tuple<std::string,
             scribbu::id3v2_3_plus_frame::tag_alter_preservation,
             scribbu::id3v2_3_plus_frame::file_alter_preservation,
             scribbu::id3v2_3_plus_frame::read_only,
             bool>
  ser_frame_2_4(const std::string &cls, SCM scm,
                scribbu::dynwind_context &ctx)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    string id;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    std::tie(id, tap, fap, ro) = ser_frame_2_3(cls, scm, ctx);

    bool unsync = false;
    if (SCM_BOOL_T == SCM_SLOT(scm, 4)) {
      unsync = true;
    }

    return std::make_tuple(id, tap, fap, ro, unsync);
  }

}

////////////////////////////////////////////////////////////////////////////////
//                            deserialization code
////////////////////////////////////////////////////////////////////////////////

namespace {

  SCM
  de_text_frame_2_2(const scribbu::id3v2_2_text_frame &f, bool unsync)
  {
    auto p = id3lu.find(f.id());
    if (p == id3lu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    SCM x = init_frame("<text-frame>", p->second);
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(f.as_str<std::string>().c_str()));

    return x;
  }

  SCM
  de_text_frame_2_3(const scribbu::id3v2_3_text_frame &f, bool unsync)
  {
    auto p = id4lu.find(f.id());
    if (p == id4lu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    SCM x = init_frame("<text-frame>", p->second, f.tag_alter_preserve(),
                       f.file_alter_preserve(), f.readonly());
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(f.as_str<std::string>().c_str()));

    return x;

  }

  SCM
  de_text_frame_2_4(const scribbu::id3v2_4_text_frame &f, bool unsync)
  {
    auto p = id4lu.find(f.id());
    if (p == id4lu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    SCM x = init_frame("<text-frame>", p->second, f.tag_alter_preserve(),
                       f.file_alter_preserve(), f.readonly(),
                       f.unsynchronised());
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(f.as_str<std::string>().c_str()));

    return x;

  }

  SCM
  de_comment_2_2(const scribbu::COM &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<comment-frame>", sym_comment_frame);

    char lang[4] = { 0, 0, 0, 0 };
    std::tie(lang[0], lang[1], lang[2]) = f.lang();
    if (0 != (lang[0] & 0x80)) lang[0] = 0;
    if (0 != (lang[1] & 0x80)) lang[1] = 0;
    if (0 != (lang[2] & 0x80)) lang[2] = 0;
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(lang));

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 7, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_comment_2_3(const scribbu::COMM &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<comment-frame>", sym_comment_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly());

    char lang[4] = { 0, 0, 0, 0 };
    std::tie(lang[0], lang[1], lang[2]) = f.lang();
    if (0 != (lang[0] & 0x80)) lang[0] = 0;
    if (0 != (lang[1] & 0x80)) lang[1] = 0;
    if (0 != (lang[2] & 0x80)) lang[2] = 0;
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(lang));

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 7, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_comment_2_4(const scribbu::COMM_2_4 &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<comment-frame>", sym_comment_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly(), f.unsynchronised());

    char lang[4] = { 0, 0, 0, 0 };
    std::tie(lang[0], lang[1], lang[2]) = f.lang();
    if (0 != (lang[0] & 0x80)) lang[0] = 0;
    if (0 != (lang[1] & 0x80)) lang[1] = 0;
    if (0 != (lang[2] & 0x80)) lang[2] = 0;
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(lang));

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 7, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_udt_2_2(const scribbu::TXX &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<user-defined-text-frame>",
                       sym_udt_frame);

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_udt_2_3(const scribbu::TXXX &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<user-defined-text-frame>",
                       sym_udt_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly());

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_udt_2_4(const scribbu::TXXX_2_4 &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<user-defined-text-frame>",
                       sym_udt_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly(), f.unsynchronised());

    string dsc = f.description<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(dsc.c_str()));

    string txt = f.text<string>();
    SCM_SET_SLOT(x, 6, scm_from_utf8_string(txt.c_str()));

    return x;

  }

  SCM
  de_cnt_2_2(const scribbu::CNT &f, bool unsync)
  {
    SCM x = init_frame("<play-count-frame>", sym_play_count_frame);
    SCM_SET_SLOT(x, 5, scm_from_size_t(f.count()));
    return x;
  }

  SCM
  de_cnt_2_3(const scribbu::PCNT &f, bool unsync)
  {
    SCM x = init_frame("<play-count-frame>", sym_play_count_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly());
    SCM_SET_SLOT(x, 5, scm_from_size_t(f.count()));
    return x;
  }

  SCM
  de_cnt_2_4(const scribbu::PCNT_2_4 &f, bool unsync)
  {
    SCM x = init_frame("<play-count-frame>", sym_play_count_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly(), f.unsynchronised());
    SCM_SET_SLOT(x, 5, scm_from_size_t(f.count()));
    return x;
  }

  SCM
  de_popm_2_2(const scribbu::POP &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<popm-frame>", sym_popm_frame);

    string own = f.email<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    unsigned char rating = f.rating();
    SCM_SET_SLOT(x, 6, scm_from_uchar(rating));

    size_t count = f.count();
    SCM_SET_SLOT(x, 7, scm_from_size_t(count));

    return x;
  }

  SCM
  de_popm_2_3(const scribbu::POPM &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<popm-frame>", sym_popm_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly());

    string own = f.email<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    unsigned char rating = f.rating();
    SCM_SET_SLOT(x, 6, scm_from_uchar(rating));

    size_t count = f.count();
    SCM_SET_SLOT(x, 7, scm_from_size_t(count));

    return x;
  }

  SCM
  de_popm_2_4(const scribbu::POPM_2_4 &f, bool unsync)
  {
    using namespace std;

    SCM x = init_frame("<popm-frame>", sym_popm_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly(), f.unsynchronised());

    string own = f.email<string>();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    unsigned char rating = f.rating();
    SCM_SET_SLOT(x, 6, scm_from_uchar(rating));

    size_t count = f.count();
    SCM_SET_SLOT(x, 7, scm_from_size_t(count));

    return x;
  }

  SCM
  de_xtag_2_2(const scribbu::XTG &f, bool unsync)
  {
    using namespace std;
    using namespace scribbu;

    SCM x = init_frame("<tag-cloud-frame>", sym_tag_cloud_frame);

    string own = f.owner();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    SCM lst = SCM_EOL;
    for (auto kv: f) {
      vector<SCM> vals;
      transform(kv.second.begin(), kv.second.end(), back_inserter(vals),
                [] (const string &s) {
                  return scm_from_stringn(s.c_str(), s.length(), "UTF-8",
                                          SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
                } );
      SCM scm_vals = scm_list_for_range(vals.begin(), vals.end());
      lst = scm_acons(scm_from_utf8_string(kv.first.c_str()), scm_vals, lst);
    }
    SCM_SET_SLOT(x, 6, lst);

    return x;
  }

  SCM
  de_xtag_2_3(const scribbu::XTAG &f, bool unsync)
  {
    using namespace std;
    using namespace scribbu;

    SCM x = init_frame("<tag-cloud-frame>", sym_tag_cloud_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly());

    string own = f.owner();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    SCM lst = SCM_EOL;
    for (auto kv: f) {
      vector<SCM> vals;
      transform(kv.second.begin(), kv.second.end(), back_inserter(vals),
                [] (const string &s) {
                  return scm_from_stringn(s.c_str(), s.length(), "UTF-8",
                                          SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
                } );
      SCM scm_vals = scm_list_for_range(vals.begin(), vals.end());
      lst = scm_acons(scm_from_utf8_string(kv.first.c_str()), scm_vals, lst);
    }
    SCM_SET_SLOT(x, 6, lst);

    return x;
  }

  SCM
  de_xtag_2_4(const scribbu::XTAG_2_4 &f, bool unsync)
  {
    using namespace std;
    using namespace scribbu;

    SCM x = init_frame("<tag-cloud-frame>", sym_tag_cloud_frame,
                       f.tag_alter_preserve(), f.file_alter_preserve(),
                       f.readonly(), f.unsynchronised());

    string own = f.owner();
    SCM_SET_SLOT(x, 5, scm_from_utf8_string(own.c_str()));

    SCM lst = SCM_EOL;
    for (auto kv: f) {
      vector<SCM> vals;
      transform(kv.second.begin(), kv.second.end(), back_inserter(vals),
                [] (const string &s) {
                  return scm_from_stringn(s.c_str(), s.length(), "UTF-8",
                                          SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
                } );
      SCM scm_vals = scm_list_for_range(vals.begin(), vals.end());
      lst = scm_acons(scm_from_utf8_string(kv.first.c_str()), scm_vals, lst);
    }
    SCM_SET_SLOT(x, 6, lst);

    return x;
  }

}

////////////////////////////////////////////////////////////////////////////////
//                               serialization code
////////////////////////////////////////////////////////////////////////////////

namespace {

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_text_frame_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;

    string id_txt = ser_frame_2_2("<text-frame>", scm, ctx);
    auto p = id3rlu.find(id_txt);
    if (p == id3rlu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    frame_id3 id = p->second;
    string txt(ctx.free_utf8_string(SCM_SLOT(scm, 5)));

    return unique_ptr<id3v2_2_frame>(
      new id3v2_2_text_frame(id, txt, encoding::UTF_8));

  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_text_frame_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<text-frame>", scm, ctx);

    auto p = id4rlu.find(id_txt);
    if (p == id4rlu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    frame_id4 id = p->second;

    string txt(ctx.free_utf8_string(SCM_SLOT(scm, 5)));

    return unique_ptr<id3v2_3_frame>(
      new id3v2_3_text_frame(id, txt, encoding::UTF_8, false,
                             on_no_encoding::fail,
                             false, tap, fap, ro));

  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_text_frame_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;
    typedef id3v2_4_text_frame::frame_encoding frame_encoding;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) =
      ser_frame_2_4("<text-frame>", scm, ctx);

    auto p = id4rlu.find(id_txt);
    if (p == id4rlu.end()) {
      throw std::logic_error("unknown frame ID");
    }

    frame_id4 id = p->second;

    string txt(ctx.free_utf8_string(SCM_SLOT(scm, 5)));

    return unique_ptr<id3v2_4_frame>(
      new id3v2_4_text_frame(id, txt, encoding::UTF_8, frame_encoding::UTF_8,
                             false, on_no_encoding::fail,
                             tap, fap, ro,
                             boost::none,
                             boost::none,
                             false,
                             unsync));

  }

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_comment_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;
    ser_frame_2_2("<comment-frame>", scm, ctx);

    string lang_txt = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    language lang = language::eng;
    if (lang_txt.size()) {
      stringstream stm(lang_txt);
      stm >> lang;
    }

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 6));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_2_frame>(new COM(lang, text, encoding::UTF_8,
                                             use_unicode::yes, dsc));

  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_comment_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<comment-frame>", scm, ctx);

    string lang_txt = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    language lang = language::eng;
    if (lang_txt.size()) {
      stringstream stm(lang_txt);
      stm >> lang;
    }

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 6));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_3_frame>(new COMM(lang, text, encoding::UTF_8,
                                              use_unicode::yes,
                                              tap, fap, ro, boost::none,
                                              boost::none, dsc));

  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_comment_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) =
      ser_frame_2_4("<comment-frame>", scm, ctx);

    string lang_txt = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    language lang = language::eng;
    if (lang_txt.size()) {
      stringstream stm(lang_txt);
      stm >> lang;
    }

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 6));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_4_frame>(
      new COMM_2_4(lang, text, encoding::UTF_8,
                   use_unicode::yes,
                   tap, fap, ro, boost::none,
                   boost::none, false, unsync, boost::none,
                   dsc));

  }

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_udt_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;
    ser_frame_2_2("<user-defined-text-frame>", scm, ctx);

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 6));

    return unique_ptr<id3v2_2_frame>(new TXX(text, encoding::UTF_8,
                                             use_unicode::yes, dsc));

  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_udt_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<user-defined-text-frame>", scm, ctx);

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 6));

    return unique_ptr<id3v2_3_frame>(new TXXX(text, encoding::UTF_8,
                                              use_unicode::yes,
                                              tap, fap, ro, boost::none,
                                              boost::none, dsc));

  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_udt_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) =
      ser_frame_2_4("<user-defined-text-frame>", scm, ctx);

    string dsc = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    string text = ctx.free_utf8_string(SCM_SLOT(scm, 6));

    return unique_ptr<id3v2_4_frame>(
      new TXXX_2_4(text, encoding::UTF_8,
                   use_unicode::yes,
                   tap, fap, ro, boost::none,
                   boost::none, false, unsync,
                   dsc));

  }

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_cnt_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;
    ser_frame_2_2("<play-count-frame>", scm, ctx);

    size_t count = scm_to_size_t(SCM_SLOT(scm, 5));

    return unique_ptr<id3v2_2_frame>(new CNT(count));

  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_cnt_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<play-count-frame>", scm, ctx);

    size_t count = scm_to_size_t(SCM_SLOT(scm, 5));

    return unique_ptr<id3v2_3_frame>(new PCNT(count, tap, fap, ro, boost::none,
                                              boost::none, boost::none));

  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_cnt_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) =
      ser_frame_2_4("<play-count-frame>", scm, ctx);

    size_t count = scm_to_size_t(SCM_SLOT(scm, 5));

    return unique_ptr<id3v2_4_frame>(
      new PCNT_2_4(count, tap, fap, ro, boost::none,
                   boost::none, false, unsync, boost::none));

  }

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_pop_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;
    ser_frame_2_2("<popm-frame>", scm, ctx);

    string email = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    unsigned char rating = scm_to_uchar(SCM_SLOT(scm, 6));
    size_t count = scm_to_size_t(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_2_frame>(new POP(email, rating, count));
  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_pop_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<popm-frame>", scm, ctx);

    string email = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    unsigned char rating = scm_to_uchar(SCM_SLOT(scm, 6));
    size_t count = scm_to_size_t(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_3_frame>(new POPM(email, rating, count,
                                              tap, fap, ro, boost::none,
                                              boost::none, boost::none));
  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_pop_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) = ser_frame_2_4("<popm-frame>", scm, ctx);

    string email = ctx.free_utf8_string(SCM_SLOT(scm, 5));
    unsigned char rating = scm_to_uchar(SCM_SLOT(scm, 6));
    size_t count = scm_to_size_t(SCM_SLOT(scm, 7));

    return unique_ptr<id3v2_4_frame>(new POPM_2_4(email, rating, count,
                                                  tap, fap, ro, boost::none,
                                                  boost::none, false, unsync,
                                                  boost::none));
  }

  std::unique_ptr<scribbu::id3v2_2_frame>
  ser_xtag_2_2(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;
    ser_frame_2_2("<tag-cloud-frame>", scm, ctx);

    string own = ctx.free_utf8_string(SCM_SLOT(scm, 5));

    map<string, set<string>> M;
    SCM scm_tags = SCM_SLOT(scm, 6);

    for (int i = 0, n = scm_to_int(scm_length(scm_tags)); i < n; ++i) {

      SCM scm = scm_list_ref(scm_tags, scm_from_int(i));

      string key = ctx.free_utf8_string(scm_list_ref(scm, 0));

      set<string> vals;
      SCM scm_vals = scm_list_ref(scm, scm_from_int(1));
      for (int j = 0, m = scm_to_int(scm_length(scm_vals)); j < m; ++j) {
        SCM scm_val = scm_list_ref(scm_vals, scm_from_int(j));
        string val = ctx.free_utf8_string(scm_val);
        vals.insert(val);
      }

      M[key] = vals;
    }

    return unique_ptr<id3v2_2_frame>(new XTG(own, M.begin(), M.end()));
  }

  std::unique_ptr<scribbu::id3v2_3_frame>
  ser_xtag_2_3(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;

    tie(id_txt, tap, fap, ro) = ser_frame_2_3("<tag-cloud-frame>", scm, ctx);

    string own = ctx.free_utf8_string(SCM_SLOT(scm, 5));

    map<string, set<string>> M;
    SCM scm_tags = SCM_SLOT(scm, 6);

    for (int i = 0, n = scm_to_int(scm_length(scm_tags)); i < n; ++i) {

      SCM pair   = scm_list_ref(scm_tags, scm_from_int(i));
      SCM first  = scm_list_ref(pair, scm_from_int(0));
      SCM second = scm_list_tail(pair, scm_from_int(1));

      string key = ctx.free_utf8_string(first);


      SCM test = scm_string_p(second);
      bool ftest = SCM_BOOL_T == test;
      test = scm_list_p(second);
      ftest = SCM_BOOL_T == test;

      SCM scm_cls = scm_class_of(second);
      test = scm_variable_p(scm_cls);
      ftest = (SCM_BOOL_T == test);


      // Could be a string, or a list of strings
      set<string> vals;

      if (scm_string_p(second) == SCM_BOOL_T) {

        vals.insert(ctx.free_utf8_string(second));

      } else if (scm_list_p(second) == SCM_BOOL_T) {

        SCM scm_len = scm_length(second);
        int m = scm_to_int(scm_len);

        for (int j = 0; j < m; ++j) {
          SCM scm_val = scm_list_ref(second, scm_from_int(j));
          string val = ctx.free_utf8_string(scm_val);
          vals.insert(val);
        }

      } else {
        throw std::runtime_error("unknown type");
      }

      M[key] = vals;
    }

    return unique_ptr<id3v2_3_frame>(new XTAG(own, M.begin(), M.end(),
                                              tap, fap, ro, boost::none,
                                              boost::none, boost::none));
  }

  std::unique_ptr<scribbu::id3v2_4_frame>
  ser_xtag_2_4(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    typedef id3v2_3_plus_frame::tag_alter_preservation
      tag_alter_preservation;
    typedef id3v2_3_plus_frame::file_alter_preservation
      file_alter_preservation;
    typedef id3v2_3_plus_frame::read_only read_only;

    dynwind_context ctx;

    string id_txt;
    tag_alter_preservation tap;
    file_alter_preservation fap;
    read_only ro;
    bool unsync;

    tie(id_txt, tap, fap, ro, unsync) =
      ser_frame_2_4("<tag-cloud-frame>", scm, ctx);

    string own = ctx.free_utf8_string(SCM_SLOT(scm, 5));

    map<string, set<string>> M;
    SCM scm_tags = SCM_SLOT(scm, 6);

    for (int i = 0, n = scm_to_int(scm_length(scm_tags)); i < n; ++i) {

      SCM scm = scm_list_ref(scm_tags, scm_from_int(i));

      string key = ctx.free_utf8_string(scm_list_ref(scm, 0));

      set<string> vals;
      SCM scm_vals = scm_list_ref(scm, scm_from_int(1));
      for (int j = 0, m = scm_to_int(scm_length(scm_vals)); j < m; ++j) {
        SCM scm_val = scm_list_ref(scm_vals, scm_from_int(j));
        string val = ctx.free_utf8_string(scm_val);
        vals.insert(val);
      }

      M[key] = vals;
    }

    return unique_ptr<id3v2_4_frame>(new XTAG_2_4(own, M.begin(), M.end(),
                                                  tap, fap, ro, boost::none,
                                                  boost::none, false, unsync,
                                                  boost::none));
  }

}

////////////////////////////////////////////////////////////////////////////////
//                     scheme_serde_dispatcher implementation                 //
////////////////////////////////////////////////////////////////////////////////

scribbu::scheme_serde_dispatcher::scheme_serde_dispatcher()
{
# define REG_DE(ver, frame, pfn, id, n)                         \
  reg_de_##ver<scribbu::frame, pfn>(scribbu::frame_id ## n(#id))

# define REG_SER(ver, pfn, id) \
  reg_ser_##ver(#id, pfn)

  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TAL, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TBP, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TCM, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TCO, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TCR, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TDA, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TDY, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TEN, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TFT, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TIM, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TKE, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TLA, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TLE, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TMT, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TOA, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TOF, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TOL, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TOR, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TOT, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TP1, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TP2, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TP3, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TP4, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TPA, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TPB, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TRC, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TRD, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TRK, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TSI, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TSS, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TT1, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TT2, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TT3, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TXT, 3);
  REG_DE(2_2, id3v2_2_text_frame, de_text_frame_2_2, TYE, 3);
  REG_DE(2_2, COM,                de_comment_2_2,    COM, 3);
  REG_DE(2_2, TXX,                de_udt_2_2,        TXX, 3);
  REG_DE(2_2, CNT,                de_cnt_2_2,        CNT, 3);
  REG_DE(2_2, POP,                de_popm_2_2,       POP, 3);
  REG_DE(2_2, XTG,                de_xtag_2_2,       XTG, 3);

  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TALB, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TBPM, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TCOM, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TCON, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TCOP, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TDAT, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TDLY, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TENC, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TEXT, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TFLT, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TIME, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TIT1, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TIT2, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TIT3, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TKEY, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TLAN, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TLEN, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TMED, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TOAL, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TOFN, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TOLY, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TOPE, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TORY, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TOWN, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPE1, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPE2, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPE3, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPE4, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPOS, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TPUB, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TRCK, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TRDA, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TRSN, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TRSO, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TSIZ, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TSRC, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TSSE, 4);
  REG_DE(2_3, id3v2_3_text_frame, de_text_frame_2_3, TYER, 4);
  REG_DE(2_3, COMM,               de_comment_2_3,    COMM, 4);
  REG_DE(2_3, TXXX,               de_udt_2_3,        TXXX, 4);
  REG_DE(2_3, PCNT,               de_cnt_2_3,        PCNT, 4);
  REG_DE(2_3, POPM,               de_popm_2_3,       POPM, 4);
  REG_DE(2_3, XTAG,               de_xtag_2_3,       XTAG, 4);

  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TALB, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TBPM, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TCOM, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TCON, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TCOP, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TDAT, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TDLY, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TENC, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TEXT, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TFLT, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TIME, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TIT1, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TIT2, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TIT3, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TKEY, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TLAN, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TLEN, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TMED, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TOAL, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TOFN, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TOLY, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TOPE, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TORY, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TOWN, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPE1, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPE2, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPE3, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPE4, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPOS, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TPUB, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TRCK, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TRDA, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TRSN, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TRSO, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TSIZ, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TSRC, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TSSE, 4);
  REG_DE(2_4, id3v2_4_text_frame, de_text_frame_2_4, TYER, 4);
  REG_DE(2_4, COMM_2_4,           de_comment_2_4,    COMM, 4);
  REG_DE(2_4, TXXX_2_4,           de_udt_2_4,        TXXX, 4);
  REG_DE(2_4, PCNT_2_4,           de_cnt_2_4,        PCNT, 4);
  REG_DE(2_4, POPM_2_4,           de_popm_2_4,       POPM, 4);
  REG_DE(2_4, XTAG_2_4,           de_xtag_2_4,       XTAG, 4);

  REG_SER(2_2, ser_text_frame_2_2, album-frame                 );
  REG_SER(2_2, ser_text_frame_2_2, artist-frame                );
  REG_SER(2_2, ser_text_frame_2_2, band-frame                  );
  REG_SER(2_2, ser_text_frame_2_2, bpm-frame                   );
  REG_SER(2_2, ser_text_frame_2_2, composer-frame              );
  REG_SER(2_2, ser_text_frame_2_2, conductor-frame             );
  REG_SER(2_2, ser_text_frame_2_2, content-group-frame         );
  REG_SER(2_2, ser_text_frame_2_2, copyright-frame             );
  REG_SER(2_2, ser_text_frame_2_2, date-frame                  );
  REG_SER(2_2, ser_text_frame_2_2, encoded-by-frame            );
  REG_SER(2_2, ser_text_frame_2_2, file-type-frame             );
  REG_SER(2_2, ser_text_frame_2_2, genre-frame                 );
  REG_SER(2_2, ser_text_frame_2_2, initial-key-frame           );
  REG_SER(2_2, ser_text_frame_2_2, interpreted-by-frame        );
  REG_SER(2_2, ser_text_frame_2_2, isrc-frame                  );
  REG_SER(2_2, ser_text_frame_2_2, langs-frame                 );
  REG_SER(2_2, ser_text_frame_2_2, length-frame                );
  REG_SER(2_2, ser_text_frame_2_2, lyricist-frame              );
  REG_SER(2_2, ser_text_frame_2_2, media-type-frame            );
  REG_SER(2_2, ser_text_frame_2_2, original-album-frame        );
  REG_SER(2_2, ser_text_frame_2_2, original-artist-frame       );
  REG_SER(2_2, ser_text_frame_2_2, original-filename-frame     );
  REG_SER(2_2, ser_text_frame_2_2, original-lyricist-frame     );
  REG_SER(2_2, ser_text_frame_2_2, original-release-year-frame );
  REG_SER(2_2, ser_text_frame_2_2, part-of-a-set-frame         );
  REG_SER(2_2, ser_text_frame_2_2, playlist-delay-frame        );
  REG_SER(2_2, ser_text_frame_2_2, publisher-frame             );
  REG_SER(2_2, ser_text_frame_2_2, recording-dates-frame       );
  REG_SER(2_2, ser_text_frame_2_2, settings-frame              );
  REG_SER(2_2, ser_text_frame_2_2, size-frame                  );
  REG_SER(2_2, ser_text_frame_2_2, subtitle-frame              );
  REG_SER(2_2, ser_text_frame_2_2, time-frame                  );
  REG_SER(2_2, ser_text_frame_2_2, title-frame                 );
  REG_SER(2_2, ser_text_frame_2_2, track-frame                 );
  REG_SER(2_2, ser_text_frame_2_2, year-frame                  );
  REG_SER(2_2, ser_comment_2_2,    comment-frame               );
  REG_SER(2_2, ser_udt_2_2,        udt-frame                   );
  REG_SER(2_2, ser_cnt_2_2,        play-count-frame            );
  REG_SER(2_2, ser_pop_2_2,        popm-frame                  );
  REG_SER(2_2, ser_xtag_2_2,       tag-cloud-frame             );

  REG_SER(2_3, ser_text_frame_2_3, album-frame                 );
  REG_SER(2_3, ser_text_frame_2_3, artist-frame                );
  REG_SER(2_3, ser_text_frame_2_3, band-frame                  );
  REG_SER(2_3, ser_text_frame_2_3, bpm-frame                   );
  REG_SER(2_3, ser_text_frame_2_3, composer-frame              );
  REG_SER(2_3, ser_text_frame_2_3, conductor-frame             );
  REG_SER(2_3, ser_text_frame_2_3, content-group-frame         );
  REG_SER(2_3, ser_text_frame_2_3, copyright-frame             );
  REG_SER(2_3, ser_text_frame_2_3, date-frame                  );
  REG_SER(2_3, ser_text_frame_2_3, encoded-by-frame            );
  REG_SER(2_3, ser_text_frame_2_3, file-owner-frame            );
  REG_SER(2_3, ser_text_frame_2_3, file-type-frame             );
  REG_SER(2_3, ser_text_frame_2_3, genre-frame                 );
  REG_SER(2_3, ser_text_frame_2_3, initial-key-frame           );
  REG_SER(2_3, ser_text_frame_2_3, interpreted-by-frame        );
  REG_SER(2_3, ser_text_frame_2_3, isrc-frame                  );
  REG_SER(2_3, ser_text_frame_2_3, langs-frame                 );
  REG_SER(2_3, ser_text_frame_2_3, length-frame                );
  REG_SER(2_3, ser_text_frame_2_3, lyricist-frame              );
  REG_SER(2_3, ser_text_frame_2_3, media-type-frame            );
  REG_SER(2_3, ser_text_frame_2_3, original-album-frame        );
  REG_SER(2_3, ser_text_frame_2_3, original-artist-frame       );
  REG_SER(2_3, ser_text_frame_2_3, original-filename-frame     );
  REG_SER(2_3, ser_text_frame_2_3, original-lyricist-frame     );
  REG_SER(2_3, ser_text_frame_2_3, original-release-year-frame );
  REG_SER(2_3, ser_text_frame_2_3, part-of-a-set-frame         );
  REG_SER(2_3, ser_text_frame_2_3, playlist-delay-frame        );
  REG_SER(2_3, ser_text_frame_2_3, publisher-frame             );
  REG_SER(2_3, ser_text_frame_2_3, recording-dates-frame       );
  REG_SER(2_3, ser_text_frame_2_3, settings-frame              );
  REG_SER(2_3, ser_text_frame_2_3, size-frame                  );
  REG_SER(2_3, ser_text_frame_2_3, station-name-frame          );
  REG_SER(2_3, ser_text_frame_2_3, station-owner-frame         );
  REG_SER(2_3, ser_text_frame_2_3, subtitle-frame              );
  REG_SER(2_3, ser_text_frame_2_3, time-frame                  );
  REG_SER(2_3, ser_text_frame_2_3, title-frame                 );
  REG_SER(2_3, ser_text_frame_2_3, track-frame                 );
  REG_SER(2_3, ser_text_frame_2_3, year-frame                  );
  REG_SER(2_3, ser_comment_2_3,    comment-frame               );
  REG_SER(2_3, ser_udt_2_3,        udt-frame                   );
  REG_SER(2_3, ser_cnt_2_3,        play-count-frame            );
  REG_SER(2_3, ser_pop_2_3,        popm-frame                  );
  REG_SER(2_3, ser_xtag_2_3,       tag-cloud-frame             );

  REG_SER(2_4, ser_text_frame_2_4, album-frame                 );
  REG_SER(2_4, ser_text_frame_2_4, artist-frame                );
  REG_SER(2_4, ser_text_frame_2_4, band-frame                  );
  REG_SER(2_4, ser_text_frame_2_4, bpm-frame                   );
  REG_SER(2_4, ser_text_frame_2_4, composer-frame              );
  REG_SER(2_4, ser_text_frame_2_4, conductor-frame             );
  REG_SER(2_4, ser_text_frame_2_4, content-group-frame         );
  REG_SER(2_4, ser_text_frame_2_4, copyright-frame             );
  REG_SER(2_4, ser_text_frame_2_4, date-frame                  );
  REG_SER(2_4, ser_text_frame_2_4, encoded-by-frame            );
  REG_SER(2_4, ser_text_frame_2_4, file-owner-frame            );
  REG_SER(2_4, ser_text_frame_2_4, file-type-frame             );
  REG_SER(2_4, ser_text_frame_2_4, genre-frame                 );
  REG_SER(2_4, ser_text_frame_2_4, initial-key-frame           );
  REG_SER(2_4, ser_text_frame_2_4, interpreted-by-frame        );
  REG_SER(2_4, ser_text_frame_2_4, isrc-frame                  );
  REG_SER(2_4, ser_text_frame_2_4, langs-frame                 );
  REG_SER(2_4, ser_text_frame_2_4, length-frame                );
  REG_SER(2_4, ser_text_frame_2_4, lyricist-frame              );
  REG_SER(2_4, ser_text_frame_2_4, media-type-frame            );
  REG_SER(2_4, ser_text_frame_2_4, original-album-frame        );
  REG_SER(2_4, ser_text_frame_2_4, original-artist-frame       );
  REG_SER(2_4, ser_text_frame_2_4, original-filename-frame     );
  REG_SER(2_4, ser_text_frame_2_4, original-lyricist-frame     );
  REG_SER(2_4, ser_text_frame_2_4, original-release-year-frame );
  REG_SER(2_4, ser_text_frame_2_4, part-of-a-set-frame         );
  REG_SER(2_4, ser_text_frame_2_4, playlist-delay-frame        );
  REG_SER(2_4, ser_text_frame_2_4, publisher-frame             );
  REG_SER(2_4, ser_text_frame_2_4, recording-dates-frame       );
  REG_SER(2_4, ser_text_frame_2_4, settings-frame              );
  REG_SER(2_4, ser_text_frame_2_4, size-frame                  );
  REG_SER(2_4, ser_text_frame_2_4, station-name-frame          );
  REG_SER(2_4, ser_text_frame_2_4, station-owner-frame         );
  REG_SER(2_4, ser_text_frame_2_4, subtitle-frame              );
  REG_SER(2_4, ser_text_frame_2_4, time-frame                  );
  REG_SER(2_4, ser_text_frame_2_4, title-frame                 );
  REG_SER(2_4, ser_text_frame_2_4, track-frame                 );
  REG_SER(2_4, ser_text_frame_2_4, year-frame                  );
  REG_SER(2_4, ser_comment_2_4,    comment-frame               );
  REG_SER(2_4, ser_udt_2_4,        udt-frame                   );
  REG_SER(2_4, ser_cnt_2_4,        play-count-frame            );
  REG_SER(2_4, ser_pop_2_4,        popm-frame                  );
  REG_SER(2_4, ser_xtag_2_4,       tag-cloud-frame             );

# undef REG_SER
# undef REG_DE
}

/// Deserialize an id3v2_2_frame to an <id3v2-frame>
SCM
scribbu::scheme_serde_dispatcher::de_2_2(const scribbu::id3v2_2_frame& frm,
                                         bool unsync) const
{
  SCM out;
  auto p = de_2_2_.find(frm.id());
  if (de_2_2_.end() == p) {
    out = de_unknown_frame_2_2(frm, unsync);
  } else {
    try {
      out = (*p->second)(frm, unsync);
    } catch (const std::exception&) {
      out = de_unknown_frame_2_2(frm, unsync);
    }
  }
  return out;
}

/// Deserialize an id3v2_3_frame to an <id3v2-frame>
SCM
scribbu::scheme_serde_dispatcher::de_2_3(const scribbu::id3v2_3_frame& frm,
                                bool unsync) const
{
  SCM out;
  auto p = de_2_3_.find(frm.id());
  if (de_2_3_.end() == p) {
    out = de_unknown_frame_2_3(frm, unsync);
  } else {
    try {
      out = (*p->second)(frm, unsync);
    } catch (const std::exception&) {
      out = de_unknown_frame_2_3(frm, unsync);
    }
  }
  return out;
}

/// Deserialize an id3v2_34_frame to an <id3v2-frame>
SCM
scribbu::scheme_serde_dispatcher::de_2_4(const scribbu::id3v2_4_frame& frm,
                                bool unsync) const
{
  SCM out;
  auto p = de_2_4_.find(frm.id());
  if (de_2_4_.end() == p) {
    out = de_unknown_frame_2_4(frm, unsync);
  } else {
    try {
      out = (*p->second)(frm, unsync);
    } catch (const std::exception&) {
      out = de_unknown_frame_2_4(frm, unsync);
    }
  }
  return out;
}

/// Serialize \a scm to an id3v2_2_frame
std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::scheme_serde_dispatcher::ser_2_2(SCM scm) const
{
  using namespace std;

  if (!SCM_IS_A_P(scm, scm_c_public_ref("scribbu", "<id3v2-frame>"))) {
    scm_error(sym_for_utf8("unexpected-type"), "ser_2_2",
              "expected <id3v2-frame>, got ~A", scm, SCM_BOOL_F);
  }

  dynwind_context ctx;

  string id(ctx.free_utf8_string(scm_symbol_to_string(SCM_SLOT(scm, 0))));
  auto p = ser_2_2_.find(id);
  std::unique_ptr<scribbu::id3v2_2_frame> pout;

  if (ser_2_2_.end() == p) {
    pout = move(ser_unknown_frame_2_2(scm));
  } else {
    scribbu::scheme_serde_dispatcher::pfn_from_scm_2_2 pfn = p->second;
    try {
      pout = pfn(scm);
    } catch(const std::exception&) {
      pout = ser_unknown_frame_2_2(scm);
    }
  }
  return pout;
}

/// Serialize \a scm to an id3v2_3_frame
std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::scheme_serde_dispatcher::ser_2_3(SCM scm) const
{
  using namespace std;

  if (!SCM_IS_A_P(scm, scm_c_public_ref("scribbu", "<id3v2-frame>"))) {
    scm_error(sym_for_utf8("unexpected-type"), "ser_2_2",
              "expected <id3v2-frame>, got ~A", scm, SCM_BOOL_F);
  }

  dynwind_context ctx;

  string id(ctx.free_utf8_string(scm_symbol_to_string(SCM_SLOT(scm, 0))));
  auto p = ser_2_3_.find(id);
  std::unique_ptr<scribbu::id3v2_3_frame> pout;
  if (ser_2_3_.end() == p) {
    pout = move(ser_unknown_frame_2_3(scm));
  } else {
    scheme_serde_dispatcher::pfn_from_scm_2_3 pfn = p->second;
    try {
      pout = pfn(scm);
    } catch (const std::exception&) {
      pout = ser_unknown_frame_2_3(scm);
    }
  }
  return pout;
}

/// Serialize \a scm to an id3v2_4_frame
std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::scheme_serde_dispatcher::ser_2_4(SCM scm) const
{
  using namespace std;

  if (!SCM_IS_A_P(scm, scm_c_public_ref("scribbu", "<id3v2-frame>"))) {
    scm_error(sym_for_utf8("unexpected-type"), "ser_2_2",
              "expected <id3v2-frame>, got ~A", scm, SCM_BOOL_F);
  }

  dynwind_context ctx;

  string id(ctx.free_utf8_string(scm_symbol_to_string(SCM_SLOT(scm, 0))));
  auto p = ser_2_4_.find(id);
  std::unique_ptr<scribbu::id3v2_4_frame> pout;

  if (ser_2_4_.end() == p) {
    pout = move(ser_unknown_frame_2_4(scm));
  } else {
    scheme_serde_dispatcher::pfn_from_scm_2_4 pfn = p->second;
    try {
      pout = pfn(scm);
    } catch (const std::exception&) {
      pout = ser_unknown_frame_2_4(scm);
    }
  }
  return pout;
}

SCM
scribbu::scheme_serde_dispatcher::de_unknown_frame_2_2(const scribbu::id3v2_2_frame &f,
                                              bool unsync) const
{
  SCM cls = scm_c_public_ref("scribbu", "<unk-frame>");

  SCM args = scm_list_1(cls);
  SCM x = scm_make(args);

  SCM_SET_SLOT(x, 0, sym_unknown_frame);
  SCM_SET_SLOT(x, 5, scm_from_locale_string(f.id().as_string().c_str()));

  std::stringstream stm;
  f.write(stm, unsync);
  std::string s = stm.str();

  SCM bv = scm_c_make_bytevector(s.length());
  memcpy(SCM_BYTEVECTOR_CONTENTS(bv), s.data(), s.length());

  SCM_SET_SLOT(x, 6, bv);

  return x;
}

SCM
scribbu::scheme_serde_dispatcher::de_unknown_frame_2_3(const scribbu::id3v2_3_frame &f,
                                              bool unsync) const
{
  typedef scribbu::id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef scribbu::id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef scribbu::id3v2_3_plus_frame::read_only read_only;

  SCM cls = scm_c_public_ref("scribbu", "<unk-frame>");

  SCM args = scm_list_1(cls);
  SCM x = scm_make(args);

  SCM_SET_SLOT(x, 0, sym_unknown_frame);
  SCM_SET_SLOT(x, 1, f.tag_alter_preserve() ==
               tag_alter_preservation::preserve ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 2, f.file_alter_preserve() ==
               file_alter_preservation::preserve ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 3, f.readonly() == read_only::set ?
               SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 5, scm_from_locale_string(f.id().as_string().c_str()));

  std::stringstream stm;
  f.write(stm, unsync);
  std::string s = stm.str();

  SCM bv = scm_c_make_bytevector(s.length());
  memcpy(SCM_BYTEVECTOR_CONTENTS(bv), s.data(), s.length());

  SCM_SET_SLOT(x, 6, bv);

  return x;
}

SCM
scribbu::scheme_serde_dispatcher::de_unknown_frame_2_4(const scribbu::id3v2_4_frame &f,
                                              bool unsync) const
{
  typedef scribbu::id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef scribbu::id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef scribbu::id3v2_3_plus_frame::read_only read_only;

  SCM cls = scm_c_public_ref("scribbu", "<unk-frame>");

  SCM args = scm_list_1(cls);
  SCM x = scm_make(args);

  SCM_SET_SLOT(x, 0, sym_unknown_frame);
  SCM_SET_SLOT(x, 1, f.tag_alter_preserve() ==
               tag_alter_preservation::preserve ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 2, f.file_alter_preserve() ==
               file_alter_preservation::preserve ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 3, f.readonly() == read_only::set ?
               SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 4, f.unsynchronised() ? SCM_BOOL_T : SCM_BOOL_F);
  SCM_SET_SLOT(x, 5, scm_from_locale_string(f.id().as_string().c_str()));

  std::stringstream stm;
  f.write(stm, unsync);
  std::string s = stm.str();

  SCM bv = scm_c_make_bytevector(s.length());
  memcpy(SCM_BYTEVECTOR_CONTENTS(bv), s.data(), s.length());

  SCM_SET_SLOT(x, 6, bv);

  return x;
}

std::unique_ptr<scribbu::id3v2_2_frame>
scribbu::scheme_serde_dispatcher::ser_unknown_frame_2_2(SCM scm) const
{
  using namespace std;
  using namespace scribbu;

  dynwind_context ctx;

  frame_id3 id(ctx.free_locale_string(SCM_SLOT(scm, 5)));

  SCM scm_bv = SCM_SLOT(scm, 6);

  size_t cb = SCM_BYTEVECTOR_LENGTH(scm_bv);
  signed char *p = SCM_BYTEVECTOR_CONTENTS(scm_bv);

  // The unknown_id3v2_2_frame ctor assumes we've stripped off the
  // frame header-- that's six bytes in v2.2.
  return unique_ptr<id3v2_2_frame>(new unknown_id3v2_2_frame(id, p + 6, p + cb));

}

std::unique_ptr<scribbu::id3v2_3_frame>
scribbu::scheme_serde_dispatcher::ser_unknown_frame_2_3(SCM scm) const
{
  using namespace std;
  using namespace scribbu;

  typedef id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef id3v2_3_plus_frame::read_only read_only;

  dynwind_context ctx;

  frame_id4 id(ctx.free_locale_string(SCM_SLOT(scm, 5)));

  tag_alter_preservation tap = tag_alter_preservation::preserve;
  if (SCM_BOOL_F == SCM_SLOT(scm, 1)) {
    tap = tag_alter_preservation::discard;
  }
  file_alter_preservation fap = file_alter_preservation::preserve;
  if (SCM_BOOL_F == SCM_SLOT(scm, 2)) {
    fap = file_alter_preservation::discard;
  }
  read_only ro = read_only::clear;
  if (SCM_BOOL_T == SCM_SLOT(scm, 3)) {
    ro = read_only::set;
  }

  SCM scm_bv = SCM_SLOT(scm, 6);

  size_t cb = SCM_BYTEVECTOR_LENGTH(scm_bv);
  signed char *p = SCM_BYTEVECTOR_CONTENTS(scm_bv);

  return unique_ptr<id3v2_3_frame>(
    // The unknown_id3v2_3_frame ctor assumes we've stripped off the
    // frame header-- that's ten bytes in v2.3.
    new unknown_id3v2_3_frame(id, tap, fap, ro,
                              boost::none, boost::none, boost::none,
                              p + 10, p + cb));

}

std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::scheme_serde_dispatcher::ser_unknown_frame_2_4(SCM scm) const
{
  using namespace std;
  using namespace scribbu;

  typedef id3v2_3_plus_frame::tag_alter_preservation
    tag_alter_preservation;
  typedef id3v2_3_plus_frame::file_alter_preservation
    file_alter_preservation;
  typedef id3v2_3_plus_frame::read_only read_only;
  typedef id3v2_3_plus_frame::read_only read_only;

  dynwind_context ctx;

  frame_id4 id(ctx.free_locale_string(SCM_SLOT(scm, 5)));

  tag_alter_preservation tap = tag_alter_preservation::preserve;
  if (SCM_BOOL_F == SCM_SLOT(scm, 1)) {
    tap = tag_alter_preservation::discard;
  }
  file_alter_preservation fap = file_alter_preservation::preserve;
  if (SCM_BOOL_F == SCM_SLOT(scm, 2)) {
    fap = file_alter_preservation::discard;
  }
  read_only ro = read_only::clear;
  if (SCM_BOOL_T == SCM_SLOT(scm, 3)) {
    ro = read_only::set;
  }
  bool unsync = false;
  if (SCM_BOOL_T == SCM_SLOT(scm, 3)) {
    unsync = true;
  }

  SCM scm_bv = SCM_SLOT(scm, 6);

  size_t cb = SCM_BYTEVECTOR_LENGTH(scm_bv);
  signed char *p = SCM_BYTEVECTOR_CONTENTS(scm_bv);

  return unique_ptr<id3v2_4_frame>(
    // The unknown_id3v2_4_frame ctor assumes we've stripped off the
    // frame header-- that's ten bytes in v2.4.
    new unknown_id3v2_4_frame(id, tap, fap, ro,
                              boost::none, boost::none,
                              false, unsync, boost::none,
                              p + 10, p + cb));
}
