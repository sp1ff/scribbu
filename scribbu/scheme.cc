/**
 * \file scheme.cc
 *
 * Copyright (C) 2015-2021 Michael Herstine <sp1ff@pobox.com>
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

#include "scheme.hh"
#include "config.h"
#include "scribbu.hh"
#include "id3v1.hh"
#include "id3v22.hh"
#include "id3v23.hh"
#include "id3v24.hh"
#include "id3v2-utils.hh"
#include "dynwind-context.hh"
#include "scheme-serde.hh"
#include "tagset.hh"

#include <string>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/functional/hash.hpp>

namespace fs = boost::filesystem;

using scribbu::dynwind_context;
using scribbu::scheme_serde_dispatcher;

///////////////////////////////////////////////////////////////////////////////
//                              utility code                                 //
///////////////////////////////////////////////////////////////////////////////

#define AUTHOR              "Michael Herstine"
#define COPYRIGHT       "2017-2021"
#define READ_ID3V1_TAG  "read-id3v1-tag"
#define READ_TAGSET     "read-tagset"
#define WITH_TRACK_IN   "with-track-in"
#define WRITE_ID3V1_TAG "write-id3v1-tag"
#define WRITE_TAGSET    "write-tagset"

namespace {

  /**
   * \brief Create a GOOPS <id3v2-tag> instance from a C++ id3v2_tag
   *
   *
   * \param p [in] id3v2_tag to be converted to a GOOPS instance
   *
   * \return a three-tuple: (<id3v2-tag>, version (2,3,4), unsync'd (#t,#f)
   *
   *
   */

  SCM scm_from_id3v2_tag(const scribbu::id3v2_tag *p)
  {
    using namespace std;
    using namespace scribbu;

    SCM cls = scm_c_public_ref("scribbu", "<id3v2-tag>");
    SCM args = scm_list_1(cls);
    SCM x = scm_make(args);

    // OK-- we now have a crisp, new <id3v2-tag> instances. Now we just need to
    // fill in the slots. Let's start with the easy ones.
    int version = (int)(p->version());
    SCM_SET_SLOT(x, 2, scm_from_uintmax(p->padding()));

    bool unsync = false;
    if (p->unsynchronised()) {
      unsync = *(p->unsynchronised());
    }

    // Now for frames-- I'm going to do this in steps. First: we need to cast
    // `p' to a concrete type in order to get at the frames:
    vector<SCM> frames;
    scheme_serde_dispatcher D;
    if (2 == version) {

      const id3v2_2_tag *ptag = dynamic_cast<const id3v2_2_tag*>(p);
      if (nullptr == ptag) {
        scm_error(sym_for_utf8("wrong-tag-version"), "scm_from_id3v2_tag",
                  "internal tag advertised version ~a, but dynamic cast failed",
                  scm_from_int(version), SCM_BOOL_F);
      }

      transform(ptag->begin(), ptag->end(), back_inserter(frames),
                [&](const id3v2_2_frame &f) {
                  return D.de_2_2(f, unsync);
                });


    } else if (3 == version) {

      const id3v2_3_tag *ptag = dynamic_cast<const id3v2_3_tag*>(p);
      if (nullptr == ptag) {
        scm_error(sym_for_utf8("wrong-tag-version"), "scm_from_id3v2_tag",
                  "internal tag advertised version ~a, but dynamic cast failed",
                  scm_from_int(version), SCM_BOOL_F);
      }

      SCM_SET_SLOT(x, 0, scm_from_bool(ptag->experimental() ? 1 : 0));
      transform(ptag->begin(), ptag->end(), back_inserter(frames),
                [&](const id3v2_3_frame &f) {
                  return D.de_2_3(f, unsync);
                });


    } else if (4 == version) {

      const id3v2_4_tag *ptag = dynamic_cast<const id3v2_4_tag*>(p);
      if (nullptr == ptag) {
        scm_error(sym_for_utf8("wrong-tag-version"), "scm_from_id3v2_tag",
                  "internal tag advertised version ~a, but dynamic cast failed",
                  scm_from_int(version), SCM_BOOL_F);
      }

      SCM_SET_SLOT(x, 0, scm_from_bool(ptag->experimental() ? 1 : 0));

      transform(ptag->begin(), ptag->end(), back_inserter(frames),
                [&](const id3v2_4_frame &f) {
                  return D.de_2_4(f, unsync);
                });


    } else {
      scm_error(sym_for_utf8("bad-tag-version"), "scm_from_id3v2_tag",
                "internal tag advertised version ~a, about which we do not know",
                scm_from_int(version), SCM_BOOL_F);

    }

    SCM_SET_SLOT(x, 1, scm_list_for_range(frames.begin(), frames.end()));
    SCM_SET_SLOT(x, 2, scm_from_uint(p->padding()));

    return scm_list_3(x, scm_from_int(version), scm_from_bool(unsync));

  } // End free function scm_from_id3v2_tag.

  /**
   * \brief produce an id3v2_tag from a GOOPS <id3v2-tag> instance
   *
   *
   * \param scm [in] a two-tuple whose first element is a GOOPS <id3v2-tag>
   * instance and whose second is an int denoting the desired ID3v2 version
   * (i.e. 2, 3, or 4)
   *
   * \return a pointer to an id3v2_2_tag, id3v2_3_tag, or id3v2_4_tag
   *
   *
   */

  std::unique_ptr<scribbu::id3v2_tag>
  scm_to_id3v2_tag(SCM scm)
  {
    using namespace std;
    using namespace scribbu;

    scheme_serde_dispatcher D;

    SCM scm_tag = scm_list_ref(scm, scm_from_int(0));
    SCM scm_ver = scm_list_ref(scm, scm_from_int(1));

    if (!SCM_IS_A_P(scm_tag, scm_c_public_ref("scribbu", "<id3v2-tag>"))) {
      scm_error(sym_for_utf8("unexpected-type"), "scm_to_id3v2_tag",
                             "expected <id3v2-tag>, got ~A", scm,
                             SCM_BOOL_F);
    }

    int version = scm_to_int(scm_ver);

    bool fexp = false;
    SCM scm_exp = SCM_SLOT(scm_tag, 0);
    if (SCM_BOOL_F == scm_null_p(scm_exp)) {
      fexp = scm_to_bool(scm_exp);
    }
    size_t cbpad = 0;
    SCM scm_pad = SCM_SLOT(scm_tag, 2);
    if (SCM_ELISP_NIL != scm_null_p(scm_pad)) {
      cbpad = scm_to_uint(scm_pad);
    }

    unique_ptr<id3v2_tag> pout;

    if (2 == version) {

      unique_ptr<id3v2_2_tag> p(new id3v2_2_tag(cbpad, fexp));

      SCM scm_frames = SCM_SLOT(scm_tag, 1);
      int nframes = scm_to_int(scm_length(scm_frames));
      for (int i = 0; i < nframes; ++i) {
        p->push_back(*D.ser_2_2(scm_list_ref(scm_frames, scm_from_int(i))));
      }

      pout = move(p);

    } else if (3 == version) {

      unique_ptr<id3v2_3_tag> p(new id3v2_3_tag(cbpad, fexp));

      SCM scm_frames = SCM_SLOT(scm_tag, 1);
      int nframes = scm_to_int(scm_length(scm_frames));
      for (int i = 0; i < nframes; ++i) {
        p->push_back(*D.ser_2_3(scm_list_ref(scm_frames, scm_from_int(i))));
      }

      pout = move(p);

    } else if (4 == version) {

      unique_ptr<id3v2_4_tag> p(new id3v2_4_tag(cbpad, fexp));

      SCM scm_frames = SCM_SLOT(scm_tag, 1);
      int nframes = scm_to_int(scm_length(scm_frames));
      for (int i = 0; i < nframes; ++i) {
        p->push_back(*D.ser_2_4(scm_list_ref(scm_frames, scm_from_int(i))));
      }

      pout = move(p);

    } else {

      scm_error(sym_for_utf8("bad-tag-version"), "scm_to_id3v2_tag",
                "got tag version ~a, about which we know nothing",
                scm_from_int(version), SCM_BOOL_F);

    }

    return pout;

  } // End free function scm_to_id3v2_tag.

}

////////////////////////////////////////////////////////////////////////////////
//                               Scheme API 0.5                               //
////////////////////////////////////////////////////////////////////////////////

extern "C" {

  /**
   * \brief (Maybe) read an ID3v1 tag from a given file
   *
   *
   * \param scm_pth [in] the text (as a Scheme string) giving a path (relative
   * or absolute) to the file of interest)
   *
   * \return an <id3v1-tag> instance if \a scm_pth contains an ID3v1 tag; else
   * nil
   *
   *
   */

  SCM_DEFINE(read_id3v1_tag, READ_ID3V1_TAG, 1, 0, 0, (SCM scm_pth),
             "Read an <id3v1-tag> from file; returns '() if there isn't one.")
  {
    using namespace std;
    using namespace scribbu;

    try {

      dynwind_context ctx;

      char *c_pth = ctx.free_locale_string(scm_pth);
      ifstream ifs = open_ifstream(c_pth, fs::ifstream::binary);

      auto pv1 = process_id3v1(ifs);
      if (!pv1) {
        return SCM_ELISP_NIL;
      }

      // If we're here, there was actually an ID3v1 tag at the end of `ifs'. We
      // now have to build up a GOOPS instance from it.
      SCM cls = scm_c_public_ref("scribbu", "<id3v1-tag>");
      SCM args = scm_list_1(cls);
      SCM x = scm_make(args);

      string title = pv1->title<string>(encoding::ISO_8859_1, encoding::UTF_8,
                                        on_no_encoding::transliterate);
      SCM scm_title = scm_from_stringn(title.c_str(), title.size(), "UTF-8",
                                       SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

      string artist = pv1->artist<string>(encoding::ISO_8859_1, encoding::UTF_8,
                                          on_no_encoding::transliterate);
      SCM scm_artist = scm_from_stringn(artist.c_str(), artist.size(), "UTF-8",
                                        SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

      string album = pv1->album<string>(encoding::ISO_8859_1, encoding::UTF_8,
                                        on_no_encoding::transliterate);
      SCM scm_album = scm_from_stringn(album.c_str(), album.size(), "UTF-8",
                                       SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

      string year = pv1->year<string>();
      SCM scm_year = scm_from_stringn(year.c_str(), year.size(), "UTF-8",
                                      SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

      string comment = pv1->comment<string>(encoding::ISO_8859_1, encoding::UTF_8,
                                            on_no_encoding::transliterate);
      SCM scm_comment = scm_from_stringn(comment.c_str(), comment.size(), "UTF-8",
                                         SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

      unsigned char genre = pv1->genre();
      SCM scm_genre = scm_from_uchar(genre);

      SCM_SET_SLOT(x, 0, scm_title);
      SCM_SET_SLOT(x, 1, scm_artist);
      SCM_SET_SLOT(x, 2, scm_album);
      SCM_SET_SLOT(x, 3, scm_year);
      SCM_SET_SLOT(x, 4, scm_comment);
      SCM_SET_SLOT(x, 5, scm_genre);

      bool v11;
      unsigned char track;
      tie(v11, track) = pv1->track_number();
      if (v11) {
        SCM_SET_SLOT(x, 6, scm_from_uchar(track));
      }

      bool ext;
      unsigned char speed;
      tie(ext, speed) = pv1->speed();

      if (ext) {

        string enh_genre = pv1->enh_genre<string>();
        SCM scm_enh_genre = scm_from_stringn(enh_genre.c_str(), enh_genre.size(),
                                             "UTF-8",
                                             SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
        SCM_SET_SLOT(x, 6, scm_enh_genre);
        SCM_SET_SLOT(x, 7, scm_from_uchar(speed));

        string start_time = pv1->start_time<string>();
        SCM scm_start_time = scm_from_stringn(start_time.c_str(),
                                              start_time.size(),
                                              "UTF-8",
                                              SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
        SCM_SET_SLOT(x, 8, scm_start_time);

        string end_time = pv1->end_time<string>();
        SCM scm_end_time = scm_from_stringn(end_time.c_str(),
                                            end_time.size(),
                                            "UTF-8",
                                            SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
        SCM_SET_SLOT(x, 9, scm_end_time);

      }

      return x;

    }
    catch (const exception &ex) {

      scm_error(scm_from_utf8_symbol("scribbu"), READ_ID3V1_TAG, "~A",
                scm_list_1(scm_from_utf8_string(ex.what())), SCM_ELISP_NIL);

    }

  }

  /**
   * \brief read all ID3v2 tags from a file
   *
   *
   * \param scm_pth [in] a Scheme string containing the path (relative or
   * absolute) to a file to be scanned for ID3v2 tags
   *
   * \return a list of (<id3v2-tag>, version, unsync) instances read from the
   * beginning of \a scm_pth
   *
   *
   */

  SCM_DEFINE(read_tagset, READ_TAGSET, 1, 0, 0, (SCM scm_pth),
             "Read an ID3v2 tagset from file")
  {
    using namespace std;
    using namespace scribbu;

    try {

      dynwind_context ctx;

      ifstream ifs = open_ifstream(ctx.free_locale_string(scm_pth), ifstream::binary);

      // I'm going to do this in three steps:

      // 1. read each tag into an id3v2_tag
      vector<unique_ptr<id3v2_tag>> v2;
      try {
        read_all_id3v2(ifs, back_inserter(v2));
      } catch (const exception &ex) {
        scm_error(sym_for_utf8("invalid-tag"), "read-tagset", "found bad tag: ~s",
                  scm_from_utf8_string(ex.what()), SCM_BOOL_F);
      }

      // 2. transform that collection of C++ id3v2_tag instances into a collection
      // of Scheme <id3v2-tag> instances
      vector<SCM> tags;
      transform(v2.begin(), v2.end(), back_inserter(tags),
                [](const unique_ptr<id3v2_tag>&p) {
                  return scm_from_id3v2_tag(p.get());
                });

      // 3. finally, take that collection of SCMs and build them into a Scheme
      // list
      return scm_list_for_range(tags.begin(), tags.end());

    }
    catch (const exception &ex) {

      scm_error(scm_from_utf8_symbol("scribbu"), READ_TAGSET, "~A",
                scm_list_1(scm_from_utf8_string(ex.what())), SCM_ELISP_NIL);

    }

  } // End read_tagset.

  struct with_track_in_ctx {
    SCM fcn_;
    SCM pth_;
    SCM tagset_;
    SCM v1_;
  };

  SCM with_track_in_thunk(void *pdata)
  {
    with_track_in_ctx *pctx = (with_track_in_ctx*) pdata;
    pctx->tagset_ = read_tagset(pctx->pth_);
    pctx->v1_ = read_id3v1_tag(pctx->pth_);
    return SCM_ELISP_NIL;
  }

  SCM with_track_in_catch_hand(void *pdata, SCM tag, SCM throw_args)
  {
    with_track_in_ctx *pctx = (with_track_in_ctx*) pdata;

    SCM oport = scm_open_output_string();
    scm_print_exception(oport, SCM_BOOL_F, tag, throw_args);
    scm_puts(";;; WARNING: failed to parse ", scm_current_warning_port());
    scm_display(pctx->pth_, scm_current_warning_port());
    scm_puts("\n", scm_current_warning_port());

    scm_close_port(oport);

    return SCM_BOOL_F;

  }

  /**
   * \brief Invoke a scheme procedure for each file in a directory tree
   *
   *
   * \param dir [in] a Scheme string containing the path (relative or
   * absolute) to a file to be scanned for ID3v2 tags
   *
   * \param fcn [in] a Scheme procedure to be invoked on each file in the
   * directory hierarchy rooted at \a dir; the parameters to the procedure
   * will be the ID3v2.3 tagset, a string naming the file, and the ID3v1
   * tag
   *
   * \return nil
   *
   *
   * Errors arisiing from invalid tags are silently discarded (perhaps this
   * should be re-considered? I could return some kind of error
   * description...). The procedure is responsbile for handling any errors
   * arising in its execution.
   *
   *
   */

  SCM_DEFINE(with_track_in, WITH_TRACK_IN, 2, 0, 0, (SCM dir, SCM fcn),
             "Invoke a Scheme procedure for each entry in a directory tree")
  {
    using namespace std;
    using namespace scribbu;

    try {

      dynwind_context ctx;

      std::size_t len = 0;
      char * data = scm_to_stringn(dir, &len, "UTF-8",
                                   SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
      ctx.free(data);

      std::string s(data, data + len);
      for (fs::recursive_directory_iterator p0(s), p1; p0 != p1; ++p0) {
        if (!fs::is_directory(*p0)) {

          SCM scm_pth = scm_from_utf8_string(p0->path().string().c_str());
          with_track_in_ctx ctx{ fcn, scm_pth, SCM_ELISP_NIL, SCM_ELISP_NIL };

          scm_c_catch(SCM_BOOL_T, with_track_in_thunk, &ctx,
                      with_track_in_catch_hand, &ctx, 0, 0);

          scm_call_3(fcn, ctx.tagset_, scm_pth, ctx.v1_);

        }
      }

      return SCM_EOF_VAL;
    }
    catch (const exception &ex) {

      scm_error(scm_from_utf8_symbol("scribbu"), WITH_TRACK_IN, "~A",
                scm_list_1(scm_from_utf8_string(ex.what())), SCM_ELISP_NIL);

    }

  }

  /**
   * \brief Write an ID3v1 tag to a given file, replacing any ID3v1 tag that
   * might be present
   *
   *
   * \param scm_tag [in] the ID3v1 tag to be written
   *
   * \param scm_pth [in] the text (as a Scheme string) giving a path (relative
   * or absolute) to the file of interest)
   *
   *
   */

  SCM_DEFINE(write_id3v1_tag, WRITE_ID3V1_TAG, 2, 0, 0,
             (SCM scm_tag, SCM scm_pth),
             "Write an <id3v1-tag> to file; replace any extant ID3v1 tag.")
  {
    using namespace std;
    using namespace scribbu;

    try {
      dynwind_context ctx;

      SCM scm_title = SCM_SLOT(scm_tag, 0);
      char *c_title = ctx.free_locale_string(scm_title);

      SCM scm_artist = SCM_SLOT(scm_tag, 1);
      char *c_artist = ctx.free_locale_string(scm_artist);

      SCM scm_album = SCM_SLOT(scm_tag, 2);
      char *c_album = ctx.free_locale_string(scm_album);

      // This slot can be '() to indicuate "not set"
      char *c_year = 0;
      SCM scm_year = SCM_SLOT(scm_tag, 3);
      if (!scm_null_p(scm_year)) {
        c_year = ctx.free_locale_string(scm_year);
      }

      SCM scm_comment = SCM_SLOT(scm_tag, 4);
      char *c_comment = ctx.free_locale_string(scm_comment);

      SCM scm_genre = SCM_SLOT(scm_tag, 5);
      unsigned char genre = scm_to_uchar(scm_genre);

      SCM scm_track_no = SCM_SLOT(scm_tag, 6);
      unsigned char track_no = scm_to_uchar(scm_track_no);

      SCM scm_enh_genre = SCM_SLOT(scm_tag, 7);
      char *c_enh_genre = ctx.free_locale_string(scm_enh_genre);

      unsigned char speed = 0;
      SCM scm_speed = SCM_SLOT(scm_tag, 8);
      if (!scm_null_p(scm_speed)) {
        speed = scm_to_uchar(scm_speed);
      }

      SCM scm_start_time = SCM_SLOT(scm_tag, 9);
      char *c_start_time = ctx.free_locale_string(scm_start_time);

      SCM scm_end_time = SCM_SLOT(scm_tag, 10);
      char *c_end_time = ctx.free_locale_string(scm_end_time);

      bool v11 = track_no != 0;
      bool enh = (strlen(c_enh_genre)  != 0) ||
        (speed != 0)                ||
        (strlen(c_start_time) != 0) ||
        (strlen(c_end_time)   != 0);

      id3v1_tag tag(v11, enh);

      if (c_title && 0 != strlen(c_title)) {
        tag.set_title(c_title, encoding::UTF_8, encoding::UTF_8);
      }
      if (c_artist && 0 != strlen(c_artist)) {
        tag.set_artist(c_artist, encoding::UTF_8, encoding::UTF_8);
      }
      if (c_album && 0 != strlen(c_album)) {
        tag.set_album(c_album, encoding::UTF_8, encoding::UTF_8);
      }

      if (c_year && 0 != strlen(c_year)) { // treat "" as "not set"
        if (4 != strlen(c_year)) {
          // Will *not* return or file dtors!
          scm_misc_error(WRITE_ID3V1_TAG, "incorrect year ~s", scm_year);
        }
        tag.set_year(c_year);
      }

      if (c_comment && 0 != strlen(c_comment)) {
        tag.set_comment(c_comment, encoding::UTF_8, encoding::UTF_8);
      }

      tag.set_genre(genre);

      if (v11) {
        tag.set_track_number(track_no);
      }

      if (enh) {
        tag.set_enh_genre(c_enh_genre, encoding::UTF_8, encoding::UTF_8);
        tag.set_speed(speed);
        if (c_start_time) {
          size_t n = strlen(c_start_time);
          if (n != 0) {
            if (6 != strlen(c_start_time)) {
              // Will *not* return or file dtors!
              scm_misc_error(WRITE_ID3V1_TAG, "incorrect start time len ~s",
                             scm_start_time);
            }
            tag.set_start_time(c_start_time);
          }
        }

        if (c_end_time) {
          size_t n = strlen(c_end_time);
          if (n != 0) {
            if (6 != strlen(c_end_time)) {
              // Will *not* return or file dtors!
              scm_misc_error(WRITE_ID3V1_TAG, "incorrect end time len ~s",
                             scm_end_time);
            }
            tag.set_end_time(c_end_time);
          }
        }
      }

      replace_id3v1(fs::path(ctx.free_locale_string(scm_pth)), tag);

      return SCM_EOL;
    }
    catch (const exception &ex) {

      scm_error(scm_from_utf8_symbol("scribbu"), WRITE_ID3V1_TAG, "~A",
                scm_list_1(scm_from_utf8_string(ex.what())), SCM_ELISP_NIL);

    }

  }

  /**
   * \brief write a set of ID3v2 tags to file
   *
   *
   * \param scm_tags [in] a Scheme list of two-tuples (<id3v2-tag> instance,
   * ID3v2 version as which to serialize (i.e. 2, 3 or 4)
   *
   * \param scm_pth [in] a Scheme string containing the path (relative or
   * absolute) to a file whose ID3v2 tags (if any) will be replaced with those
   * in \a scm_tags
   *
   * \param scm_rest [in] keyword arguments; at present there are two keywords
   * accepted: \c #:apply-unsync & \c #:copy. apply-unsync, if #t, apply the
   * unsynchronisation scheme to all tags; if #f apply to none, and if \c
   * 'as-needed, apply to each tag on an as-needed basis to prevent false
   * syncs. It defaults to #f. copy, if #t, make a backup-copy; if false do not
   * (defaults to #f).
   *
   *
   */

  SCM_DEFINE(write_tagset, WRITE_TAGSET, 2, 0, 1,
             (SCM scm_tags, SCM scm_path, SCM scm_rest),
             "Write an ID3v2 tagset to file")
  {
    using namespace std;
    using namespace scribbu;

    try {
      SCM scm_apply_unsync = SCM_BOOL_F, scm_copy = SCM_BOOL_F;
      scm_c_bind_keyword_arguments(WRITE_TAGSET, scm_rest,
                                   (scm_t_keyword_arguments_flags)0,
                                   kw_apply_unsync, &scm_apply_unsync,
                                   kw_copy, &scm_copy,
                                   SCM_UNDEFINED);

      apply_unsync au = apply_unsync::never;
      if (SCM_BOOL_T == scm_apply_unsync) {
        au = apply_unsync::always;
      } else if (SCM_BOOL_T == scm_equal_p(sym_as_needed, scm_apply_unsync)) {
        au = apply_unsync::as_needed;
      } else if (SCM_BOOL_F != scm_apply_unsync) {
        scm_error(scm_from_utf8_symbol("unexpected-apply-unsync"),
                  WRITE_TAGSET, "expected #t, #f or 'as-needed, got ~A",
                  scm_apply_unsync, SCM_BOOL_F);
      }

      bool fcopy = SCM_BOOL_T == scm_copy;

      // 1. walk scm_tags, converting each to an id3v2_tag subclass
      vector<unique_ptr<id3v2_tag>> tags;

      size_t len = scm_to_size_t(scm_length(scm_tags));
      for (size_t i = 0; i < len; ++i) {
        SCM scm_tag = scm_list_ref(scm_tags, scm_from_size_t(i));
        tags.emplace_back(scm_to_id3v2_tag(scm_tag));
      }

      // 2. write the tagset
      dynwind_context ctx;

      fs::path pth(ctx.free_locale_string(scm_path));
      if (fcopy) {
        replace_tagset_copy(pth, tags.begin(), tags.end(), au);
      } else {
        maybe_emplace_tagset(pth, tags.begin(), tags.end(), au,
                             emplace_strategy::reduce_padding_evenly,
                             padding_strategy::adjust_padding_evenly);
      }

      return SCM_EOL;
    }
    catch (const exception &ex) {

      scm_error(scm_from_utf8_symbol("scribbu"), WRITE_TAGSET, "~A",
                scm_list_1(scm_from_utf8_string(ex.what())), SCM_ELISP_NIL);

    }

  }

  SCM
  get_version_variable(void*)
  {
    return scm_c_public_variable("system repl common", "*version*");
  }

  SCM
  get_version_variable_hand(void*, SCM, SCM)
  {
    return SCM_EOL;
  }

  void
  customize_welcome()
  {
    using namespace std;

    // Attempt to lookup variable "*version*" in module
    // `(system repl common)'...
    SCM scm_version = scm_c_catch(SCM_BOOL_T,
                                  get_version_variable, 0,
                                  get_version_variable_hand, 0,
                                  0, 0);
    // and if we're not successful...
    if (SCM_EOL == scm_version) {
      // just silently give up & continue.
      return;
    }

    SCM scm_value = scm_variable_ref(scm_version);

    dynwind_context ctx;
    char *ptext = ctx.free_locale_string(scm_value);

    string welcome(PACKAGE_STRING "\n" COPYRIGHT " " AUTHOR "\n\nYou are in the Guile REPL; in your shell, type `info scribbu' for documentation.\n\n");
    welcome += ptext;

    SCM scm_new = scm_from_stringn(welcome.c_str(), welcome.size(), "UTF-8",
                                   SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
    scm_variable_set_x(scm_version, scm_new);
  }

  SCM
  get_load_path_variable(void*)
  {
    return scm_c_lookup("%load-path");
  }

  SCM
  get_load_path_variable_hand(void*, SCM, SCM)
  {
    return SCM_EOL;
  }

  void
  customize_load_path(const std::string &datadir)
  {
    SCM scm_load_path = scm_c_catch(SCM_BOOL_T,
                                    get_load_path_variable, 0,
                                    get_load_path_variable_hand, 0,
                                    0, 0);
    if (SCM_EOL == scm_load_path) {
      return;
    }

    SCM scm_value = scm_variable_ref(scm_load_path); // should be a list
    if (!scm_is_pair(scm_value)) {
      return;
    }

    // Walk that list, checking for DATADIR/guile/site
    std::string site = datadir + "/guile/site";
    dynwind_context ctx;
    bool found = false;
    for (int i = 0, n = scm_to_int(scm_length(scm_value)); i < n; ++i) {
      SCM scm = scm_list_ref(scm_value, scm_from_int(i));
      char *c_pth = ctx.free_locale_string(scm);
      if (0 == strcmp(c_pth, site.c_str())) {
        found = true;
        break;
      }
    }

    if (!found) {
      SCM args = scm_list_2(scm_list_1(scm_from_locale_string(site.c_str())),
                            scm_value);
      scm_variable_set_x(scm_load_path, scm_append(args));
    }
  }

  // Initializae the Guile interpreter
  void*
  initialize_guile(void *praw)
  {
    const init_guile *pig = reinterpret_cast<const init_guile*>(praw);

    scribbu::init_symbols();

#   ifndef SCM_MAGIC_SNARFER
#   include "scheme.x"
#   endif

    customize_welcome();
    customize_load_path(pig->datadir_);

    return 0;
  }

} // End extern "C".

