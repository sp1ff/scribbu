/**
 * \file scheme.cc
 *
 * Copyright (C) 2015-2018 Michael Herstine <sp1ff@pobox.com>
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

#include <string>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/functional/hash.hpp>

namespace fs = boost::filesystem;

///////////////////////////////////////////////////////////////////////////////
//                              utility code                                 //
///////////////////////////////////////////////////////////////////////////////

#define ADD_ID3V2_COMMENT     "scribbu/add-id3v2-comment"
#define ADD_USER_DEFINED_TEXT "scribbu/add-user-defined-text"
#define AUTHOR                "Michael Herstine <sp1ff@pobox.com>"
#define COPYRIGHT             "Copyright (C) 2017-2018"
#define DELETE_ID3V2_TAG      "scribbu/delete-id3v2-tag"
#define GET_FRAME             "scribbu/get-frame"
#define GET_ID3V1_STRING      "scribbu/get-id3v1-string"
#define GET_ID3V2_ATTRIBUTE   "scribbu/get-id3v2-attribute"
#define GET_PATH              "scribbu/get-path"
#define HAS_FRAME             "scribbu/has-frame"
#define HAS_ID3V1_TAG         "scribbu/has-id3v1-tag"
#define MAKE_ID3V2_TAG        "scribbu/make-id3v2-tag"
#define MAKE_TRACK            "scribbu/make-track"
#define NUM_ID3V2_TAGS        "scribbu/get-id3v2-tag-count"
#define REPLACE_TAGS          "scribbu/replace-tags"
#define SET_FRAME             "scribbu/set-frame"
#define WITH_TRACK_IN         "scribbu/with-track-in"
#define WRITE_ID3V2_TAG       "scribbu/write-id3v2-tag"
#define WRITE_TRACK           "scribbu/write-track"

namespace {
  
  /////////////////////////////////////////////////////////////////////////////
  //                              Guile constants                            //
  /////////////////////////////////////////////////////////////////////////////

  SCM_GLOBAL_KEYWORD(kw_crc, "checksum");
  SCM_GLOBAL_KEYWORD(kw_description, "description");
  SCM_GLOBAL_KEYWORD(kw_dst_encoding, "destination-encoding");
  SCM_GLOBAL_KEYWORD(kw_experimental, "experimental");
  SCM_GLOBAL_KEYWORD(kw_ext_header, "extended-header");
  SCM_GLOBAL_KEYWORD(kw_language, "language");
  SCM_GLOBAL_KEYWORD(kw_load_data, "load-data");
  SCM_GLOBAL_KEYWORD(kw_padding, "padding");
  SCM_GLOBAL_KEYWORD(kw_src_encoding, "source-encoding");
  SCM_GLOBAL_KEYWORD(kw_unicode, "unicode");

  // SCM sym_album;
  SCM_GLOBAL_SYMBOL(sym_album, "scribbu/album");
  SCM_GLOBAL_SYMBOL(sym_artist, "scribbu/artist");
  SCM_GLOBAL_SYMBOL(sym_comment, "scribbu/comment");
  SCM_GLOBAL_SYMBOL(sym_content_type, "scribbu/content-type");
  SCM_GLOBAL_SYMBOL(sym_encoded_by, "scribbu/encoded-by");
  SCM_GLOBAL_SYMBOL(sym_id3v2_revision, "scribbu/id3v2-revision");
  SCM_GLOBAL_SYMBOL(sym_id3v2_version, "scribbu/id3v2-version");
  SCM_GLOBAL_SYMBOL(sym_title, "scribbu/title");
  SCM_GLOBAL_SYMBOL(sym_track, "scribbu/track");
  SCM_GLOBAL_SYMBOL(sym_year, "scribbu/year");

}

/// Stack-based allocator for scm_dynwind_begin/end
class dynwind_context
{
public:
  dynwind_context(bool rewindable = false);
  ~dynwind_context()
  { scm_dynwind_end(); }
  void free(void *p)
  { scm_dynwind_free(p); }
  char* free_locale_string(SCM scm);

  dynwind_context(const dynwind_context&)            = delete;
  dynwind_context(dynwind_context&&)                 = delete;
  dynwind_context& operator=(const dynwind_context&) = delete;
  dynwind_context& operator=(dynwind_context&&)      = delete;

};

dynwind_context::dynwind_context(bool rewindable /*= false*/)
{
  scm_dynwind_begin((scm_t_dynwind_flags) (rewindable ? SCM_F_DYNWIND_REWINDABLE : 0));
}

char* dynwind_context::free_locale_string(SCM scm)
{
  char *p = scm_to_locale_string(scm);
  scm_dynwind_free(p);
  return p;
}


///////////////////////////////////////////////////////////////////////////////
//                        Scheme foreign type `track'                        //
///////////////////////////////////////////////////////////////////////////////

/**
 * \brief Foreign object type `track'
 *
 *
 * Class scm_track represents zero or more ID3v2 tags, track data, and
 * optionally a ID3v1. tag.  The track data may be the actual track data, or it
 * may be abbreviated by a track_data instance. The former is obviously more
 * heavyweight, but allows loading the .mp3 file, editing the tags, and writing
 * the result to disk regardless of whether the underlying file is present. The
 * latter is more lightweight while still allowing replacing the tagset on an
 * extant file.
 *
 * Cf. https://www.gnu.org/software/guile/manual/guile.html#Defining-Foreign-Object-Types
 *
 *
 */

class scm_track
{
public:
  scm_track(const fs::path &pth, bool load_data = false);

public:
  fs::path path() const
  { return pth_; }
  scribbu::id3v1_tag* id3v1_tag() const
  { return pv1_.get(); }
  std::size_t num_id3v2_tags() const
  { return v2_.size(); }
  const scribbu::id3v2_tag&
  get_id3v2_tag(unsigned int idx) const;
  scribbu::id3v2_tag&
  get_id3v2_tag(unsigned int idx);
  void
  insert_id3v2_tag(std::unique_ptr<scribbu::id3v2_tag> &ptag, std::size_t idx)
  { v2_.insert(v2_.begin() + idx, std::move(ptag)); }
  /// Write a single ID3v2 tag to disk
  std::size_t write_id3v2_tag(std::ostream &os, std::size_t idx) const
  { return v2_[idx]->write(os); }
  void delete_id3v2_tag(std::size_t idx)
  { v2_.erase(v2_.begin() + idx); }
  /// Write tagset & data to \a pth; will throw if "load data" wasn't specified
  /// on construction
  void write(const fs::path &pth) const;
  /// Just replace the tagset on \a pth; leave the track data unchanged
  void replace_tags(const fs::path &pth) const;


private:
  fs::path pth_;
  std::vector<std::unique_ptr<scribbu::id3v2_tag>> v2_;
  std::unique_ptr<scribbu::track_data> ptd_;
  bool data_loaded_;
  std::vector<unsigned char> data_;
  std::unique_ptr<scribbu::id3v1_tag> pv1_;

};

scm_track::scm_track(const fs::path &pth, bool load_data /*= false*/): pth_(pth), data_loaded_(false)
{
  fs::ifstream ifs(pth_, fs::ifstream::binary);

  // Read all ID3v2 tags into v2_...
  scribbu::read_all_id3v2(ifs, back_inserter(v2_));
  // `ifs' get pointer is now at the beginning of the audio date,
  fs::ifstream::pos_type begin = ifs.tellg();
  // and this call will consume everything up tot he ID3v1 tag, or the end of
  // the file...
  ptd_.reset(new scribbu::track_data(ifs));
  if (load_data) {
    // move the get ptr back tot he beginning of the track,
    ifs.seekg(begin);
    // compute the size of the track...
    scribbu::id3v1_info I = scribbu::ends_in_id3v1(ifs);
    std::size_t cb = I.start_ - begin;
    data_.resize(cb);
    // and copy it.
    ifs.read((char*)&(data_[0]), cb);
    data_loaded_ = true;
  }

  pv1_ = scribbu::process_id3v1(ifs);
}

const scribbu::id3v2_tag&
scm_track::get_id3v2_tag(unsigned int idx) const
{
  if (idx >= v2_.size()) {
    std::stringstream stm;
    if (v2_.empty()) {
      stm << "no ID3v2 tags";
    }
    else {
      stm << "index " << idx << " is out-of-range ([0-" << v2_.size() - 1 << "])";
    }
    throw std::range_error(stm.str());
  }
  return *v2_[idx];
}

scribbu::id3v2_tag&
scm_track::get_id3v2_tag(unsigned int idx)
{
  if (idx >= v2_.size()) {
    std::stringstream stm;
    if (v2_.empty()) {
      stm << "no ID3v2 tags";
    }
    else {
      stm << "index " << idx << " is out-of-range ([0-" << v2_.size() - 1 << "])";
    }
    throw std::range_error(stm.str());
  }
  return *v2_[idx];
}

void
scm_track::write(const fs::path &pth) const
{
  using namespace std;
  using namespace scribbu;

  if (! data_loaded_) {
    throw std::logic_error("Cannot write a track that wasn't created with load-data");
  }

  fs::ofstream os(pth, fs::ofstream::binary);

  // Begin by writing the ID3v2 tags we have:
  for_each(v2_.begin(), v2_.end(), [&os](const unique_ptr<id3v2_tag> &ptag) { ptag->write(os); });

  // We're now at the start of the track data:
  os.write((char*)&(data_[0]), data_.size());

  // Write our ID3v1 tag, if any
  if (pv1_) {
    pv1_->write(os);
  }

}

/// Just replace the tagset on \a pth; leave the track data unchanged
void
scm_track::replace_tags(const fs::path &pth) const
{
  using namespace std;
  using namespace scribbu;

  const ios::iostate EXC_MASK = ios::eofbit | ios::failbit | ios::badbit;

  // Open our source file...
  fs::ifstream ifs(pth_, fs::ifstream::binary);
  ifs.exceptions(EXC_MASK);

  // as well as a temporary output file...
  fs::path tmp = fs::unique_path();
  fs::ofstream ofs(tmp, fs::ifstream::binary);

  // Begin by writing the ID3v2 tags we have to `tmp':
  for_each(v2_.begin(), v2_.end(), [&ofs](const unique_ptr<id3v2_tag> &ptag) { ptag->write(ofs); });

  // Now skip over any ID3v2 tags in the input file...
  scribbu::id3v2_info id3v2 = scribbu::looking_at_id3v2(ifs);
  while (id3v2.present_) {
    ifs.seekg(id3v2.size_, ios_base::cur);
    // Another?
    id3v2 = scribbu::looking_at_id3v2(ifs);
  }

  id3v1_info I = ends_in_id3v1(ifs);
  size_t cb = I.start_ - ifs.tellg();
  std::unique_ptr<char[]> p(new char[cb]);
  ifs.read(p.get(), cb);
  ofs.write(p.get(), cb);

  // Write our ID3v1 tag, if any
  if (pv1_) {
    pv1_->write(ofs);
  }

  ifs.close();
  ofs.close();

  // Finally, move the temporary file over our input file:
  fs::rename(tmp, pth);
}

namespace {

  SCM track_type; // initialized in init_track_type

  SCM
  track_for_path(const fs::path &pth, bool load_data=false)
  {
    return scm_make_foreign_object_1(track_type, new scm_track(pth, load_data));
  }

  void
  finalize_track(SCM scm)
  {
    scm_track *p = (scm_track*) scm_foreign_object_ref(scm, 0);
    if (p) {
      delete p;
      scm_foreign_object_set_x(scm, 0, 00);
    }
  }

  SCM_DEFINE(make_track, MAKE_TRACK, 1, 0, 1,
             (SCM pth, SCM rest),
             "Create a new track instance from a file")
  {
    dynwind_context ctx;

    char *ppth = ctx.free_locale_string(pth);

    SCM scm_load_data = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments(MAKE_TRACK, rest, (scm_t_keyword_arguments_flags)0,
                                 kw_load_data, &scm_load_data, SCM_UNDEFINED);

    bool load_data = false;
    if (!SCM_UNBNDP(scm_load_data)) {
      load_data = scm_to_bool(scm_load_data);
    }

    SCM scm = track_for_path(fs::path(ppth), load_data);

    return scm;
  }

  // https://www.gnu.org/software/guile/manual/guile.html#Defining-Foreign-Object-Types
  void
  init_track_type (void)
  {
    SCM name, slots;
    scm_t_struct_finalize finalizer;

    name = scm_from_utf8_symbol("track");
    slots = scm_list_1(scm_from_utf8_symbol("data"));
    finalizer = finalize_track;

    track_type = scm_make_foreign_object_type(name, slots, finalizer);
  }

}


///////////////////////////////////////////////////////////////////////////////
//                              Scheme Support                               //
///////////////////////////////////////////////////////////////////////////////

namespace {

  SCM
  frame_as_scm(const scribbu::id3v2_tag &tag, SCM attr)
  {
    using namespace std;

    SCM result;
    string text;
    if (scm_is_symbol(attr)) {

      if (scm_is_eq(attr, sym_album)) {
        text = tag.album();
      }
      else if (scm_is_eq(attr, sym_artist)) {
        text = tag.artist();
      }
      else if (scm_is_eq(attr, sym_content_type)) {
        text = tag.content_type();
      }
      else if (scm_is_eq(attr, sym_encoded_by)) {
        text = tag.encoded_by();
      }
      else if (scm_is_eq(attr, sym_title)) {
        text = tag.title();
      }
      else if (scm_is_eq(attr, sym_track)) {
        text = tag.track();
      }
      else if (scm_is_eq(attr, sym_year)) {
        text = tag.year();
      }
      else {
        // NB. Will *not* return or fire dtors!
        scm_misc_error(GET_FRAME, "unrecognized symbol: ~s", attr);
      }

      result = scm_from_stringn(text.c_str(), text.size(), "UTF-8",
                                SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
    }
    else {
      // NB. Will *not* return or fire dtors!
      scm_misc_error(GET_FRAME, "unrecognized symbol: ~s", attr);
    }

    return result;
  }

  void
  set_frame_as_scm(scribbu::id3v2_tag &tag, SCM attr, SCM value)
  {
    using namespace std;

    // We're about to start unpacking strings-- open a dynamic wind context
    dynwind_context ctx;

    char* pvalue = scm_to_utf8_stringn(value, NULL);
    ctx.free(pvalue);

    if (scm_is_symbol(attr)) {

      if (scm_is_eq(attr, sym_album)) {
        tag.album(pvalue);
      }
      else if (scm_is_eq(attr, sym_artist)) {
        tag.artist(pvalue);
      }
      else if (scm_is_eq(attr, sym_content_type)) {
        tag.content_type(pvalue);
      }
      else if (scm_is_eq(attr, sym_encoded_by)) {
        tag.encoded_by(pvalue);
      }
      else if (scm_is_eq(attr, sym_title)) {
        tag.title(pvalue);
      }
      else if (scm_is_eq(attr, sym_track)) {
        tag.track(pvalue);
      }
      else if (scm_is_eq(attr, sym_year)) {
        tag.year(pvalue);
      }
      else {
        // Will *not* return or file dtors!
        scm_misc_error(HAS_FRAME, "unknown attribute ~s", attr);
      }

    }
    else {
      // NB. Will *not* return or fire dtors!
      scm_misc_error(SET_FRAME, "unrecognized symbol: ~s", attr);
    }

  }
}

extern "C" {

  SCM_DEFINE(add_id3v2_comment, ADD_ID3V2_COMMENT, 3, 0, 1,
             (SCM track, SCM index, SCM scm_text, SCM rest),
             "Add an ID3v2 comment to a tag")
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(index));

    SCM scm_lang    = SCM_UNDEFINED;
    SCM scm_src     = SCM_UNDEFINED;
    SCM scm_unicode = SCM_UNDEFINED;
    SCM scm_dsc     = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments(ADD_ID3V2_COMMENT, rest,
                                 (scm_t_keyword_arguments_flags)0,
                                 kw_language, &scm_lang,
                                 kw_description, &scm_dsc,
                                 kw_unicode, &scm_unicode,
                                 kw_src_encoding, &scm_src,
                                 SCM_UNDEFINED);

    char *ptext = scm_to_utf8_stringn(scm_text, NULL);
    ctx.free(ptext);
    string text(ptext);

    language lang = language::from_locale;
    if (!SCM_UNBNDP(scm_lang)) {
      char *p = scm_to_locale_string(scm_lang);
      ctx.free(p);
      stringstream stm(p);
      stm >> lang;
    }

    scribbu::encoding src = scribbu::encoding::UTF_8;
    if (!SCM_UNBNDP(scm_src)) {
      char *p = scm_to_locale_string(scm_src);
      ctx.free(p);
      stringstream stm(p);
      stm >> src;
    }

    use_unicode uni = use_unicode::no;
    if (!SCM_UNBNDP(scm_unicode) && scm_is_true(scm_unicode)) {
      uni = use_unicode::yes;
    }

    string dsc;
    if (!SCM_UNBNDP(scm_dsc)) {
      char *p = scm_to_locale_string(scm_dsc);
      ctx.free(p);
      dsc = p;
    }

    tag.add_comment(text, lang, src, uni, dsc);

    return SCM_EOL;
  }

  SCM_DEFINE(add_user_defined_text, ADD_USER_DEFINED_TEXT, 3, 0, 1,
             (SCM track, SCM index, SCM scm_text, SCM rest),
             "Add a user-defined text frame to an ID3v2 tag")
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(index));

    SCM scm_src     = SCM_UNDEFINED;
    SCM scm_unicode = SCM_UNDEFINED;
    SCM scm_dsc     = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments(ADD_USER_DEFINED_TEXT, rest,
                                 (scm_t_keyword_arguments_flags)0,
                                 kw_description, &scm_dsc,
                                 kw_unicode, &scm_unicode,
                                 kw_src_encoding, &scm_src,
                                 SCM_UNDEFINED);

    char *ptext = scm_to_utf8_stringn(scm_text, NULL);
    ctx.free(ptext);
    string text(ptext);

    scribbu::encoding src = scribbu::encoding::UTF_8;
    if (!SCM_UNBNDP(scm_src)) {
      char *p = scm_to_locale_string(scm_src);
      ctx.free(p);
      stringstream stm(p);
      stm >> src;
    }

    use_unicode uni = use_unicode::no;
    if (!SCM_UNBNDP(scm_unicode) && scm_is_true(scm_unicode)) {
      uni = use_unicode::yes;
    }

    string dsc;
    if (!SCM_UNBNDP(scm_dsc)) {
      char *p = scm_to_locale_string(scm_dsc);
      ctx.free(p);
      dsc = p;
    }

    tag.add_user_defined_text(text, src, uni, dsc);

    return SCM_EOL;
  }

  SCM_DEFINE(delete_id3v2_tag, "scribbu/delete-id3v2-tag", 2, 0, 0,
             (SCM track, SCM index),
             "Delete an ID3v2 tag from a track")
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    ptrack->delete_id3v2_tag(scm_to_uint(index));

    return SCM_EOL;
  }

  SCM_DEFINE(get_path, GET_PATH, 1, 0, 0,
             (SCM track),
             "Return the path from which a track was created")
  {
    scm_track *p = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_locale_string(p->path().c_str());
  }

  SCM_DEFINE(has_id3v1_tag, HAS_ID3V1_TAG, 1, 0, 0,
             (SCM track),
             "Return true iff the given track has an ID3v1 tag")
  {
    scm_track *p = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_bool(0 != p->id3v1_tag());
  }

  SCM_DEFINE(get_id3v1_string, GET_ID3V1_STRING, 2, 0, 1,
             (SCM track, SCM item, SCM rest),
             "Retrieve the given string from the track's ID3v1 tag.")
  {
    using namespace std;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    dynwind_context ctx;

    SCM src_encoding = SCM_UNDEFINED;
    SCM dst_encoding = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments(GET_ID3V1_STRING, rest,
                                  (scm_t_keyword_arguments_flags)0,
                                 kw_src_encoding, &src_encoding,
                                 kw_dst_encoding, &dst_encoding,
                                 SCM_UNDEFINED);
    scribbu::encoding src = scribbu::encoding::CP1252;
    if (!SCM_UNBNDP(src_encoding)) {
      char *p = scm_to_locale_string(src_encoding);
      ctx.free(p);
      stringstream stm(p);
      stm >> src;
    }

    scribbu::encoding dst = scribbu::encoding::UTF_8;
    if (!SCM_UNBNDP(src_encoding)) {
      char *p = scm_to_locale_string(dst_encoding);
      ctx.free(p);
      stringstream stm(p);
      stm >> dst;
    }

    const scribbu::id3v1_tag *ptag = ptrack->id3v1_tag();
    if (!ptag) {
      scm_misc_error(GET_ID3V1_STRING, "no ID3v1 tag", SCM_ELISP_NIL);
    }

    string text;

    if (scm_is_eq(item, sym_album)) {
      text = ptag->album<string>(src, dst);
    }
    else if (scm_is_eq(item, sym_artist)) {
      text = ptag->artist<string>(src, dst);
    }
    else if (scm_is_eq(item, sym_comment)) {
      text = ptag->comment<string>(src, dst);
    }
    else if (scm_is_eq(item, sym_title)) {
      text = ptag->title<string>(src, dst);
    }
    else if (scm_is_eq(item, sym_year)) {
      text = ptag->year<string>(src, dst);
    }
    else {
      // Will *not* return or file dtors!
      scm_misc_error(GET_ID3V1_STRING, "unknown attribute ~s", item);
    }

    return scm_from_stringn(text.c_str(), text.size(), "UTF-8",
                            SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
  }

  SCM_DEFINE(num_id3v2_tags, NUM_ID3V2_TAGS, 1, 0, 0,
             (SCM track),
             "Return the number of ID3v2 tags present in a track")
  {
    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_int(ptrack->num_id3v2_tags());
  }

  SCM_DEFINE(with_track_in, WITH_TRACK_IN, 2, 0, 0,
             (SCM dir, SCM fcn),
             "Invoke a Scheme procedure for each entry in a directory tree")
  {
    dynwind_context ctx;

    std::size_t len = 0;
    char * data = scm_to_stringn(dir, &len, "UTF-8", SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
    ctx.free(data);

    std::string s(data, data + len);
    for (fs::recursive_directory_iterator p0(s), p1; p0 != p1; ++p0) {
      if (!fs::is_directory(*p0)) {
        scm_call_1(fcn, track_for_path(*p0));
      }
    }

    return SCM_EOF_VAL;
  }

  /**
   * \brief Check to see whether an ID3v2 tag has a given frame
   *
   *
   * \param track [in] SCM referring to an instance of the track foreign type
   *
   * \param index [in] SCM referring to an unisgned int identifying the ID3v2
   * tag in which the caller is interested
   *
   * \param frameid [in] SCM referring to either a pre-defined symbol naming an
   * ID3v2 frame ('e.g. 'encoded-by), or the text naming a frame
   *
   * \return the number of frames matching the given frame ID
   *
   *
   * This differs from has_id3v2_attribute in that it can accept arbitrary
   * frame identifiers (as opposed to a finite set of core ID3v2 attributes).
   *
   *
   */

  SCM_DEFINE(has_frame, HAS_FRAME, 3, 0, 0,
             (SCM track, SCM index, SCM frameid),
             "Check to see whether an ID3v2 tag has a given frame")
  {
    using namespace std;
    using namespace scribbu;

    dynwind_context ctx;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    const id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(index));

    size_t fresult = 0;
    if (scm_is_symbol(frameid)) {

      if (scm_is_eq(frameid, sym_album)) {
        fresult = tag.has_album();
      }
      else if (scm_is_eq(frameid, sym_artist)) {
        fresult = tag.has_artist();
      }
      else if (scm_is_eq(frameid, sym_content_type)) {
        fresult = tag.has_content_type();
      }
      else if (scm_is_eq(frameid, sym_encoded_by)) {
        fresult = tag.has_encoded_by();
      }
      else if (scm_is_eq(frameid, sym_title)) {
        fresult = tag.has_title();
      }
      else if (scm_is_eq(frameid, sym_track)) {
        fresult = tag.has_track();
      }
      else if (scm_is_eq(frameid, sym_year)) {
        fresult = tag.has_year();
      }
      else {
        // NB. Will *not* return or fire dtors!
        scm_misc_error(HAS_FRAME, "unrecognized symbol: ~s", frameid);
      }

    }
    else {

      char *pid = scm_to_locale_string(frameid);
      ctx.free(pid);

      if (2 == tag.version()) {
        const id3v2_2_tag& tag22 = dynamic_cast<const id3v2_2_tag&>(tag);
        fresult = tag22.has_frame(pid);
      }
      else if (3 == tag.version()) {
        const id3v2_3_tag& tag23 = dynamic_cast<const id3v2_3_tag&>(tag);
        fresult = tag23.has_frame(pid);
      }
      else {
        const id3v2_4_tag& tag24 = dynamic_cast<const id3v2_4_tag&>(tag);
        fresult = tag24.has_frame(pid);
      }

    }

    return scm_from_unsigned_integer(fresult);
  }

  /**
   * \brief Return the values of one or more frames in an ID3v2 tag
   *
   *
   * \param track [in] SCM referring to an instance of the track foreign type
   *
   * \param idx [in] SCM referring to an unisgned int referring to the ID3v2
   * tag in which the caller is interested
   *
   * \param attr [in] either a scalar or a list; each scalar may be either a
   * string naming the frame, or a predefined symbol
   *
   * \return either a scalar or a list; one scalar for each required frame
   *
   *
   */

  SCM_DEFINE(get_frame, GET_FRAME, 3, 0, 0,
             (SCM track, SCM idx, SCM attr),
             "Return the values of one or more frames in an ID3v2 tag")
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    const id3v2_tag &tag = ptrack->get_id3v2_tag(scm_to_uint(idx));

    SCM result = SCM_UNDEFINED;

    if (scm_is_true(scm_list_p(attr))) {
      SCM len_as_scm = scm_length(attr);
      result = scm_make_list(len_as_scm, SCM_UNDEFINED);
      for (uint32_t i = 0, n = scm_to_uint32(len_as_scm); i < n; ++i) {
        SCM scm = scm_list_ref(attr, scm_from_unsigned_integer(i));
        scm_list_set_x(result, scm_from_unsigned_integer(i), frame_as_scm(tag, scm));
      }
    }
    else {
      result = frame_as_scm(tag, attr);
    }

    return result;

  }

  /**
   * \brief Set an ID3v2 attribute
   *
   *
   */

  SCM_DEFINE(set_frame, SET_FRAME, 3, 1, 0,
             (SCM track, SCM index, SCM attr, SCM value),
             "Set an ID3v2 attribute")
  {
    using namespace std;
    using namespace scribbu;

    static const SCM ZERO = scm_from_uint32(0);
    static const SCM ONE  = scm_from_uint32(1);

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(index));

    if (SCM_UNDEFINED == value) {
      // `attr' should be a list of pairs of attribute/value pairs
      for (unsigned i = 0, n = scm_to_uint32(scm_length(attr)); i < n; ++i) {
        SCM pair = scm_list_ref(attr, scm_from_uint32(i));
        set_frame_as_scm(tag, scm_list_ref(pair, ZERO), scm_list_ref(pair, ONE));
      }
    }
    else {
      set_frame_as_scm(tag, attr, value);
    }

    return SCM_EOL;
  }

  SCM_DEFINE(get_id3v2_attribute, GET_ID3V2_ATTRIBUTE, 3, 0, 0,
             (SCM track, SCM index, SCM attr),
             "Retrieve an attribute of an ID3v2 tag")
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(index));

    SCM result;
    if (scm_is_eq(attr, sym_id3v2_version)) {
      result = scm_from_unsigned_integer(tag.version());
    }
    else if (scm_is_eq(attr, sym_id3v2_revision)) {
      result = scm_from_unsigned_integer(tag.revision());
    }
    else {
      // Will *not* return or file dtors!
      scm_misc_error(HAS_FRAME, "unknown attribute ~s", attr);
    }

    return result;
  }

  /**
   * \brief Create a new ID3v2 tag
   *
   *
   * \param track [in] reference to a scribbu/track instance in which the new
   * tag will be created
   *
   * \param index [in] zero-based index after which the ID3v2 tag shall be
   * created
   *
   * \param rest [in] keyword parameters parameterizing the new tag, on which
   * more below
   *
   *
   * Add a new ID3v2 tag to a track instance:
   *
   *   - padding: # of bytes of padding to be included in the tag
   *
   *   - experimental: boolean representing the "experimental" bit in the header
   *
   *   - extended-header: boolean indicating whether or not the tag shall have an
   *     extended header
   *
   *   - checksum: boolean indicating whether or not the tag shall include a
   *     checksum as part of the header
   *
   *
   * \note This method will create an ID3v2.3 tag
   *
   * \todo Support other revisions?
   *
   *
   */

  SCM_DEFINE(make_id3v2_tag, MAKE_ID3V2_TAG, 2, 0, 1,
             (SCM track, SCM index, SCM rest),
             "Create a new ID3v2 tag")
  {
    using namespace std;
    using namespace scribbu;

    SCM kwpad, kwexp, kwext, kwcrc;
    kwpad = kwexp = kwext = kwcrc = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments(MAKE_ID3V2_TAG, rest,
                                 (scm_t_keyword_arguments_flags)0,
                                 kw_padding, &kwpad,
                                 kw_experimental, &kwexp,
                                 kw_ext_header, &kwext,
                                 kw_crc, &kwcrc,
                                 SCM_UNDEFINED);

    size_t cbpad = 0;
    bool fexp = false;
    id3v2_3_tag::want_extended_header ext = id3v2_3_tag::want_extended_header::none;

    if (!SCM_UNBNDP(kwpad)) {
      cbpad = scm_to_uint(kwpad);
    }
    if (!SCM_UNBNDP(kwexp)) {
      fexp = scm_to_bool(kwexp);
    }
    if (!SCM_UNBNDP(kwcrc)) {
      ext = id3v2_3_tag::want_extended_header::with_crc;
    }
    else if (!SCM_UNBNDP(kwext)) {
      ext = id3v2_3_tag::want_extended_header::present;
    }

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    unique_ptr<id3v2_tag> ptag(new id3v2_3_tag(cbpad, fexp, ext));
    ptrack->insert_id3v2_tag(ptag, scm_to_uint(index));

    return SCM_EOL;

  } // End free function make_id3v2_tag.

  /**
   * \brief Write an ID3v2 tag to file (scribbu/write-id3v2-tag)
   *
   *
   * \param track [in] reference to a scribbu/track instance containing the tag
   * to be dumped
   *
   * \param index [in] zero-based index of the ID3v2 tag to be dumped
   *
   * \param out [in] path to which the tag shall be written
   *
   * \return the number of bytes that were written
   *
   *
   * This method implements the scheme function scribbu/write-id3v2-tag.
   *
   *
   */

  SCM_DEFINE(write_id3v2_tag, WRITE_ID3V2_TAG, 3, 0, 0,
             (SCM track, SCM index, SCM out),
             "Write an ID3v2 tag to file")
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    size_t idx = scm_to_uint(index);

    dynwind_context ctx;

    char *pth = scm_to_locale_string(out);
    ctx.free(pth);

    fs::ofstream ofs(pth, fs::ofstream::binary);
    size_t cb = ptrack->write_id3v2_tag(ofs, idx);

    return scm_from_uint(cb);

  } // End free function write_id3v2_tag.

  SCM_DEFINE(replace_tags, REPLACE_TAGS, 2, 0, 0,
             (SCM track, SCM out),
             "Replace the tags in a file")
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    dynwind_context ctx;
    char *pth = ctx.free_locale_string(out);

    ptrack->replace_tags(pth);

    return SCM_EOL;
  }
  
  SCM_DEFINE(write_track, WRITE_TRACK, 2, 0, 0,
             (SCM track, SCM out),
             "Write a track to file")
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    dynwind_context ctx;
    char *pth = ctx.free_locale_string(out);

    ptrack->write(pth);

    return SCM_EOL;
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


  // Initializae the Guile interpreter
  void*
  initialize_guile(void*)
  {
    init_track_type();

#   ifndef SCM_MAGIC_SNARFER
#   include "scheme.x"
#   endif

    customize_welcome();

    return 0;
  }

}
