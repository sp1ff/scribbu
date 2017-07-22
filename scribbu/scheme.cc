#include "scheme.hh"

#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2-edit.hh>

#include <string>

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/functional/hash.hpp>

namespace fs = boost::filesystem;


///////////////////////////////////////////////////////////////////////////////
//                              utility code                                 //
///////////////////////////////////////////////////////////////////////////////

namespace std {

  template <>
  struct hash<char*>
  {
    std::size_t operator()(char *p) const;
  };

}

//template<>
std::size_t std::hash<char*>::operator()(char *p) const
{
  // https://stackoverflow.com/questions/2590677/how-do-i-combine-hash-values-in-c0x
  std::size_t seed = 0x9e3779b9;

  while (*p) {
    boost::hash_combine(seed, *p++);
  }

  return seed;
}

scribbu::encoding
encoding_for_string(const char *p)
{
  std::hash<const char*> H;
  std::size_t H001 = H("ASCII");
  std::size_t H002 = H("ISO_8859_1");
  std::size_t H003 = H("ISO_8859_2");
  std::size_t H004 = H("ISO_8859_3");
  std::size_t H005 = H("ISO_8859_4");
  std::size_t H006 = H("ISO_8859_5");
  std::size_t H007 = H("ISO_8859_7");
  std::size_t H008 = H("ISO_8859_9");
  std::size_t H009 = H("ISO_8859_10");
  std::size_t H000 = H("ISO_8859_13");
  std::size_t H011 = H("ISO_8859_14");
  std::size_t H012 = H("ISO_8859_15");
  std::size_t H013 = H("ISO_8859_16");
  std::size_t H014 = H("KOI8_R");
  std::size_t H015 = H("KOI8_U");
  std::size_t H016 = H("KOI8_RU");
  std::size_t H017 = H("CP1250");
  std::size_t H018 = H("CP1251");
  std::size_t H019 = H("CP1252");
  std::size_t H020 = H("CP1253");
  std::size_t H021 = H("CP1254");
  std::size_t H022 = H("CP1257");
  std::size_t H023 = H("CP850");
  std::size_t H024 = H("CP866");
  std::size_t H025 = H("CP1131");
  std::size_t H026 = H("MacRoman");
  std::size_t H027 = H("MacCentralEurope");
  std::size_t H028 = H("MacIceland");
  std::size_t H029 = H("MacCroatian");
  std::size_t H030 = H("MacRomania");
  std::size_t H031 = H("MacCyrillic");
  std::size_t H032 = H("MacUkraine");
  std::size_t H033 = H("MacGreek");
  std::size_t H034 = H("MacTurkish");
  std::size_t H035 = H("Macintosh");
  return scribbu::encoding::ASCII;
}


///////////////////////////////////////////////////////////////////////////////
//                        Scheme foreign type `track'                        //
///////////////////////////////////////////////////////////////////////////////

// https://www.gnu.org/software/guile/manual/guile.html#Defining-Foreign-Object-Types

class scm_track
{
public:
  scm_track(const fs::path &pth);

public:
  fs::path path() const 
  {
    return pth_;
  }
  scribbu::id3v1_tag* id3v1_tag() const 
  {
    return pv1_.get();
  }
  std::size_t num_id3v2_tags() const 
  {
    return v2_.size();
  }
  const scribbu::id3v2_tag&
  get_id3v2_tag(unsigned int idx) const
  {
    return *v2_[idx];
  }

private:
  fs::path pth_;
  std::vector<std::unique_ptr<scribbu::id3v2_tag>> v2_;
  std::unique_ptr<scribbu::track_data> ptd_;
  std::unique_ptr<scribbu::id3v1_tag> pv1_;

};

scm_track::scm_track(const fs::path &pth):
  pth_(pth)
{
  fs::ifstream ifs(pth_, fs::ifstream::binary);

  scribbu::read_all_id3v2(ifs, back_inserter(v2_));
  ptd_.reset(new scribbu::track_data(ifs));
  pv1_ = scribbu::process_id3v1(ifs);
}

namespace {

  SCM track_type; // initialized in init_track_type

  SCM
  track_for_path(const fs::path &pth)
  {
    return scm_make_foreign_object_1(track_type, new scm_track(pth));
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

  SCM
  make_track(SCM pth)
  {
    scm_dynwind_begin((scm_t_dynwind_flags)0);

    char *ppth = scm_to_locale_string(pth);
    scm_dynwind_free(ppth);

    SCM scm = track_for_path(fs::path(ppth));

    scm_dynwind_end();

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

  /////////////////////////////////////////////////////////////////////////////
  //                              Guile constants                            //
  /////////////////////////////////////////////////////////////////////////////

  const char * const MODULE = "scribbu";

  const char * const WITH_TRACK_IN    = "scribbu/with-track-in";
  const char * const MAKE_TRACK       = "scribbu/make-track";
  const char * const GET_PATH         = "scribbu/get-path";
  const char * const HAS_ID3V1_TAG    = "scribbu/has-id3v1-tag";
  const char * const GET_ID3V1_STRING = "scribbu/get-id3v1-string";
  const char * const NUM_ID3V2_TAGS   = "scribbu/get-id3v2-tag-count";
  const char * const HAS_ID3V2_ATTR   = "scribbu/has-id3v2-attribute";
  const char * const SET_TEXT_FRAME   = "scribbu/set-text-frame";
  
  SCM kw_src_encoding;
  SCM kw_dst_encoding;

  SCM sym_album;
  SCM sym_artist;
  SCM sym_comment;
  SCM sym_content_type;
  SCM sym_encoded_by;
  SCM sym_title;
  SCM sym_year;

}

extern "C" {

  SCM
  get_path(SCM track)
  {
    scm_track *p = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_locale_string(p->path().c_str());
  }

  SCM
  has_id3v1_tag(SCM track)
  {
    scm_track *p = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_bool(0 != p->id3v1_tag());
  }

  SCM
  get_id3v1_string(SCM track, SCM item, SCM rest)
  {
    using namespace std;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    // TODO(sp1ff): Turn this into a guard object?
    scm_dynwind_begin((scm_t_dynwind_flags)0);

    char *pitem = scm_to_locale_string(scm_symbol_to_string(item));
    scm_dynwind_free(pitem);

    SCM src_encoding = SCM_UNDEFINED;
    SCM dst_encoding = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments("scribbu/get-id3v1-string", rest, 
                                 (scm_t_keyword_arguments_flags)0,
                                 kw_src_encoding, &src_encoding,
                                 kw_dst_encoding, &dst_encoding,
                                 SCM_UNDEFINED);
    scribbu::encoding src = scribbu::encoding::CP1252;
    if (!SCM_UNBNDP(src_encoding)) {
      char *p = scm_to_locale_string(src_encoding);
      scm_dynwind_free(p);
      stringstream stm(p);
      stm >> src;
    }

    scribbu::encoding dst = scribbu::encoding::UTF_8;
    if (!SCM_UNBNDP(src_encoding)) {
      char *p = scm_to_locale_string(dst_encoding);
      scm_dynwind_free(p);
      stringstream stm(p);
      stm >> dst;
    }

    const scribbu::id3v1_tag *ptag = ptrack->id3v1_tag();
    if (!ptag) {
      scm_misc_error("scribbu/get-id3v1-string", "no ID3v1 tag", SCM_ELISP_NIL);
    }

    size_t hsh = hash<char*>()(pitem);

    string text;
    if (0x29b554db91812ebf == hsh) {
      text = ptag->album<string>(src, dst);
    }
    else if (0x5e77d8c0755e0f24 == hsh) {
      text = ptag->artist<string>(src, dst);
    }
    else if (0xebe545bc68ef4629 == hsh) {
      text = ptag->comment<string>(src, dst);
    }
    else if (0x29b554c440d104f7 == hsh) {
      text = ptag->title<string>(src, dst);
    }
    else if (0xa3b7d469da3c99  == hsh) {
      text = ptag->year<string>(src, dst);
    }
    else {
      // Will *not* return or file dtors!
      scm_misc_error("scribbu/get-id3v1-string", "unknown attribute ~s", item);
    }
    
    scm_dynwind_end();

    return scm_from_stringn (text.c_str(), text.size(), "UTF-8",
                             SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
  }

  SCM
  num_id3v2_tags(SCM track)
  {
    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    return scm_from_int(ptrack->num_id3v2_tags());
  }


  SCM
  has_id3v2_attribute(SCM track, SCM tagidx, SCM attr)
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    const id3v2_tag& tag = ptrack->get_id3v2_tag(scm_to_uint(tagidx));

    scm_dynwind_begin((scm_t_dynwind_flags)0);

    char *pattr = scm_to_locale_string(scm_symbol_to_string(attr));
    scm_dynwind_free(pattr);

    size_t hsh = hash<char*>()(pattr);

    bool ok;
    if (0x29b554db91812ebf == hsh) {
      ok = tag.has_album();
    }
    else if (0x5e77d8c0755e0f24 == hsh) {
      ok = tag.has_artist();
    }
    else if (0xebaa22cb48c58626 == hsh) {
      ok = tag.has_content_type();
    }
    else if (0x47cc724f893044ce == hsh) {
      ok = tag.has_encoded_by();
    }
    else if (0x29b554c440d104f7 == hsh) {
      ok = tag.has_title();
    }
    else if (0xa3b7d469da3c99 == hsh) {
      ok = tag.has_year();
    }
    else {
      // Will *not* return or file dtors!
      scm_misc_error(HAS_ID3V2_ATTR, "unknown attribute ~s", attr);
    }
    
    scm_dynwind_end();

    return scm_from_bool(ok);      
  }

  /// Invoke a Scheme procedure for each entry in a directory tree
  SCM
  with_track_in(SCM dir, SCM fcn)
  {
    // TODO(sp1ff): I think this entire function is unsafe-- if `fcn' exits
    // non-locally, no dtors will fire.
    std::size_t len = 0;
    char * data = scm_to_stringn(dir, &len, "UTF-8", SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);

    std::string s(data, data + len);
    for (fs::recursive_directory_iterator p0(s), p1; p0 != p1; ++p0) {
      if (!fs::is_directory(*p0)) {
        scm_call_1(fcn, track_for_path(*p0));
      }
    }

    free(data);

    return SCM_EOF_VAL;
  }

  /// Add an ID3v2 text frame to an extant ID3v2 tag
  SCM
  set_text_frame(SCM track,
                 SCM index,
                 SCM outer_frame_id,
                 SCM outer_encoding,
                 SCM text,
                 SCM outer_replace)
  {
    // TODO(sp1ff): Hoist this higher
    static const bool DEF_REPLACE = false;

    using namespace std;
    using namespace scribbu;

    ///////////////////////////////////////////////////////////////////////////
    //                       unpack our parameters                           //
    ///////////////////////////////////////////////////////////////////////////

    bool replace = DEF_REPLACE;
    // `replace' is optional-- if the caller didn't specify, it will come in as
    // `SCM_UNDEFINED'.
    if (scm_is_bool(outer_replace)) {
      replace = scm_is_true(outer_replace);
    }

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    unsigned int idx = scm_to_uint(index);

    // We're about to start unpacking strings-- open a dynamic wind context
    scm_dynwind_begin((scm_t_dynwind_flags)0);

    char *pframe_id;
    if (scm_is_symbol(outer_frame_id)) {
      pframe_id = scm_to_locale_string(scm_symbol_to_string(outer_frame_id));
    }
    else {
      pframe_id = scm_to_locale_string(outer_frame_id);
    }
    scm_dynwind_free(pframe_id);

    char *pencoding = scm_to_locale_string(scm_symbol_to_string(outer_encoding));
    scm_dynwind_free(pencoding);

    char *ptext = scm_to_locale_string(text);
    scm_dynwind_free(ptext);

    ///////////////////////////////////////////////////////////////////////////
    //               coerce those parameters to internal types               //
    ///////////////////////////////////////////////////////////////////////////
    
    scribbu::encoding enc = encoding_for_string(pencoding);

    ///////////////////////////////////////////////////////////////////////////
    //                                 do it                                 //
    ///////////////////////////////////////////////////////////////////////////

    add_text_frame(ptrack->path(), idx, pframe_id, enc, ptext, replace);

    scm_dynwind_end();

    return SCM_UNDEFINED;
  }
  

  /// Register all scribbu-defined symbols
  static
  void
  register_symbols()
  {
    kw_src_encoding = scm_from_utf8_keyword("source-encoding");
    kw_dst_encoding = scm_from_utf8_keyword("destination-encoding");

    sym_album        = scm_from_utf8_keyword("album");
    sym_artist       = scm_from_utf8_keyword("artist");
    sym_comment      = scm_from_utf8_keyword("comment");
    sym_content_type = scm_from_utf8_keyword("conent-type");
    sym_encoded_by   = scm_from_utf8_keyword("encoded-by");
    sym_title        = scm_from_utf8_keyword("title");
    sym_year         = scm_from_utf8_keyword("year");
  }

  /// Regiser all scribbu-defined foreign functions
  static
  void
  register_functions ()
  {
    scm_c_define_gsubr(WITH_TRACK_IN,    2, 0, 0, (void*)&with_track_in);
    scm_c_define_gsubr(MAKE_TRACK,       1, 0, 0, (void*)&make_track);
    scm_c_define_gsubr(GET_PATH,         1, 0, 0, (void*)&get_path);
    scm_c_define_gsubr(HAS_ID3V1_TAG,    1, 0, 0, (void*)&has_id3v1_tag);
    scm_c_define_gsubr(GET_ID3V1_STRING, 2, 0, 1, (void*)&get_id3v1_string);
    scm_c_define_gsubr(NUM_ID3V2_TAGS,   1, 0, 0, (void*)&num_id3v2_tags);
    scm_c_define_gsubr(HAS_ID3V2_ATTR,   3, 0, 0, (void*)&has_id3v2_attribute);
    scm_c_define_gsubr(SET_TEXT_FRAME,   5, 1, 0, (void*)&set_text_frame);
  }

  /// Actual initialization routine-- meant to be called from within scm_c_define_module
  static
  void
  define_in_module(void*)
  {
    init_track_type();
    register_symbols();
    register_functions();

    // Export all procedures we want to be public:
    scm_c_export(WITH_TRACK_IN,    0);
    scm_c_export(MAKE_TRACK,       0);
    scm_c_export(GET_PATH,         0);
    scm_c_export(HAS_ID3V1_TAG,    0);
    scm_c_export(GET_ID3V1_STRING, 0);
    scm_c_export(NUM_ID3V2_TAGS,   0);
    scm_c_export(HAS_ID3V2_ATTR,   0);
    scm_c_export(SET_TEXT_FRAME,   0);
  }

  void* 
  initialize_guile(void*)
  {
    scm_c_define_module(MODULE, define_in_module, 0);
    scm_c_use_module(MODULE);
    return 0;
  }

}
