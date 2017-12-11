#include "scheme.hh"

#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v2-utils.hh>

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

class dynwind_context
{
public:
  dynwind_context(bool rewindable = false);
  ~dynwind_context()
  { scm_dynwind_end(); }
  void free(void *p)
  { scm_dynwind_free(p); }

  dynwind_context(const dynwind_context&)            = delete;
  dynwind_context(dynwind_context&&)                 = delete;
  dynwind_context& operator=(const dynwind_context&) = delete;
  dynwind_context& operator=(dynwind_context&&)      = delete;

};

dynwind_context::dynwind_context(bool rewindable /*= false*/)
{
  scm_dynwind_begin(
    (scm_t_dynwind_flags) (rewindable ? SCM_F_DYNWIND_REWINDABLE : 0));
}

/**
 * \brief Enumerated set of defined symbols
 *
 *
 * This module defines a number of Scheme symbols, which are sometimes passed
 * to C functions as variables of type SCM. For reasons I can't identify, I
 * can't define the symbols here in C (via scm_string_to_symbol, e.g.) and then
 * compare the input parameters to those pre-defined variables. The only way
 * I've been able to reliably identify them is to convert the incoming SCM
 * variable to a string & examine that.
 *
 *
 */

enum class symbol {
  album, artist, comment, content_type, encoded_by, title, year
};

std::unordered_map<std::size_t, symbol> symbols_;

symbol symbol_for_scm(SCM scm)
{
  dynwind_context ctx;

  char *pscm = scm_to_utf8_string(scm_symbol_to_string(scm));
  ctx.free(pscm);

  return symbols_[std::hash<char*>()(pscm)];

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
  scribbu::id3v2_tag&
  get_id3v2_tag(unsigned int idx)
  {
    return *v2_[idx];
  }
  void
  insert_id3v2_tag(std::unique_ptr<scribbu::id3v2_tag> &ptag, std::size_t idx)
  {
    v2_.insert(v2_.begin() + idx, std::move(ptag));
  }
  std::size_t write_id3v2(std::ostream &os, std::size_t idx) const
  {
    return v2_[idx]->write(os);
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
  const char * const HAS_FRAME        = "scribbu/has-frame";
  const char * const GET_ID3V2_ATTR   = "scribbu/get-id3v2-attribute";
  const char * const SET_ID3V2_ATTR   = "scribbu/set-id3v2-attribute";
  const char * const MAKE_ID3V2_TAG   = "scribbu/make-id3v2-tag";
  const char * const WRITE_ID3V2_TAG  = "scribbu/write-id3v2-tag";
  
  SCM kw_src_encoding;
  SCM kw_dst_encoding;
  SCM kw_padding;
  SCM kw_experimental;
  SCM kw_ext_header;
  SCM kw_crc;

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

    dynwind_context ctx;

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
      scm_misc_error("scribbu/get-id3v1-string", "no ID3v1 tag", SCM_ELISP_NIL);
    }

    string text;
    switch (symbol_for_scm(item)) {
    case symbol::album:
      text = ptag->album<string>(src, dst);
      break;
    case symbol::artist:
      text = ptag->artist<string>(src, dst);
      break;
    case symbol::comment:
      text = ptag->comment<string>(src, dst);
      break;
    case symbol::title:
      text = ptag->title<string>(src, dst);
      break;
    case symbol::year:
      text = ptag->year<string>(src, dst);
      break;
    default:
      // Will *not* return or file dtors!
      scm_misc_error("scribbu/get-id3v1-string", "unknown attribute ~s", item);
    }

    return scm_from_stringn(text.c_str(), text.size(), "UTF-8",
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

    bool ok;
    switch (symbol_for_scm(attr)) {
    case symbol::album:
      ok = tag.has_album();
      break;
    case symbol::artist:
      ok = tag.has_artist();
      break;
    case symbol::content_type:
      ok = tag.has_content_type();
      break;
    case symbol::encoded_by:
      ok = tag.has_encoded_by();
      break;
    case symbol::title:
      ok = tag.has_title();
      break;
    case symbol::year:
      ok = tag.has_year();
      break;
    default:
      // Will *not* return or file dtors!
      scm_misc_error(HAS_ID3V2_ATTR, "unknown attribute ~s", attr);
    }
    
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
  
  SCM
  has_frame(SCM track,
            SCM index,
            SCM frameid)
  {
    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    // TODO(sp1ff): Replace this with a mnemonic constant
    std::size_t idx = scm_to_uint32(index);
    const scribbu::id3v2_tag &tag = ptrack->get_id3v2_tag(idx);
    if (!scm_is_symbol(frameid)) {
      // NB. Will *not* return or fire dtors!
      scm_wrong_type_arg(HAS_FRAME, 3, frameid);
    }
    std::size_t result;
    if  (scm_eq_p(sym_album, frameid)) {
      result = tag.has_album();
    }
    else if (scm_eq_p(sym_artist, frameid)) {
      result = tag.has_artist();
    }
    else if (scm_eq_p(sym_content_type, frameid)) {
      result = tag.has_content_type();
    }
    else if (scm_eq_p(sym_encoded_by, frameid)) {
      result = tag.has_encoded_by();
    }
    else if (scm_eq_p(sym_title, frameid)) {
      result = tag.has_title();
    }
    else if (scm_eq_p(sym_year, frameid)) {
      result = tag.has_year();
    }
    else {
      // NB. Will *not* return or fire dtors!
      scm_misc_error(HAS_FRAME, "unrecognized symbol: ~s", frameid);
    }

    return scm_from_unsigned_integer(result);
  }

  SCM
  get_id3v2_attribute(SCM track, SCM tagidx, SCM attr)
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    size_t idx = scm_to_uint32(tagidx);

    const id3v2_tag& tag = ptrack->get_id3v2_tag(idx);

    string text;
    switch (symbol_for_scm(attr)) {
    case symbol::album:
      text = tag.album();
      break;
    case symbol::artist:
      text = tag.artist();
      break;
    case symbol::content_type:
      text = tag.content_type();
      break;
    case symbol::encoded_by:
      text = tag.encoded_by();
      break;
    case symbol::title:
      text = tag.title();
      break;
    case symbol::year:
      text = tag.year();
      break;
    default:
      // Will *not* return or file dtors!
      scm_misc_error(HAS_ID3V2_ATTR, "unknown attribute ~s", attr);
    }

    return scm_from_stringn(text.c_str(), text.size(), "UTF-8",
                            SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE);
  }

  /// Set an ID3v2 attribute
  SCM
  set_id3v2_attribute(SCM track,
                      SCM index,
                      SCM attr,
                      SCM value)
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);

    unsigned int idx = scm_to_uint32(index);

    id3v2_tag &tag = ptrack->get_id3v2_tag(idx);

    // We're about to start unpacking strings-- open a dynamic wind context
    dynwind_context ctx;

    char* pvalue = scm_to_utf8_stringn(value, NULL);
    ctx.free(pvalue);

    switch (symbol_for_scm(attr)) {
    case symbol::album:
      tag.album(pvalue);
      break;
    case symbol::artist:
      tag.artist(pvalue);
      break;
    case symbol::content_type:
      tag.content_type(pvalue);
      break;
    case symbol::encoded_by:
      tag.encoded_by(pvalue);
      break;
    case symbol::title:
      tag.title(pvalue);
      break;
    case symbol::year:
      tag.year(pvalue);
      break;
    default:
      // Will *not* return or file dtors!
      scm_misc_error(HAS_ID3V2_ATTR, "unknown attribute ~s", attr);
    }

    return SCM_UNDEFINED;
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

  SCM
  make_id3v2_tag(SCM track, SCM index, SCM rest)
  {
    using namespace std;
    using namespace scribbu;

    // TODO(sp1ff): Abstract this, somehow (mapping SCM keywords to C++
    // function invocations)...
    SCM kwpad, kwexp, kwext, kwcrc;
    kwpad = kwexp = kwext = kwcrc = SCM_UNDEFINED;
    scm_c_bind_keyword_arguments("scribbu/make_id3v2_tag", rest, 
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

    return SCM_UNDEFINED;

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

  SCM
  write_id3v2_tag(SCM track, SCM index, SCM out)
  {
    using namespace std;
    using namespace scribbu;

    scm_track *ptrack = (scm_track*) scm_foreign_object_ref(track, 0);
    size_t idx = scm_to_uint(index);

    dynwind_context ctx;

    char *pth = scm_to_locale_string(out);    
    ctx.free(pth);

    fs::ofstream ofs(pth, fs::ofstream::binary);
    size_t cb = ptrack->write_id3v2(ofs, idx);

    return scm_from_uint(cb);
    
  } // End free function write_id3v2_tag.

  /// Register all scribbu-defined symbols
  static
  void
  register_symbols()
  {
    kw_src_encoding = scm_from_utf8_keyword("source-encoding");
    kw_dst_encoding = scm_from_utf8_keyword("destination-encoding");
    kw_padding      = scm_from_utf8_keyword("padding");
    kw_experimental = scm_from_utf8_keyword("experimental");
    kw_ext_header   = scm_from_utf8_keyword("extended-header");
    kw_crc          = scm_from_utf8_keyword("checksum");

    // TODO(sp1ff): This works, but smells; there's *something* I'm not getting
    // about Guile symbols...
#   define DEFINE_SYMBOL(var, sym, enm)                                 \
    ( var ) = scm_string_to_symbol(scm_from_utf8_string(#sym));         \
    symbols_[std::hash<char*>()((char*)#sym)] = symbol::enm             \

    DEFINE_SYMBOL(sym_album,        album,        album);
    DEFINE_SYMBOL(sym_artist,       artist,       artist);
    DEFINE_SYMBOL(sym_comment,      comment,      comment);
    DEFINE_SYMBOL(sym_content_type, content-type, content_type);
    DEFINE_SYMBOL(sym_encoded_by,   encoded-by,   encoded_by);
    DEFINE_SYMBOL(sym_title,        title,        title);
    DEFINE_SYMBOL(sym_year,         year,         year);

  }

  /// Regiser all scribbu-defined foreign functions
  static
  void
  register_functions ()
  {
    scm_c_define_gsubr(WITH_TRACK_IN,    2, 0, 0, (void*)&with_track_in      );
    scm_c_define_gsubr(MAKE_TRACK,       1, 0, 0, (void*)&make_track         );
    scm_c_define_gsubr(GET_PATH,         1, 0, 0, (void*)&get_path           );
    scm_c_define_gsubr(HAS_ID3V1_TAG,    1, 0, 0, (void*)&has_id3v1_tag      );
    scm_c_define_gsubr(GET_ID3V1_STRING, 2, 0, 1, (void*)&get_id3v1_string   );
    scm_c_define_gsubr(NUM_ID3V2_TAGS,   1, 0, 0, (void*)&num_id3v2_tags     );
    scm_c_define_gsubr(HAS_ID3V2_ATTR,   3, 0, 0, (void*)&has_id3v2_attribute);
    scm_c_define_gsubr(HAS_FRAME,        3, 0, 0, (void*)&has_frame          );
    scm_c_define_gsubr(GET_ID3V2_ATTR,   3, 0, 0, (void*)&get_id3v2_attribute);
    scm_c_define_gsubr(SET_ID3V2_ATTR,   4, 0, 0, (void*)&set_id3v2_attribute);
    scm_c_define_gsubr(MAKE_ID3V2_TAG,   2, 0, 1, (void*)&make_id3v2_tag     );
    scm_c_define_gsubr(WRITE_ID3V2_TAG,  3, 0, 0, (void*)&write_id3v2_tag    );
  }

  /// Actual initialization routine-- meant to be called from within scm_c_define_module
  static
  void
  define_in_module(void*)
  {
    init_track_type();
    register_symbols();
    register_functions();

    // export all procedures we want to be public:
    scm_c_export(WITH_TRACK_IN,    0);
    scm_c_export(MAKE_TRACK,       0);
    scm_c_export(GET_PATH,         0);
    scm_c_export(HAS_ID3V1_TAG,    0);
    scm_c_export(GET_ID3V1_STRING, 0);
    scm_c_export(NUM_ID3V2_TAGS,   0);
    scm_c_export(HAS_ID3V2_ATTR,   0);
    scm_c_export(HAS_FRAME,        0);
    scm_c_export(GET_ID3V2_ATTR,   0);
    scm_c_export(SET_ID3V2_ATTR,   0);
    scm_c_export(MAKE_ID3V2_TAG,   0);
    scm_c_export(WRITE_ID3V2_TAG,  0);
  }

  // Initializae the Guile interpreter
  void* 
  initialize_guile(void*)
  {
    scm_c_define_module(MODULE, define_in_module, 0);
    scm_c_use_module(MODULE);
    return 0;
  }

}
