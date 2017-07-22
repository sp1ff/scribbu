#ifndef ID3V2_UTILS_HH_INCLUDED
#define ID3V2_UTILS_HH_INCLUDED 1
#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/tbt-support.hh>

#include <memory>

namespace scribbu {

  /**
   * \brief Attempt to read an ID3v2 tag from is
   *
   *
   * \param is [in] An input stream from which the ID3v2 tag shall be read
   *
   * \return An ID3v2 tag, typed as pointer to id3v2_tag
   *
   *
   * This function shall:
   *
   * - read the first ten bytes from \a is; if it's not an ID3v2 tag, restore
   *   the get ptr & so indicate to the caller (by throwing)
   *
   * - if it *is*, construct an instance of the appropriate type and return it
   *   as a ptr to id3v2_tag.
   *
   *
   */

  std::unique_ptr<id3v2_tag> maybe_read_id3v2(std::istream &is);

  std::unique_ptr<id3v2_tag> read_id3v2(std::istream &is, std::size_t idx);
  
  /**
   * \brief Read all ID3v2 tags from an input stream
   *
   *
   * \param is [in] An input stream from which ID3v2 tags shall be read
   *
   * \param p [in,out] A forward output iterator to which unique_ptr-s to
   * id3v2_tag-s shall be copied
   *
   * \return p, after copy
   *
   *
   * There may be multiple ID3v2 tags at the start of a file; use this
   * method to read all possible & copy pointers to the resulting tags.
   *
   *
   */

  template <typename forward_output_iterator>
  forward_output_iterator read_all_id3v2(std::istream           &is,
                                         forward_output_iterator p) {
    try {
      std::unique_ptr<id3v2_tag> ptag = maybe_read_id3v2(is);
      while (ptag) {
        *p = std::move(ptag); p++;
        ptag = maybe_read_id3v2(is);
      }
    }
    catch (const std::exception&) {
    }

    return p;
  }

  /**
   * \class processor
   *
   * \brief Functor for evaluating tag-based templates
   *
   *
   * I want to build a templating system where the template parameters are
   * filled in based on ID3v{1,2} tags, but I'm not sure what I want, yet.
   *
   * Examples:
   *
   * 1. "%A - %T%E"-- produce strings of the form "$artist -
   * $title$extension"; both tags are rendered verbatim in UTF-8, and the tag
   * value shall be determined by referring to the ID3v2 tag first, if present,
   * falling back to ID3v1 if not, and failing otherwise. Note that %E is the
   * original file extension (including the dot), and is *not* derived from any
   * tag.
   *
   * 2. "%(artist:encoding=cp1252&compress&sourcing=preferv1) -
   * %(title)(%extension:default=.mp3)"-- produce strings of the form "$artist
   * - $title$extension". The title shall be rendered in CP1252, with
   * whitespace compressed, and the ID3v1 tag shall be prefered to the
   * ID3v2. If the source file has no extension, ".mp3" shall be used.
   *
   * Options I can imagine:
   *
   * - compress whitespace
   * - different encodings
   * - different sourcing behaviour (e.g. prefer the ID3v1, or just fail if
   *   ID3v2 not present)
   * - provide a default value if the tag can't be evaluated
   *
   * 3. "%A - %T( \(track #%(track)\))?.mp3"-- produce strings of the form "$artist
   * - $title (track #$track).mp3" when the track number is available, falling
   * back to just "$artist - $title.mp3" else
   *
   * I wanted to use boost::spirit for this, but I spent the better part of
   * three days trying to figure out how the heck it worked. The docs, written
   * in form of "type X, type Y, type Z-- there, now it works!" were useless.
   *
   * EBNF:
   <code>

   template := term *

   term := text | replacement

   replacement := %(repl-name(:format)?) | %repl-abbrev | (term *)'?'

   repl-name := "artist" | "title" |...

   repl-abbrev := A | T |...

   format := option (&option) *

   option := ...

   </code>
   *
   *
   */

  class template_processor
  {
  public:
    /// Construct with the template in textual form
    template_processor(const std::string &templat);
    /// Given the path of the file, process our template
    std::string operator()(const boost::filesystem::path &pth) const;

  private:
    std::vector<std::shared_ptr<scribbu::tbt_support::term>> terms_;

  };

} // End namespace scribbu.


#endif // not ID3V2_UTILS_HH_INCLUDED
