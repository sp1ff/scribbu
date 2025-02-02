/**
 * \file pprinter.hh
 *
 * Copyright (C) 2015-2024 Michael Herstine <sp1ff@pobox.com>
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

#ifndef PPRINTER_HH_INCLUDED
#define PPRINTER_HH_INCLUDED 1
/**
 * \page scribbu_pprinter Pretty Printing
 *
 * Pretty-printing ID3v2 tags & frames
 *
 * \section scribbu_pprinter_intro Introduction
 *
 * Package for printing tags & frames to standard output streams.
 *
 * \section scribbu_pprinter_discuss Discussion
 *
 * \subsection scribbu_pprinter_discuss_problem The Problem
 *
 * I would like to implement operator<< for scribbu::id3v2_tag-s (and for other
 * things, but scribbu::id3v2_tag presents the most challenging
 * problem). scribbu::id3v2_tag has three subclasses, each of which contains a
 * polymorphic collection of frames. So far, this could be solved through a
 * virtual function on scribbu::id3v2_tag & on each frame base class
 * (scribbu::id3v2_2_frame, scribbu::id3v2_3_frame &
 * scribbu::id3v2_4_frame). However, I would also like to be able to support
 * different styles of pretty-printing (compact, standard, detailed, &c).
 *
 * My first approach was to go the virtual function route, and simply
 * dispatch based on the selected style, something like this:
 *
 \code
   class thing... {
       ...
       virtual void print_on(ostream&os) {
           ...
           format fmt = get_format(os);
           if (format::compact == fmt) {
               // compact code here
           }
           else if (format::standard == fmt) {
               // standard code here
           }
           // and so forth...
 \endcode
 *
 * It quickly became clear that this would become a mess; every time I wanted
 * to add a new format I would need to remember, and touch, every tag & frame
 * pretty-print implementation. Furthermore, each of those functions would
 * become ever more complex as I added more styles of pretty-printing.
 *
 * \subsection scribbu_pprinter_discuss_visitor Visitor
 *
 * My next attempt was to implement Visitor \ref scribbu_pprinter_ref_01 "[1]",
 * which is of course tailor-made for this: I had a set of types (the ID3 tag &
 * frame classes) which was unlikely to change, and a set of operations (print
 * in standard fashion, in compact, CSV, &c) that was much more fluid. Each
 * style of pretty-printing would be implemented as a distinct class,
 * eliminating the problems described above. The cost would be adding an
 * "accept" virtual to each tag & frame type, which would then invoke the
 * relevant visitor method (for two virtual function lookups). There was one
 * wrinkle where frames were involved: employing Visitor in this way would mean:
 *
 *    1. operator<< constructing a Visitor of the appropriate type and invoking
 *       accept on the tag, passing the Visitor as an argument (one virtual
 *       lookup)
 *
 *    2. in the implementation, we would now have resolved the tag type (since
 *       accept would be virtual), and so we could call the appropriate Visitor
 *       virtual, passing \c *this as an argument (a second virtual lookup)
 *
 *    3. at this point, we're in the Visitor, and we've resolved both types; we
 *       can pretty-print the tag, _but_ that tag has a polymorphic collection
 *       of frames- we call accept on each tag (virtual lookup for each frame)
 *
 *    4. in the frame's accept implementation, we now have the frame
 *       type, so we can call the appropriate Visitor virtual, passing
 *       \c *this as an argument (a second virtual lookup for each frame)
 *
 *    5. in the Visitor overload, we've now resolved both the frame type and
 *       the Visitor type & can pretty-print the frame... but this second
 *       virtual invocation seemed superfluous to me, since we had already
 *       figured out the Visitor type in step 2.
 *
 * Of course, Visitor also introduced the usual dependency problems: id3v_2_tag
 * depended on \c visitor, each concrete \c visitor subclass dependended
 * on all of id3v2_2_tag, id3v2_3_tag and id3v2_4_tag, each of which
 * in turn depended on id3v2_tag.
 *
 * Moving to Acyclic Visitor \ref scribbu_pprinter_ref_02 "[2]" would remove
 * that, in the process substituting the virtual function invocation in 2 for a
 * dynamic cast. It did nothing for the superfluous virtual invocation for each
 * frame.
 *
 * \subsection scribbu_pprinter_discuss_ddispatch Dual Dispatch
 *
 * I decided to look for something a little cleaner; something that would
 * preserve the benefits of Acyclic Visitor (easily adding new styles of
 * pretty-printing without having to touch extant code) while eliminating some
 * of the drawbacks (\em 2*nframes virtual function invocations for each frame
 * plus one virtual invocation & one dynamic cast; adding special-purpose
 * members like "accept" & most of all the circular dependencies).
 *
 * It occurred to me that Visitor was really just a solution to the
 * dual-dispatch problem; the two virtual function calls are just resolving the
 * two types in serial. So I dusted off \ref ref_01 "Alexandrescu", who sketches
 * out a general-purpose dual dispatch facility based on a map of \c type_info
 * pairs (i.e. the types on which we would like to dispatch) to function
 * pointers (the corresponding implementations).
 *
 * As I started laying out my implementation, however, I realized I could
 * simplify it significantly for my particular case. I adapted Alexandrescu's
 * implementation by mapping tag/frame type to a pointer-to-virtual-member on
 * the *base* pretty-printer (imaginatively named pprinter). Class
 * pprint_dispatcher maintains four hash tables:
 *
 \code

  typedef std::ostream&
  (*tag_dispatch_type)(pprinter*, const scribbu::id3v2_tag&, std::ostream&);

  typedef std::ostream&
  (*frame_v22_dispatch_type)(pprinter*, const scribbu::id3v2_2_frame&, std::ostream&);

  typedef std::ostream&
  (*frame_v23_dispatch_type)(pprinter*, const scribbu::id3v2_3_frame&, std::ostream&);

  typedef std::ostream&
  (*frame_v24_dispatch_type)(pprinter*, const scribbu::id3v2_4_frame&, std::ostream&);

  std::unordered_map<std::type_index, tag_dispatch_type> tags_;
  std::unordered_map<std::type_index, frame_v22_dispatch_type> v22_frames_;
  std::unordered_map<std::type_index, frame_v23_dispatch_type> v23_frames_;
  std::unordered_map<std::type_index, frame_v24_dispatch_type> v24_frames_;

 \endcode
 *
 * \c tags_ maps id3v2_tag concrete sub-class types to pointers to trampoline
 * functions (see \ref ref_01 "Alexandrescu") which dynamic cast their second
 * parameters to the correct type, and invoke the virutal through their first
 * parameter. So if I have an id3v2_tag that I want to pretty-print to an output
 * stream, I just do this:
 *
 \code

 ostream& operator<<(ostream &os, const id3v2_tag &tag)
 {
    pprinter *pp = // get my pretty printer, somehow
    const pprint_dispatcher &D = // ...
    return D.tag_impl(typeid(tag))(pp, tag, os);
  }

 \endcode
 *
 * Similarly for the other three hash tables.
 *
 * There are no efficiency gains here, since the trampoline still needs to carry
 * out a dynamic cast & a virtual function invocation (I suppose I've traded one
 * virtual function invocation for a dynamic cast, but I see no clear
 * advantage to that).  However, this approach accomplishes the following:
 *
 *   1. it preserves the \ref ref_03 "Open/Closed Principle", unlike my first,
 *      naive implementation (but like both Visitor variants)
 *
 *   2. it breaks up the dependency cycle that Visitor entails
 *
 *   3. it captures (and checks at compile-time) the contract a new pretty-print
 *      implementation will have to satisfy (rather than simply writing them
 *      down in documentation & finding out at runtime that the author forgot
 *      one, as in Acyclic Visitor)
 *
 *   4. basing the implementation on pprinter virtuals allows me to populate the
 *      hash tables completely in-library, eliminating the need for any kind of
 *      registration or "glue" code between new pretty-print implementations &
 *      the extant framework
 *
 * \section scribbu_pprinter_refs References
 *
 * 1. \anchor scribbu_pprinter_ref_01 [1] Unknown, Visitor pattern
 *    https://en.wikipedia.org/wiki/Visitor_pattern (updated 2015)
 *
 * 2. \anchor scribbu_pprinter_ref_02 [2] Robert C. Martin: Acyclic Visitor
 *    http://condor.depaul.edu/dmumaugh/OOT/Design-Principles/acv.pdf (updated
 *    August 30, 2019)
 *
 * 3. \anchor scribbu_pprinter_ref_03 [3] Erich Gamma, Richard Helm, Ralph
 *    Johnson, John Vlissides, Design Patterns: Elements of Reusable
 *    Object-Oriented Software, Addison Wesley, Boston, 1994.
 *
 *
 */

#include <iostream>

#include <boost/optional/optional.hpp>

#include <scribbu/charsets.hh>

namespace scribbu {

  // All the things that can be pretty-printed
  class id3v2_tag;
  class id3v2_2_tag;
  class id3v2_3_tag;
  class id3v2_4_tag;
  class track_data;
  class id3v1_tag;
  class id3v2_2_frame;
  class unknown_id3v2_2_frame;
  class id3v2_2_text_frame;
  class UFI;
  class TXX;
  class COM;
  class CNT;
  class POP;
  class XTG;
  class id3v2_3_frame;
  class unknown_id3v2_3_frame;
  class id3v2_3_text_frame;
  class UFID;
  class ENCR;
  class TXXX;
  class COMM;
  class PCNT;
  class POPM;
  class XTAG;
  class PRIV;
  class id3v2_4_frame;
  class unknown_id3v2_4_frame;
  class id3v2_4_text_frame;
  class UFID_2_4;
  class ENCR_2_4;
  class TXXX_2_4;
  class COMM_2_4;
  class PCNT_2_4;
  class POPM_2_4;
  class XTAG_2_4;
  class PRIV_2_4;

  /// The interface to which pretty-printers shall conform
  struct pprinter
  {
    virtual std::ostream&
    pprint_v2_2_tag(const id3v2_2_tag&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_v2_3_tag(const id3v2_3_tag&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_v2_4_tag(const id3v2_4_tag&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_track_data(const track_data&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_v1_tag(const id3v1_tag&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &, std::ostream&) = 0;
    virtual std::ostream&
    pprint_id3v2_2_text_frame(const id3v2_2_text_frame&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_UFI(const UFI&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_TXX(const TXX&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_COM(const COM&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_CNT(const CNT&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_POP(const POP&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_XTG(const XTG&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_id3v2_3_text_frame(const id3v2_3_text_frame&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_UFID(const UFID&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_ENCR(const ENCR&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_TXXX(const TXXX&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_COMM(const COMM&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_PCNT(const PCNT&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_POPM(const POPM&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_XTAG(const XTAG&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_PRIV(const PRIV&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_id3v2_4_text_frame(const id3v2_4_text_frame&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_UFID_2_4(const UFID_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_ENCR_2_4(const ENCR_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_TXXX_2_4(const TXXX_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_COMM_2_4(const COMM_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_PCNT_2_4(const PCNT_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_POPM_2_4(const POPM_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_XTAG_2_4(const XTAG_2_4&, std::ostream&) = 0;
    virtual std::ostream&
    pprint_PRIV_2_4(const PRIV_2_4&, std::ostream&) = 0;

    virtual ~pprinter()
    { }
    virtual pprinter* clone() = 0;

    static
    std::tuple<scribbu::encoding, scribbu::on_no_encoding>
    encoding_from_stream(std::ostream &os);

    static
    unsigned int
    optional_to_uint(const boost::optional<bool> &x);
  };

  /**
   * \brief ABC for pretty-printer manipulators
   *
   *
   * The logic for "inserting" a pretty printer into an output stream is
   * essentially the same in all cases, so I've factored out the (rather
   * complex) logic here. For each concrete pretty printer, sublcass this class
   * to produce an ostream manipulator that will imbue the stream with an
   * instance of that pretty printer.
   *
   * For example, suppose we have a concrete pretty printer named
   * "cool_printer". Then we would imbue streams with cool_printer instances
   * thusly:
   *
   \code

     class print_cooly: public pprint_manipulator {
     public:
       print_cooly(...): pprint_manipulator(new cool_printer(...))
       { }
     };

     // later
     os << print_cooly(...) << tag1 << tag2 << endl;

   \endcode
   *
   * I can't find much in the Standard on requirements for user-defined
   * manipulators. Since this implementation works by inserting a UDT into the
   * target ostream, I'm abiding by the same rules as for function pprint,
   * above, with one exception: since I'm not actually inserting anything into
   * the underlying stream buffer, I'm dispensing with the sentry.
   *
   *
   */

  class pprint_manipulator
  {
  public:
    pprint_manipulator(pprinter *pp): pp_(pp)
    { }
    virtual ~pprint_manipulator();

    /// Retrieve the iword/pword index reserved for all pretty-printer
    /// manipulators
    static int index();

  private:
    pprint_manipulator(const pprint_manipulator&)            = delete;
    pprint_manipulator(pprint_manipulator&&)                 = delete;
    pprint_manipulator& operator=(const pprint_manipulator&) = delete;
    pprint_manipulator& operator=(pprint_manipulator&&)      = delete;

    friend std::ostream&
    operator<<(std::ostream &os, const pprint_manipulator &c);

    /// Retrieve our underlying pprinter; caller takes ownership
    pprinter* take() const;
    /// Imbue \a os with our pretty printer
    static void do_manip(const pprint_manipulator &manip, std::ostream &os);
    /// Callback to be registered with the stream; will handle copying
    /// & deleting the underlying pprinter
    static void callback(std::ios_base::event e, std::ios_base &ios, int idx);
    /// pprinter instances constructed by this manipulator; will be copied over
    /// to the stream (and eventually destroyed by `callback')
    mutable pprinter *pp_;

  }; // End class pprint_manipulator.

  std::ostream&
  operator<<(std::ostream &os, const pprint_manipulator &c);

  /**
   * \brief Stock pretty printer
   *
   *
   * If the target stream has not explicitly been imbued with a pretty printer,
   * this one shall be used. It can be explicitly configured via the
   * pretty_print stream manipulator, defined below.
   *
   *
   */

  class standard_pprinter: public pprinter {

  public:

    // Convenience typedef for specifying tag & frame internal encodings;
    // `none' means "detect/deduce automatically"
    typedef boost::optional<scribbu::encoding> encoding_type;

    // Some default settings
    static const std::size_t DEFAULT_INDENT       = 0U;
    static const bool        DEFAULT_EXPAND_GENRE = true;
    static const encoding    DEFAULT_V1_ENCODING;
    static const boost::optional<encoding> DEFAULT_V2_ENCODING;

  public:
    standard_pprinter(
      std::size_t                      indent       = DEFAULT_INDENT,
      bool                             expand_genre = DEFAULT_EXPAND_GENRE,
      const encoding_type             &v1enc        = DEFAULT_V1_ENCODING,
      const boost::optional<encoding> &v2enc        = DEFAULT_V2_ENCODING):
      indent_       (indent),
      expand_genre_ (expand_genre),
      v1enc_        (v1enc),
      v2enc_        (v2enc),
      sin_          (indent_, ' ')
    { }

  public:

    /**
     * \brief Insert an ID3v2.2 tag into a (narrow) output stream
     *
     *
     * The format used will be as follows:
     *
     \code

       ${INDENT}ID3v2.2: $ARTIST - $TITLE
       ${INDENT}$ALBUM( \(track $TRACK\)), $YEAR(
       ${INDENT}$CONTENT-TYPE)(
       ${INDENT}$ENCODED-BY)(
       ${INDENT}$COMMENT0
       ${INDENT}$COMMENT1
       ...)(
       ${INDENT}$GENRE)
       ${INDENT}N frames:
       ${INDENT}${INDENT}${FRAME0}
       ...
     \endcode
     *
     *
     */

    virtual std::ostream&
    pprint_v2_2_tag(const id3v2_2_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_3_tag(const id3v2_3_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_4_tag(const id3v2_4_tag&, std::ostream&);
    virtual std::ostream&
    pprint_track_data(const track_data&, std::ostream&);

    /**
     * \brief Insert an ID3v1 tag into a (narrow) output stream
     *
     *
     * The format used will be as follows:
     *
     \code

       ${INDENT}ID3v1(.1)( \(enhanced\)): $ARTIST - $TITLE
       ${INDENT}$ALBUM( \(track $TRACK\)), $YEAR
       ${INDENT}$COMMENT
       ${INDENT}$GENRE(
       ${INDENT}$START-$END: $SPEED)

     \endcode
     *
     * Where parentheses indicate fields that may or may not appear depending
     * on the tag, and $INDENT denotes a configurable number of spaces to be
     * prepended to each line.
     *
     *
     */

    virtual std::ostream&
    pprint_v1_tag(const id3v1_tag&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_2_text_frame(const id3v2_2_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFI(const UFI&, std::ostream&);
    virtual std::ostream&
    pprint_TXX(const TXX&, std::ostream&);
    virtual std::ostream&
    pprint_COM(const COM&, std::ostream&);
    virtual std::ostream&
    pprint_CNT(const CNT&, std::ostream&);
    virtual std::ostream&
    pprint_POP(const POP&, std::ostream&);
    virtual std::ostream&
    pprint_XTG(const XTG&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_3_text_frame(const id3v2_3_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID(const UFID&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR(const ENCR&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX(const TXXX&, std::ostream&);
    virtual std::ostream&
    pprint_COMM(const COMM&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT(const PCNT&, std::ostream&);
    virtual std::ostream&
    pprint_POPM(const POPM&, std::ostream&);
    virtual std::ostream&
    pprint_XTAG(const XTAG&, std::ostream&);
    virtual std::ostream&
    pprint_PRIV(const PRIV&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_4_text_frame(const id3v2_4_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID_2_4(const UFID_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR_2_4(const ENCR_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX_2_4(const TXXX_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_COMM_2_4(const COMM_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT_2_4(const PCNT_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_POPM_2_4(const POPM_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_XTAG_2_4(const XTAG_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_PRIV_2_4(const PRIV_2_4&, std::ostream&);

    virtual pprinter* clone() {
      return new standard_pprinter(*this);
    }

  protected:
    // All the ID3v2 tags follow a similar idiom-- factor that out here
    void print_id3v2_tag(const id3v2_tag &tag, std::ostream &os);

  private:

    std::size_t               indent_;
    bool                      expand_genre_;
    boost::optional<encoding> v1enc_;
    boost::optional<encoding> v2enc_;
    std::string               sin_;          // pre-computed indent

  };

  struct pretty_print: public pprint_manipulator {
    pretty_print(
      std::size_t indent = standard_pprinter::DEFAULT_INDENT,
      bool expand_genre = standard_pprinter::DEFAULT_EXPAND_GENRE,
      const encoding &v1enc = standard_pprinter::DEFAULT_V1_ENCODING,
      const boost::optional<encoding> &v2enc =
        standard_pprinter::DEFAULT_V2_ENCODING):
      pprint_manipulator(new standard_pprinter(indent, expand_genre,
                                               v1enc, v2enc))
    { }
  };

  std::ostream& operator<<(std::ostream &os, const id3v2_tag &tag);
  std::ostream& operator<<(std::ostream &os, const track_data &data);
  std::ostream& operator<<(std::ostream &os, const id3v1_tag &tag);
  std::ostream& operator<<(std::ostream &os, const id3v2_2_frame &frame);
  std::ostream& operator<<(std::ostream &os, const id3v2_3_frame &frame);
  std::ostream& operator<<(std::ostream &os, const id3v2_4_frame &frame);

}

#endif // not PPRINTER_HH_INCLUDED
