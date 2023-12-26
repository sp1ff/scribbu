/**
 * \file id3v2.hh
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

#ifndef ID3V2_HH_INCLUDED
#define ID3V2_HH_INCLUDED 1
/**
 * \page scribbu_id3v2 ID3v2 Tags
 *
 * \section scribbu_id3v2_intro Introduction
 *
 * The \ref scribbu_id3v1 "IDv1" tag had obvious limitations, leading to the
 * introduction of ID3v2 by Martin Nilsson & Michael Mutschler in 1998 \ref
 * scribbu_id3v2_refs_2 "[2]". Despite the name, it has almost nothing in
 * common with ID3v1 : "ID3v2 tags are of variable size, and usually occur at
 * the start of the file, to aid streaming media. They consist of a number of
 * frames, each of which contains a piece of metadata. For example, the TIT2
 * frame contains the title, and the WOAR frame contains the URL of the
 * artist's website. Frames can be up to 16MB in length, while total tag size
 * is limited to 256MB. The internationalization problem was solved by allowing
 * the encoding of strings not only in ISO-8859-1, but also in Unicode \ref
 * scribbu_id3v2_refs_3 "[3]""
 *
 * ``There are three versions of ID3v2:
 *
 * - ID3v2.2 was the first public version of ID3v2. It used three character
 *   frame identifiers rather than four (TT2 for the title instead of
 *   TIT2). Most of the common v2.3 and v2.4 frames have direct analogues in
 *   v2.2.  Now this standard is considered obsolete.
 *
 * - ID3v2.3 expanded the frame identifier to four characters, and added a
 *   number of frames.  A frame can contain multiple values, separated with a
 *   null byte. This is the most widely used version of ID3v2 tags.
 *
 * - ID3v2.4 is the latest version published, dated November 1, 2000. Notably,
 *   it allows textual data to be encoded in UTF-8, which was a common practice
 *   in earlier tags (despite the standard, since it was not supported yet)
 *   because it has several noticeable advantages over UTF-16. Another new
 *   feature allows the addition of a tag to the end of the file before other
 *   tags (like ID3v1).
 *
 * Windows Explorer and Windows Media Player cannot handle ID3v2.4 tags in any
 * version, up to and including Windows 10 / Windows Media Player 12. Windows
 * can understand ID3v2 up to and including version 2.3.'' \ref scribbu_id3v2_refs_3 "[3]"
 *
 * \section scribbu_id3v2_discuss Discussion
 *
 * \subsection scribbu_id3v2_discuss_header Header
 *
 * ID3v2 tags of any version begin with a ten byte header (throughout, '$'
 * denotes a hexadecimal quantity, and % a binary):
 *
 \verbatim

  | field                | representation | bytes |
  |----------------------+----------------+-------+
  | ID3/file identifier  | "ID3"          |     3 |
  | ID3 version/revision | $xx yy         |     2 |
  | ID3 flags            | %xxxxxxxx      |     1 |
  | ID3 size             | 4*%0xxxxxxx    |     4 |

 \endverbatim
 *
 * The flags are version-specific, and documented on their respective pages
 * (cf. scribbu::id3v2_2_tag, scribbu::id3v2_3_tag & scribbu::id3v2_4_tag).
 * The size is expressed as a four-byte sync-safe (see \ref
 * scribbu_id3v2_unsync "below") unsigned integer (28 bits, in other words).
 * It is the number of bytes in the tag \em after any unsynchronisation has
 * been applied, including padding, excluding the header. Expressed
 * differently, it is the size of the tag once serialized to disk, less the
 * header. Expressed yet another way, it is the number of bytes remaining to be
 * read by a reader after the ten-byte header.
 *
 *
 * \section scribbu_id3v2_unsync Unsynchorisation
 *
 * MPEG decoding software uses a two-byte sentinel value in the input stream to
 * detect the beginning of the audio. MPEG decoding software that is not
 * ID3-aware could mistakenly interpret that value as the beginning of the
 * audio should it happen to occur in the ID3v2 tag. Unsynchronisation is an
 * optional encoding scheme for the ID3v2 header to prevent
 * that. "Unsynchronisation may only be made with MPEG 2 layer I, II and III
 * and MPEG 2.5 files."  \ref scribbu_id3v2_refs_1 "[1]"
 *
 * More specifically, whenever a two byte combination of the form:
 *
 *   %11111111 111xxxxx (i.e. $FF $Ex or $FF Fx)
 *
 * is encountered in an ID3v2 tag to be written to disk, it is replaced with:
 *
 *   %11111111 00000000 111xxxxx
 *
 * and the 'unsynchronisation' flag will be set.
 *
 * This leaves us with an ambiguous situation on read: if we encounter
 * a bit pattern
 *
 *   %11111111 00000000 111xxxxx
 *
 * when reading a tag with the unsynchronisation flag set, we have no way to
 * know whether that was a false sync that was unsynchronised (and so the three
 * bytes should be interpreted as %11111111 111xxxxx) or whether those three
 * bytes had occurred naturally in the tag when it was written. To resolve this,
 * on applying unsynchronisation all two-byte sequences of the form $FF 00
 * should also be written as $FF 00 00 (in other words, on deserialization all
 * two-byte sequences of the form $FF 00 can just be interpreted as $FF).
 *
 * In ID3v2.2, deserialization is straightforward. The ten byte header is
 * "sync-safe" by definition, so it can be read without any additional
 * interpretation, and if the unsynchronisation flag is set, all two-byte
 * sequences of the form $FF 00 should be interpreted as just $FF.
 *
 * ID2v2.3 introduced an extended header, which is *not* sync-safe, and
 * padding, which *is* (padding bytes must always be $00). In section 3.2
 * \ref scribbu_id3v2_refs_4 "[4]" the specification explicitly notes that the
 * extended header \em is subject to unsynchronisation. Therefore, in the end,
 * ID3v2.3 tags are deserialized in the same way as ID3v2.2: once the header is
 * read, all two-byte sequences of the form $FF 00 should again be interpreted
 * as just $FF.
 *
 * ID3v2.4 introduced a different extended header and a footer, both of which
 * *are* sync-safe, and frames can be unsynchronised individually. The
 * unsynchronisation flag in the header being set indicates that all frames are
 * unsynchronised; unset in the header means that at least one frame is *not*
 * unsynchronised.
 *
 * It is important to note that tag components (headers, frames, padding &c)
 * can be considered independently for purposes of unsynchronisation (with one
 * exception, on which more below). An ID3v2 header may never contain false
 * syncs and may never \em introduce a false sync, since its last byte may
 * never have its high bit set. An ID3v2 tag may contains false syncs within
 * itself, but may never \em complete a false sync introduced by a preceding
 * component, since the frame header begins with an ASCII character (which
 * may not be zero nor have its high bit set).
 *
 * An ID3v2.3 extended header may contain false syncs within itself, but will
 * never complete a false sync (since it follws an ID3v2 header) and may never
 * introduce a false sync, since it must be followed by a frame (compliant tags
 * must contain at least one frame). ID3v2.2 defines no extended header and
 * ID3v2.4's header & footer are sync-safe.
 *
 * The one exception to this is when the last frame in a tag has a final byte of
 * 0xff. If there is no padding (or footer, in the case of ID3v2.4) this is a
 * false sync in ID3v2.3+. If there is padding, it will introduce a 0xff 0x00
 * pattern which must be written as 0xff 0x00 0x00 if unsynchronisation is being
 * applied.
 *
 * This might suggest handling unsynchronisation at a tag level (e.g. when
 * serializing a frame, have all components write themselves to a temporary
 * buffer without applying the unsynchronisation scheme, then applying it in
 * one fell swoop), but this is unattractive for two reasons:
 *
 *   - ID3v2.4 applies the usnychronisation scheme on a frame-by-frame basis
 *   - ID3v2.3+ frames can be encrypted and/or compressed, and ID3v2.2 supports
 *     an encrypted frame type-- in all cases this suggests that they will be
 *     caching representations of themselves in various forms; I don't want to
 *     cache individual frames \em and entire tags
 *
 * Perhaps I'll reconsider this in the future, but at this time scribbu tags
 * write themselves component-by-component, applying unsynchronisation (if
 * requested) as they go.
 *
 *
 * \section scribbu_impl_notes Implementation Notes
 *
 * \subsection scribbu_impl_notes_iterators Frame Iterators
 *
 * I originally conceived of & implemented ID3v2 tags & frames as immutable
 * copies of whatever was read off disk on construction. As the project
 * developed, it became clear that this was not going to be sufficient and that
 * the library would need to provide some way of editing frames, as well.
 *
 * Consequently, I've tried to make the ID3v2 tag classes behave like a
 * standard-compliant container for frames... except that the frames are
 * polymorphic. There are also a lot of ancillary datastructures that need to
 * be updated when the collection of frames changes. This has been an
 * interesting design problem & I wanted to note a few of my thoughts here.
 *
 * \subsubsection scribbu_impl_notes_iterators_proxies Proxy Iterators
 *
 * I can't simply return pointers and/or references to the ID3v2 frames
 * contained in my tag classes because they have a number of ancillary
 * datastructures that need to be kept in-sync with the basic collection of
 * frames. This suggests an iterator that dereferences to a proxy for a frame;
 * the proxy will do the appropriate things when changed in any way. Even
 * better, I have an example from which to work: std::vector<bool> (in
 * bits/stl_bvector.h). It turns out, however, that such containers have a long
 * & checkered history in C++:
 *
 * ``The vector<bool> specialization was intentionally put into the standard to
 * provide an example of how to write a proxied container...This would all have
 * been well and good, except that the container and iterator requirements were
 * never updated to allow for proxied containers. In fact, proxied containers
 * are categorically disallowed; a container<T>::reference must be a true
 * reference (T&), never a proxy. Iterators have similar requirements;
 * dereferencing a forward, bidirectional, or random-access iterator must yield
 * a true reference (T&), never a proxy. These points preclude any proxy-based
 * containers from meeting the standard container requirements.''
 * \ref scribbu_id3v2_refs_5 "[5]"
 *
 * The problem seems to be that allowing a proxied container makes meeting
 * complexity guarantees a lot tougher:
 *
 * ``both the original STL's container requirements and the C++ standard's
 * container requirements are based on the implicit assumption (among others)
 * that dereferencing an iterator both is a constant-time operation and
 * requires negligible time compared to other operations. As James Kanze
 * correctly pointed out on the comp.std.c++ newsgroup a couple of years
 * ago, neither of these assumptions is true for a disk-based container or a
 * packed-representation container.'' \ref scribbu_id3v2_refs_5 "[5]"
 *
 * From his article, it's not clear to me whether there is any other problem
 * beyond that (e.g. do any of the standard algorithms take the address of
 * the thing returned by dereferencing an iterator?):
 *
 * ``Proxied collections are a useful tool and can often be appropriate,
 * especially for collections that can become very large. Every programmer
 * should know about them. They just don't fit as well into STL as many people
 * thought, that's all. Even vector<bool> is still a perfectly good model for
 * how to write a proxied collection; it's just not a container, that's all,
 * and it should be called something else (some people asked for "bitvector")
 * so that people wouldn't think that it has anything to do with a conforming
 * container.'' \ref scribbu_id3v2_refs_5 "[5]"
 *
 * Eric Niebler's far more recent article suggests not:
 *
 * ``An interesting historical note: the original STL design didn’t have the
 * “true reference” requirement that causes the problem. Take a look at the SGI
 * docs for the Forward Iterator concept. Nowhere does it say that *it should
 * be a real reference. The docs for Trivial Iterators specifically mention
 * proxy references and say they’re legit.'' \ref scribbu_id3v2_refs_6 "[6]"
 *
 * I did some experimenting & found that I could get the standard algorithms to
 * work just fine with a proxied approach, and based on that, I've proceeded
 * down this implementation path.
 *
 * As an aside, I \em did try boost::iterator_facade, but couldn't get it to
 * work with a proxy; that may be my own ignorance, but I enjoyed coding up my
 * own "from scratch" in any event.
 *
 *
 * \subsection scribbu_impl_notes_serdes Serialization
 *
 * Moving to a notion of tags as mutable containers rather than read-only
 * copies of what had been read off disk introduced another problem:
 * serialization.  At first, implementing method size() was simple-- return the
 * size field as read from disk.  Once I introduced tag & frame constructors
 * allowing in-memory construction, size now had to be computed. This was not
 * difficult for ID3v2.2 tags, but in version 2.3 a problem presented itself:
 * how to compute that in the presence of compression and/or encryption of
 * individual frames?  One would need to provisionally serialize the frame,
 * compress and/or encrypt it, and then count the resulting number of bytes.
 *
 * The problem becomes worse when determining whether unsynchronisation is
 * needed-- one needs to (in the worst case) serialize all frames (including
 * compression & encryption), compute a checksum and \em then check for false
 * syncs.
 *
 * The easiest approach would have been an interface providing a single method:
 *
 \code

   enum { unsync_always, unsnyc_if_needed, unsync_never } unsync;
   size_t write(ostream &os, unsync x) const;

 \endcode
 *
 * This could be implemented by serializing everything once (including any
 * compression and encryption) and then deciding whether to apply
 * unsynchronisation on the way out.
 *
 * However, this would eliminate the possibility of methods size() and
 * needs_unsynchronisation() which I found unpalatable. When I realized I could
 * apply lazy evaluation & a "dirty" flag to individual frames to avoid
 * repeated serialization I decided to go with an interface that presents a
 * naive approach, easily implemented for ID3v2.2 frames, that would mask a
 * more complex implementation in the case of later frame implementations.
 *
 *
 * \section scribbu_id3v2_refs References
 *
 * 1. \anchor scribbu_id3v2_refs_1 [1] Nilson, Martin. ID3 tag version 2
 * http://id3.org/id3v2-00 (updated September 1, 2019)
 *
 * 2. \anchor scribbu_id3v2_refs_2 [2] Nilson, Martin.  Contributors
 * http://id3.org/Contributors (updated September 1, 2019)
 *
 * 3. \anchor scribbu_id3v2_refs_3 [3] unknown, ID3v2
 * https://en.wikipedia.org/wiki/ID3#ID3v2 (updated September 1, 2019)
 *
 * 4. \anchor scribbu_id3v2_refs_4 [4] Nilson, Martin. ID3 tag version 2.4.0 -
 * Main Structure http://id3.org/id3v2.4.0-structure (updated September 1, 2019)
 *
 * 5. \anchor scribbu_id3v2_refs_5 [5] Sutter, Herb. When Is a Container Not a
 * Container? http://www.gotw.ca/publications/mill09.htm (updated September 1,
 * 2019)
 *
 * 6. \anchor scribbu_id3v2_refs_6 [6] Niebler, Eric. To Be or Not to Be (an
 * Iterator) http://ericniebler.com/2015/01/28/to-be-or-not-to-be-an-iterator/
 * (updated September 1, 2019)
 *
 * 7. \anchor scribbu_id3v2_refs_7 [7] Stroustrup, B. and Sutton,
 * A. (Editors). A Concept Design for the STL
 * http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3351.pdf (updated
 * September 1, 2019)
 *
 *
 */

#include <exception>
#include <iostream>
#include <memory>
#include <mutex>
#include <unordered_map>

#include <boost/exception/all.hpp>
#include <boost/optional/optional.hpp>
#include <boost/shared_array.hpp>

#include <scribbu/charsets.hh>
#include <scribbu/errors.hh>
#include <scribbu/framesv2.hh>

namespace scribbu {

  namespace detail {

    /// Return an unsigned int from a three-octet sync-safe int; this
    /// implementation assumes that the input is in big-endian (per the ID3v2
    /// specification) & will return the correct value regardless of the host
    /// byte order.
    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2);

    /// Return an unsigned int from a four-octet sync-safe int; this
    /// implementation assumes that the input is in big-endian (per the ID3v2
    /// specification) & will return the correct value regardless of the host
    /// byte order.
    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3);

    /// Return an unsigned int from a five-octet sync-safe int; this
    /// implementation assumes that the input is in big-endian (per the ID3v2
    /// specification) & will return the correct value regardless of the host
    /// byte order.
    std::size_t unsigned_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3,
                                        unsigned char b4);

    /// Return a thirty-two bit unsigned int from a four-octet sync-safe int;
    /// this implementation assumes that the input is in big-endian (per the
    /// ID3v2 specification) & will return the correct value regardless of the
    /// host byte order.
    std::uint32_t uint32_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3);

    /// Return a thirty-two bit unsigned int from a five-octet sync-safe int;
    /// this implementation assumes that the input is in big-endian (per the
    /// ID3v2 specification) & will return the correct value regardless of the
    /// host byte order.
    std::uint32_t uint32_from_sync_safe(unsigned char b0,
                                        unsigned char b1,
                                        unsigned char b2,
                                        unsigned char b3,
                                        unsigned char b4);

    /// Return an unsigned int from a three-octet non-sync-fafe int; this
    /// implementation assumes that the input is in big-endian (per the ID3v2
    /// specification) & will return the correct value regardless of the host
    /// byte order.
    std::size_t unsigned_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2);

    /// Return an unsigned int from a four-octet non-sync-fafe int; this
    /// implementation assumes that the input is in big-endian (per the ID3v2
    /// specification) & will return the correct value regardless of the host
    /// byte order.
    std::size_t unsigned_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2,
                                            unsigned char b3);

    /// Return a thirty-two bit unsigned int from a three-octet non-sync-safe
    /// int; this implementation assumes that the input is in big-endian (per
    /// the ID3v2 specification) & will return the correct value regardless of
    /// the host byte order.
    std::uint32_t uint32_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2);

    /// Return a thirty-two bit unsigned int from a four-octet non-sync-safe
    /// int; this implementation assumes that the input is in big-endian (per
    /// the ID3v2 specification) & will return the correct value regardless of
    /// the host byte order.
    std::uint32_t uint32_from_non_sync_safe(unsigned char b0,
                                            unsigned char b1,
                                            unsigned char b2,
                                            unsigned char b3);

    void sync_safe_from_unsigned(std::size_t x, unsigned char b[]);
  }

  class zlib_error: public virtual boost::exception,
                    public virtual std::runtime_error
  {
  public:
    zlib_error(int status);
  };

  const std::size_t ID3V2_HEADER_SIZE = 10;

  /// Structure tying together all the information returned from
  /// looking_at_id3v2
  struct id3v2_info {
    /// If an ID3v2 tag is present, this will be set to true; if not,
    /// it will be set to false and all other fields are undefined.
    bool          present_;
    unsigned char version_;
    unsigned char revision_;
    unsigned char flags_;
    std::uint32_t size_;
  };

  /**
   * \brief Determine whether an input stream is positioned at the start of an
   * ID3v2 tag; if it is, return data found in the ID3v2 header
   *
   * \param is [in] the input stream to be probed for an ID3v2 tag
   *
   * \param restore_get_if_found [in,opt] If the caller sets this to true (the
   * default), and an ID3v2 tag is present in \a is, the input stream's get
   * pointer will be reset to its position on entry. If the caller sets it to
   * false, the get pointer will be left positioned ten bytes further than it
   * was on entry (i.e. it will be positioned to the first byte after the ID3v2
   * header). If no ID3v2 tag is present, the get pointer is always restored to
   * where it was on entry to this function.
   *
   * \retrun an id3v2_info struct containing the results
   *
   *
   * This is intended to be used as a peek operation where the caller can avoid
   * a full parse of the tag; it does the following:
   *
   * - read the first ten bytes
   * - if it's not an ID3v2 tag, restore the get ptr & so indicate to
   *   the caller
   * - if it *is* so indicate, along with the version (so that the
   *   caller can construct the corresponding type) and the size (so
   *   the caller can skip the tag, if desired)
   * - optionally restore the get ptr
   *
   *
   */

  id3v2_info
  looking_at_id3v2(std::istream &is, bool restore_get_if_found=true);

  /**
   * \brief Restore false sync signals in a buffer
   *
   *
   * \param p [in] Address of the buffer to be resynchronized; note that the
   * buffer will be resynchronized in-place
   *
   * \param cb [in] The number of bytes in the buffer
   *
   * \return The number of bytes in the resynchronized buffer
   *
   *
   * "Unsynchronisation" refers to the process of removing false
   * synchronisations when writing an ID3v2 tag; cf.  sec.  5 of the ID3v2.3
   * specification, or \ref scribbu_id3v2_unsync "here" for background.  For
   * purposes of this discussion, resynchronisation refers to the process of
   * replacing each two-byte sequence 0xff, 0x00 with just 0xff in the provided
   * buffer.
   *
   * This implementation will perform the resynchronisation in place;
   * i.e. given an input buffer like so:
   *
   \code

     [0x00, 0x01, 0xff, 0x00, 0x02, 0x03, 0xff, 0x00, 0x04, 0x05, 0xff, 0x00, 0x06, 0x07]

   \endcode
   *
   * this function will alter the buffer to this:
   *
   \code

     [0x00, 0x01, 0xff, 0x02, 0x03, 0xff, 0x04, 0x05, 0xff, 0x06, 0x07, 0x00, 0x06, 0x07]

   \endcode
   *
   * and return a value of eleven.
   *
   * I chose this interface for two reasons, the first being that it avoids an
   * allocation & the second being that the copies follow a particular
   * pattern. In this example, we copy the following ranges:
   *
   * - [4,7) -> [3,6)
   * - [8,11) -> [6,9)
   * - [12,14) -> [9,11)
   *
   * Note that for each successive false sync being restored, we can copy
   * successively larger & larger chunks at each time, leaving open the
   * possibility for optimization through loop unrolling, SSE, &c.
   *
   *
   */

  std::size_t resynchronise(unsigned char *p, std::size_t cb);

  /**
   * \class id3v2_tag
   *
   * \brief Core ID3v2 functionality
   *
   *
   * Class id3v2_tag handles some functionality common to all ID3v2 tags, such
   * as version/revision information & the unsynch attribute, but it mostly
   * defines an interface to which all concrete ID3v2 tags shall conform,
   * enabling client code to work in a version-independent way (e.g. a client
   * should be able to ask for the "artist" without having to worry about
   * whether the tag is ID3v2.2, 2.3 or 2.4).
   *
   *
   */

  class id3v2_tag {

  public:
    ///////////////////////////////////////////////////////////////////////////////
    //                             Nested Types                                  //
    ///////////////////////////////////////////////////////////////////////////////

    // Some frames are constrained by the standard to be unique
    class duplicate_frame_error: public error
    {
    public:
      duplicate_frame_error(const frame_id3 &id, std::size_t n): id3_(id), n_(n)
      { }
      duplicate_frame_error(const frame_id4 &id, std::size_t n): id4_(id), n_(n)
      { }
      virtual const char * what() const noexcept(true);
    private:
      frame_id3 id3_;
      frame_id4 id4_;
      std::size_t n_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    class unknown_frame_error: public error
    {
    public:
      unknown_frame_error(const frame_id3 &id): id3_(id)
      { }
      unknown_frame_error(const frame_id4 &id): id4_(id)
      { }
      virtual const char * what() const noexcept(true);
    private:
      frame_id3 id3_;
      frame_id4 id4_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    class reserved_frame_error: public error
    {
    public:
      reserved_frame_error(const frame_id3 &id): id3_(id)
      { }
      reserved_frame_error(const frame_id4 &id): id4_(id)
      { }
      virtual const char * what() const noexcept(true);
    private:
      frame_id3 id3_;
      frame_id4 id4_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    class invalid_tag: public error
    {
    public:
      invalid_tag()
      { }
      virtual const char * what() const noexcept(true);
    };

    class no_tag: public error
    {
    public:
      no_tag()
      { }
      virtual const char * what() const noexcept(true);
    };

    class unknown_version: public error
    {
    public:
      unknown_version(unsigned char version):
        version_(version)
      { }
      unsigned char get_version() const
      { return version_; }
      virtual const char * what() const noexcept(true);
    private:
      unsigned char version_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    typedef scribbu::encoding encoding;

  public:

    ///////////////////////////////////////////////////////////////////////////
    //                             Construction                              //
    ///////////////////////////////////////////////////////////////////////////

    /// Initialize from the first five bytes of \a is
    id3v2_tag(std::istream &is);
    /// Initialize from an id3v2_info
    id3v2_tag(const id3v2_info &H);
    /// Initialize "from scratch"
    id3v2_tag(unsigned char ver, unsigned char rev):
      version_(ver), revision_(rev), unsync_(boost::none)
    { }
    id3v2_tag(const id3v2_tag &that):
      version_(that.version_),
      revision_(that.revision_),
      unsync_(that.unsync_)
    { }
    virtual ~id3v2_tag()
    { }
    virtual id3v2_tag* clone() const = 0;

    id3v2_tag& operator=(const id3v2_tag &that) {
      if (this != &that) {
        version_ = that.version_;
        revision_ = that.revision_;
        unsync_ = that.unsync_;
      }
      return *this;
    }

  public:
    /////////////////////////////////////////////////////////////////////////////
    //                      Common ID3v2 Attributes                            //
    /////////////////////////////////////////////////////////////////////////////

    /// Return the ID3v2 version of this tag (e.g. an ID3v2.3 tag will return 3)
    unsigned char version() const
    { return version_; }
    /// Return the ID3v2 revision of this tag; no revisions other than zero
    /// were ever defined AFAIK
    unsigned char revision() const
    { return revision_; }
    /// Return whether or not the unsynchronisation scheme was applied during
    /// the most recent serialization or desieralization; if this tag was never
    /// serdes, return boost:;none
    boost::optional<bool> unsynchronised() const
    { return unsync_; }
    /// Retrieve this tag's ID3v2 flags; the exact meaning of each bit will
    /// depend on the particular ID3v2 version
    virtual unsigned char flags() const = 0;

    /////////////////////////////////////////////////////////////////////////////
    //                          id3v2 Serialization                            //
    /////////////////////////////////////////////////////////////////////////////

    /// Compute the serialized size of this tag (in bytes) exclusive of the
    /// ID3v2 header (i.e. return the total tag size on disk, in bytes, less
    /// ten)
    virtual std::size_t size(bool unsync = false) const = 0;
    /// Return true if the serialization of this tag would contain false syncs
    /// if serialized in its present state
    virtual bool needs_unsynchronisation() const = 0;
    /// Serialize this tag to an output stream, perhaps applying the
    /// unsynchronisation scheme if the caller so chooses ("unsynchronised"
    /// will be updated accordingly)
    virtual std::size_t write(std::ostream &os, bool unsync = false) const = 0;

    /////////////////////////////////////////////////////////////////////////////
    //                  attributes common to all ID3v2 tags                    //
    /////////////////////////////////////////////////////////////////////////////

    virtual std::size_t num_frames() const = 0;
    virtual std::size_t padding() const = 0;
    virtual void padding(std::size_t padding) = 0;

    /////////////////////////////////////////////////////////////////////////////
    //                    Frames Common to all ID3v2 Tags                      //
    /////////////////////////////////////////////////////////////////////////////

    virtual
    std::string
    album(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    artist(encoding dst = encoding::UTF_8,
           on_no_encoding rsp = on_no_encoding::fail,
           const boost::optional<encoding> &src =
           boost::none) const = 0;
    virtual
    std::string
    content_type(encoding dst = encoding::UTF_8,
                 on_no_encoding rsp = on_no_encoding::fail,
                 const boost::optional<encoding> &src =
                   boost::none) const = 0;
    virtual
    std::string
    encoded_by(encoding dst = encoding::UTF_8,
               on_no_encoding rsp = on_no_encoding::fail,
               const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    languages(encoding dst = encoding::UTF_8,
              on_no_encoding rsp = on_no_encoding::fail,
              const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::size_t play_count() const = 0;
    virtual
    std::string
    title(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    track(encoding dst = encoding::UTF_8,
          on_no_encoding rsp = on_no_encoding::fail,
          const boost::optional<encoding> &src = boost::none) const = 0;
    virtual
    std::string
    year(encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const = 0;

    virtual std::size_t has_album() const = 0;
    virtual std::size_t has_artist() const = 0;
    virtual std::size_t has_content_type() const = 0;
    virtual std::size_t has_encoded_by() const = 0;
    virtual std::size_t has_languages() const = 0;
    virtual std::size_t has_play_count() const = 0;
    virtual std::size_t has_title() const = 0;
    virtual std::size_t has_track() const = 0;
    virtual std::size_t has_year() const = 0;

    virtual void
    album(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    artist(const std::string &text,
           encoding src = encoding::UTF_8,
           bool add_bom = false,
           on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    content_type(const std::string &text,
                 encoding src = encoding::UTF_8,
                 bool add_bom = false,
                 on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    encoded_by(const std::string &text,
               encoding src = encoding::UTF_8,
               bool add_bom = false,
               on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    languages(const std::string &text,
              encoding src = encoding::UTF_8,
              bool add_bom = false,
              on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    title(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    track(const std::string &text,
          encoding src = encoding::UTF_8,
          bool add_bom = false,
          on_no_encoding rsp = on_no_encoding::fail) = 0;
    virtual void
    year(const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail) = 0;

    /**
     * \brief Add a comment frame
     *
     *
     * \param text [in] comment text
     *
     * \param lang [in, opt] optional ISO-639-2 language code in which this
     * comment is expressed
     *
     * \param src [in, opt] optional encoding scheme used in the comment text &
     * description
     *
     * \param unicode [in, opt] if \a no, the comment will be encoded as
     * ISO-8859-1; if yes as Unicode (the preciseu unicode form will be
     * determined by the implementation, no which more below), and if with_bom
     * Unicode with a Byte Order Mark
     *
     * \param description [in, opt] optional brief description of the comment
     * text
     *
     * \param rsp [in, opt] action on conversion failure
     *
     *
     * ID3v2.2 & .3 only offer IOS-8859-1 and UCS-2 encoding; ID3v2.4 offers
     * ISO-8859-1, UTF-16, UTF-16BE, & UTF-8. This virtual, since it applies to
     * all versions, allows the caller to only choose between "unicode" and
     * "not unicode". If the caller wants finer-grained control they'll need to
     * cast their tag to id3v2_4_tag & invoke the relevant method thereon.
     *
     *
     */

    virtual void
    add_comment(const std::string &text,
                language lang = language::from_locale,
                encoding src = encoding::UTF_8,
                use_unicode unicode = use_unicode::no,
                const std::string &description = std::string(),
                on_no_encoding rsp = on_no_encoding::fail) = 0;

    /**
     * \brief Add a user-defined text frame
     *
     *
     * \param text [in] user-defined text
     *
     * \param src [in, opt] optional encoding scheme used in the comment text &
     * description
     *
     * \param unicode [in, opt] if \a no, the comment will be encoded as
     * ISO-8859-1; if yes as Unicode (the preciseu unicode form will be
     * determined by the implementation, no which more below), and if with_bom
     * Unicode with a Byte Order Mark
     *
     * \param dsc [in, opt] optional brief description of the comment
     * text
     *
     * \param rsp [in, opt] action on conversion failure
     *
     *
     * ID3v2.2 & .3 only offer IOS-8859-1 and UCS-2 encoding; ID3v2.4 offers
     * ISO-8859-1, UTF-16, UTF-16BE, & UTF-8. This virtual, since it applies to
     * all versions, allows the caller to only choose between "unicode" and
     * "not unicode". If the caller wants finer-grained control they'll need to
     * cast their tag to id3v2_4_tag & invoke the relevant method thereon.
     *
     *
     */

    virtual void
    add_user_defined_text(const std::string &text,
                          encoding src = encoding::UTF_8,
                          use_unicode unicode = use_unicode::no,
                          const std::string &dsc = std::string(),
                          on_no_encoding rsp = on_no_encoding::fail) = 0;

    /// Retrieve the contents of an arbitrary text frame
    virtual
    std::string
    text(id3v2_text_frames id,
         encoding dst = encoding::UTF_8,
         on_no_encoding rsp = on_no_encoding::fail,
         const boost::optional<encoding> &src = boost::none) const = 0;
    /// Set the contents of an arbitrary text frame
    virtual
    void
    text(id3v2_text_frames id,
         const std::string &text,
         encoding src = encoding::UTF_8,
         bool add_bom = false,
         on_no_encoding rsp = on_no_encoding::fail) = 0;
    /// Delete an arbitrary text frame
    virtual
    void
    delete_frame(id3v2_text_frames id) = 0;

    //////////////////////////////////////////////////////////////////////////
    //                           iterator support                           //
    //////////////////////////////////////////////////////////////////////////

  protected:

    /**
     * \class frame_iterator_base
     *
     * \brief Functionality common to both const & mutable iterators
     *
     * \param impl_type (non-const) (random-access) iterator type in the
     * underlying frame collection
     *
     *
     * I've factored our as much iterator functionality as possible here. It's
     * implemented in terms of a mutable iterator; the const_iterator takes
     * care of enforcing RO semantics.
     *
     *
     */

    template <typename impl_type>
    class frame_iterator_base {

    public:
      /// std iterator category-- all frame iterators are random-access
      typedef std::random_access_iterator_tag iterator_category;
      /// result of subtracting two frame iterators
      typedef std::ptrdiff_t difference_type;
      /// constructs a "one-past-the-end" iterator
      frame_iterator_base()
      { }
      /// \a p0 is the beginning of the container & \a p is the position which
      /// will represent this
      frame_iterator_base(const impl_type &p0, const impl_type &p): p0_(p0), p_(p)
      { }
      virtual ~frame_iterator_base()
      { }

    public:
      /// retrieve this iterator's index in the enclosing container
      std::size_t index() const
      { return p_ - p0_; }
      friend difference_type operator-(const frame_iterator_base &lhs,
                                       const frame_iterator_base &rhs)
      { return lhs.p_ - rhs.p_; }
      friend bool operator==(const frame_iterator_base &lhs,
                             const frame_iterator_base &rhs)
      { return lhs.p_ == rhs.p_; }
      friend bool operator!=(const frame_iterator_base &lhs,
                             const frame_iterator_base &rhs)
      { return ! (lhs == rhs); }
      friend bool operator<(const frame_iterator_base &lhs,
                            const frame_iterator_base &rhs)
      { return lhs.p_ < rhs.p_; }

    protected:
      /// increment this iterator by n; return a referene to this iterator
      frame_iterator_base& incr(std::ptrdiff_t n = 1)
      { p_ += n; return *this; }
      /// decrement this iterator by n; return a referene to this iterator
      frame_iterator_base& decr(std::ptrdiff_t n = 1)
      { p_ -= n; return *this; }

    protected:
      impl_type p0_, p_;
    };

    /**
     * \class frame_iterator
     *
     * \brief non-const frame iterator
     *
     * \param tag_type concrete subclass
     *
     *
     */

    template <class tag_type, class frame_type, class proxy_type, class impl_type>
    class frame_iterator: public frame_iterator_base<impl_type> {

      typedef frame_iterator_base<impl_type> base_type;

    public:
      /// std iterator category-- all frame iterators are random-access
      typedef std::random_access_iterator_tag iterator_category;
      /// result of subtracting two frame iterators
      typedef std::ptrdiff_t difference_type;

    public:
      /// The type "pointed to" by this iterator
      typedef frame_type value_type;
      typedef frame_type *pointer;
      typedef frame_type &reference;

      frame_iterator()
      { }

      explicit frame_iterator(tag_type *pown, const impl_type &p):
        base_type(pown->frames_.begin(), p), pown_(pown)
      { }

      frame_iterator& operator++()
      { base_type::incr(); return *this; }

      frame_iterator operator++(int)
      {
        frame_iterator tmp(*this);
        base_type::incr();
        return tmp;
      }

      frame_iterator& operator--()
      { base_type::decr(); return *this; }

      frame_iterator operator--(int)
      {
        frame_iterator tmp(*this);
        base_type::decr();
        return tmp;
      }

      frame_iterator& operator+=(difference_type i)
      { base_type::incr(i); return *this; }

      frame_iterator& operator-=(difference_type i)
      { base_type::decr(i); return *this; }

      frame_iterator operator+(difference_type i)
      {
        frame_iterator tmp(*this);
        tmp += i;
        return tmp;
      }

      frame_iterator operator-(difference_type i)
      {
        frame_iterator tmp(*this);
        tmp -= i;
        return tmp;
      }

      proxy_type operator*() const
      { return proxy_type(pown_, base_type::index()); }

      proxy_type operator->() const
      { return proxy_type(pown_, base_type::index()); }

      proxy_type operator[](difference_type i)
      { return *(*this + i); }

    private:
      tag_type *pown_;

    };

    /**
     * \class const_frame_iterator
     *
     * \brief const frame iterator
     *
     *
     */

    template <class frame_type, class impl_type>
    class const_frame_iterator: public frame_iterator_base<impl_type> {

      typedef frame_iterator_base<impl_type> base_type;

    public:
      /// std iterator category-- all frame iterators are random-access
      typedef std::random_access_iterator_tag iterator_category;
      /// result of subtracting two frame iterators
      typedef std::ptrdiff_t difference_type;

    public:
      /// The type "pointed to" by this iterator
      typedef frame_type value_type;
      typedef const frame_type *pointer;
      typedef const frame_type &reference;

      const_frame_iterator()
      { }

      explicit const_frame_iterator(const impl_type &p0,
                                    const impl_type &p):
        frame_iterator_base<impl_type>(p0, p)
      { }

      template <class tag_type, typename proxy_type>
      const_frame_iterator(const frame_iterator<tag_type, frame_type, proxy_type, impl_type> &p):
        frame_iterator_base<impl_type>(p)
      { }

      const_frame_iterator& operator++()
      { base_type::incr(); return *this; }

      const_frame_iterator operator++(int)
      {
        const_frame_iterator tmp(*this);
        base_type::incr();
        return tmp;
      }

      const_frame_iterator& operator--()
      { base_type::decr(); return *this; }

      const_frame_iterator operator--(int)
      {
        const_frame_iterator tmp(*this);
        base_type::decr();
        return tmp;
      }

      const_frame_iterator& operator+=(difference_type i)
      { base_type::incr(i); return *this; }

      const_frame_iterator& operator-=(difference_type i)
      { base_type::decr(i); return *this; }

      const_frame_iterator operator+(difference_type i)
      {
        const_frame_iterator tmp(*this);
        tmp += i;
        return tmp;
      }

      const_frame_iterator operator-(difference_type i)
      {
        const_frame_iterator tmp(*this);
        tmp -= i;
        return tmp;
      }
      reference operator*() const
      {
        return *(base_type::p_->get());
      }

      pointer operator->() const
      { return base_type::p_->get(); }

    };

  protected:
    /// Set the "unsynchronised" field
    void unsynchronised(bool f)
    { unsync_ = f; }
    /// Parse the flags & size from an ID3v2 header
    std::pair<unsigned char, std::size_t>
    parse_flags_and_size(std::istream &is);

  private:
    /// ID3v2 version (e.g. for an ID3v2.3 flag, this would be "3")
    unsigned char version_;
    /// ID3v2 revision; AFAIK no value other than "0" was ever defined by spec
    unsigned char revision_;
    /// Whether or not unsynchronisation was applied during
    /// serialization/deserialization for this tag; if this tag has never been
    /// serdes, then this member will be boost::none
    boost::optional<bool> unsync_;

  }; // End class id3v2_tag.

} // End namespace scribbu.

#endif // not ID3V2_HH_INCLUDED
