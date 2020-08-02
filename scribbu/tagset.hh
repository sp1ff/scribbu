/**
 * \file tagset.hh
 *
 * Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>
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
#ifndef TAGSET_H_INCLUDED
#define TAGSET_H_INCLUDED 1
/**
 * \page scribbu_tagsets Tagsets
 *
 * \section scribbu_tagsets_intro Introduction
 *
 * This module provides facilities for writing "tagsets", or collections of
 * ID3v2 tags. It is aimed at wholesale replacement of tagsets in that its
 * methods write an entire tagset at once. While it is intelligent enough to
 * "emplace" them (i.e. to overwrite an existing tagset of equal or larger size)
 * to avoid copying track data needlessly, it is not well-suited to incremental
 * modifications (e.g. increasing the play count). It specifically excludes
 * ID3v1 tags-- those are handled by free functions declared in id3v1.hh.
 *
 *
 * \section scribbu_tagsets_scribbu scribbu Facilities for File I/O
 *
 * At the time of this writing, there is no central location documenting
 * scribbu file I/O facilities. This is a start.
 *
 * Types related to file I/O:
 *
 *     1. scribbu::file_info (scribbu/scribbu.hh): a utility class describing an
 *     arbitrary file with attributes:
 *
 *        1. filename
 *
 *        2. parent directory
 *
 *        3. size of the file in bytes
 *
 *    2. scribbu::track_data (scribbu/scribbu.hh): a utility class specifically
 *    describing an audio file. It is constructed with an istream whose get ptr
 *    is positioned just after the ID3 tags. It has attributes:
 *
 *        1. size of track data in bytes
 *
 *        2. MD5 checksum of track data
 *
 *    3.  scribbu::id3v2_info (scribbu/id3v2.hh): describes an ID3v2 tag (or the
 *    absence thereof-- it's returned from looking_at_id3v2). It has attributes:
 *
 *        1. presence flag
 *
 *        2. ID3v2 version, revision & flags
 *
 *        3. size of the tag, in bytes
 *
 *    4. scribbu::id3v1_info (scribbu/id3v1.hh): describes an ID3v1 tag (or the
 *    absence thereof it's returned by ends_in_id3v1). It has attributes:
 *
 *        1. type (including "none")
 *
 *        2. file offsets of the beginning & end of the tag
 *
 *
 * Free functions related to file I/O:
 *
 *     1. scribbu::open_file (scribbu/scribbu.hh): takes a boost filesystem
 *        path, returns a scribbu::file_info & a boost filesystem istream.
 *
 *     2. scribbu::looking_at_id3v2 (scribbu/id3v2.hh): takes a boost filesystem
 *        istream & determines whether there is an ID3v2 tag at the stream's get
 *        ptr; returns a scribbu::id3v2_info.
 *
 *     3. scribbu::maybe_read_id3v2 (scribbu/scribbu-utils.hh): takes a boost
 *        filesystem istream, returns an id3v2_tag*; if there is no ID3v2 tag at
 *        the input stream's get ptr, returns nil
 *
 *     4. scribbu::read_id3v2 (scribbu/scribbu-utils.hh): takes a boost
 *        filesystem istream, returns an id3v2_tag*; if there is no ID3v2 tag at
 *        the input stream's get ptr, throws an exception
 *
 *     5. scribbu::read_all_id3v2 (scribbu/scribbu-utils.hh): takes a boost
 *        filesystem istream & an output iterator to which unique_ptrs can be
 *        copied, copies id3v2_tag*-s to the output iterator. If there _are_ no
 *        ID3v2 tags, just doesn't copy anything.
 *
 *     6. scribbu::ends_in_id3v1 (scribbu/id3v1.hh): tests an istream for the
 *     precense of an ID3v1 tag at the end; returns an id3v1_info.
 *
 *     7. scribbu::process_id3v1 (scribbu/id3v1.hh): takes an istream, returns
 *     an id3v1_tag*, or null if there is none
 *
 *
 * Putting these together, the stock recipe for processing an MP3 file is:
 *
 \code

  fs::ifstream ifs(pth, ios_base::binary);
  vector<unique_ptr<scribbu::id3v2_tag>> id3v2;
  scribbu::read_all_id3v2(ifs, back_inserter(id3v2));
  scribbu::track_data td((istream&)ifs);
  unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(ifs);

 \endcode
 *
 * or
 *
 \code

  fs::ifstream ifs(in, fs::ifstream::binary);
  ifs.exceptions(EXC_MASK);

  scribbu::id3v2_info id3v2 = scribbu::looking_at_id3v2(ifs);

  while (id3v2.present_) {
    // ifs' get ptr is at the beginning of the ID3v2 tag
    // the tag has 10 + id3v2.size_) bytes in it
    id3v2 = scribbu::looking_at_id3v2(ifs);
  }

  // Now at the beginning of the track data
  scribbu::id3v1_info I = scribbu::ends_in_id3v1(ifs);
  // there are `cb' bytes in the track
  std::size_t cb = I.start_ - ifs.tellg();
  if (I.start_ != I.end_) {
    // ifs' get ptr is at the start of the ID3v1 tag
    // it has I.end_ - I.start_ bytes in it
  }

 \endcode
 *
 *
 *
 */

#include <sstream>

#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/mapped_file.hpp>

#include <scribbu/id3v2.hh>

namespace scribbu {

  class invalid_tagset_request: public error{

  public:
    enum cause {
      emplace_mismatch_sz,
      adj_pad_not_enough,
    };

  public:
    invalid_tagset_request(cause c): cause_(c)
    { }
    virtual const char * what() const noexcept;

  private:
    cause cause_;
    mutable std::shared_ptr<std::string> pwhat_;

  };

  enum class apply_unsync {
    always,
    never,
    as_needed
  };

  namespace detail {

    /**
     * \brief Produce a backup file name
     *
     *
     * \param pth [in] boost filesystem path naming a file requiring a unique
     * name for backup
     *
     * \return A boost filesystem path naming a backup for \a pth of the form
     * "$pth.N" where N is an integer
     *
     *
     * This function will examine the directory in which \a pth resides for
     * filenames of the form "$pth.N" (i.e. files beginning with \a pth with
     * integer suffixes), sort them numerically (i.e. .10 is greater than .1)
     * and return such a path with an integer suffix one greater than the
     * maximum.
     *
     * The intent is to produce a sequence of backup filenames in the old
     * VMS style: foo.1, foo.2, foo.4 &c.
     *
     *
     */

    inline
    boost::filesystem::path
    get_backup_name(const boost::filesystem::path &pth)
    {
      using namespace std;
      using namespace boost::filesystem;

      auto here = absolute(pth).parent_path();
      auto prefix = pth.string();
      size_t nprefix = prefix.length();

      vector<path> entries;
      for (directory_iterator p0(here), p1; p0 != p1; ++p0) {

        string s = p0->path().string();

        // we can eliminuate most entries by simply matching the
        // first part of the string against `prefix'.
        if (prefix != s.substr(0, nprefix)) {
          continue;
        }

        // We next expect s[nprefix] == '.'; the "+1" below guards
        // against "prefix."
        if (s.length() <= nprefix + 1 || '.' != s[nprefix]) {
          continue;
        }

        // We next expect all characters in s[nprefix+1:] to be digits
        bool all_num = true;
        for (ptrdiff_t i = nprefix + 1, n = s.length(); i < n; ++i) {
          if ('0' > s[i] || '9' < s[i]) {
            all_num = false;
            break;
          }
        }

        if (! all_num) {
          continue;
        }

        // If we made it here, *p0 is a path of the form <prefix>.NNN
        entries.push_back(*p0);
      }

      if (entries.empty()) {
        string ext = pth.extension().string() + ".1";
        return here / path(pth.stem().string() + ext);
      }

      sort(entries.begin(), entries.end(),
           [](const path &p1, const path &p2) {
             int x0 = atoi(p1.extension().string().substr(1).c_str());
             int x1 = atoi(p2.extension().string().substr(1).c_str());
             return x0 < x1;
           });

      string s = entries.back().extension().string().substr(1);
      int n = atoi(s.c_str());
      n += 1;

      stringstream stm;
      stm << pth.string() << "." << n;

      return path(stm.str());

    } // End get_backup_name.

    inline
    bool
    compute_apply_unsync(apply_unsync unsync, const id3v2_tag &tag)
    {
      if (apply_unsync::never == unsync) {
        return false;
      } else if (apply_unsync::always == unsync) {
        return true;
      } else {
        return tag.needs_unsynchronisation();
      }
    }

  } // End nested namespace detail.

  /**
   * \brief Replace an ID3v2 tagset by making a copy of the entire file
   *
   *
   * \param pth [in] boost filesystem path naming the file whose tagset shall be
   * replaced
   *
   * \param p0 [in] a forward input iterator marking the beginning of a range of
   * pointers to id3v2_tag (or a type supporting operator->) which shall be
   * written to \a pth
   *
   * \param p1 [in] a forward input iterator marking the one-past-the-end
   * element of a range of pointers to id3v2_tag (or a type supporting
   * operator->) which shall be written to \a pth
   *
   * \param unsync [in] a boolean indicating whether or not to apply the
   * unsynchronisation scheme while writing out [p0,p1)
   *
   * \param keep_backup [in] if the caller sets this to true, the original
   * file will be kept in a backup named by appending a unique, sequential
   * integer to the original name
   *
   *
   * \todo Support applying unsynchronisation on a tag-by-tag basis
   *
   *
   * If the new tagset is larger than the extant tagset, and the difference
   * cannot be taken up by padding, or the caller would prefer not to reduce the
   * padding, then we're left with no choice but to write the new tagset to disk
   * in a new file & copy the remainder of the old file after that into the new.
   *
   * This function works in a way that may seem unneccessarily complex at first:
   *
   * 1. open a temporary file & write the new tagset to it
   *
   * 2. copy the contents of \a pth \em after its tagset to the temporary file
   *
   * 3. if \a keep_backup is true, copy the original file to the backup name
   *
   * 4. copy the temporary file over the original file
   *
   * This expends more disk I/O for the benefit of not writing the original file
   * until the very end (in what I would *think* would be an atomic
   * operation). If, say, I worked on \a pth in place, and encountered an error
   * partway through, I would have corrupted the original file.
   *
   *
   */

  template <typename forward_input_iterator>
  void replace_tagset_copy(const boost::filesystem::path &pth,
                           forward_input_iterator p0,
                           forward_input_iterator p1,
                           apply_unsync unsync,
                           // LATER(sp1ff): would like this to be defaulted to false
                           bool keep_backup = true)
  {
    using namespace std;

    // TODO(sp1ff): this is re-used all over the place-- factor out?
    const ios::iostate EXC_MASK = ios::eofbit  |
                                  ios::failbit |
                                  ios::badbit;

    namespace fs = boost::filesystem;
    namespace sys = boost::system;

    // Ideally, I'd using std::tmpfile or mkstmp; functions that determine the
    // temporary name & open it atomically, to avoid the race condition (and
    // threat vector) of having a process determine the name, a second process
    // create open a file by that name, and the first then opening it. The
    // problem is that such implementations will delete the temp file as
    // soon as I close my handle to it, making it impossible for me to
    // write, close & copy it.

    // My (admittedly poor) solution is to live with the race condition &
    // trucnate on open, hoping to merely stomp on the other process' file in
    // the event I hit it (the race condition).
    fs::path tmpnam = fs::temp_directory_path() / fs::unique_path();
    fs::ofstream tmpfs(tmpnam, ios_base::out |  ios_base::binary | ios_base::trunc);
    tmpfs.exceptions(EXC_MASK);

    // for_each(p0, p1, [&](unique_ptr<id3v2_tag> &p) {
    typedef typename forward_input_iterator::value_type T;
    for_each(p0, p1, [&](T &p) {
      bool do_unsync = detail::compute_apply_unsync(unsync, *p);
      p->write(tmpfs, do_unsync);
    });

    // At this point, the new tagset has been written to the temp file.
    fs::ifstream ifs(pth, fs::ifstream::binary);
    ifs.exceptions(EXC_MASK);

    scribbu::id3v2_info id3v2 = scribbu::looking_at_id3v2(ifs);
    while (id3v2.present_) {
      ifs.seekg(10 + id3v2.size_, fs::ifstream::cur);
      id3v2 = scribbu::looking_at_id3v2(ifs);
    }

    // `ifs'' get ptr is now located at the beginning of it's track data

    // Surely not the most efficient implementation, but until I have some data
    // I'm going to defer to the std implementation...
    streampos here = ifs.tellg();
    ifs.seekg(0, fs::ifstream::end);
    streampos there = ifs.tellg();
    ifs.seekg(here, fs::ifstream::beg);

    size_t cb = there - here;
    vector<char> buf(cb);
    ifs.read(&(buf[0]), cb);
    tmpfs.write(&(buf[0]), cb);

    // Write complete-- close all file handles & shuffle the files.
    tmpfs.close();
    ifs.close();

    fs::path cp;
    if (keep_backup) {

      cp = detail::get_backup_name(pth);

      // This is a race condition (`cp' could be created in between the call to
      // `exists' and `copy'), but in that unlikely event the worst that will
      // happen is the call to `copy' will fail.
      if (fs::exists(cp)) {
        fs::remove(cp);
      }

      fs::copy(pth, cp);
    }

    // Attempt a rename
    sys::error_code ec;
    fs::rename(tmpnam, pth, ec);
    if (ec) {
      if (sys::errc::cross_device_link == ec.value()) {
        // `rename()' doesn't work across filesystems-- fallback to copy + rm.

        // Thing is, `copy' fails when the destination exists (sigh). So, copy
        // `pth' off to a backup (if we haven't already)...
        if (cp.empty()) {
          cp = detail::get_backup_name(pth);
          if (fs::exists(cp)) {
            fs::remove(cp);
          }
          fs::copy(pth, cp);
        }
        // remove the original...
        fs::remove(pth);
        // and attempt the copy & remmove
        try {
          fs::copy_file(tmpnam, pth);
          fs::remove(tmpnam);
        } catch (const std::exception &ex) {
          // Well, we're kind of screwed. At least attempt to put the original
          // file back.
          fs::copy(cp, pth, ec); // `ec' ignored intentionally on return
          throw;
        }
      } else {
        throw sys::system_error(ec);
      }
    }
  }

  /**
   * \brief Compute the serialized size, as well as the total padding, of a
   * tagset on disk
   *
   *
   * \param p0 [in] a forward input iterator dereferencing to an id3v2_tag*
   * (or any type for which operator-> resolves to an id3v2_tag&) pointing to
   * the first element in a range of tags
   *
   * \param p1 [in] an identical iterator pointint to the one-past-the-end
   * element of the range of tags of interest
   *
   * \param unsync [in] true indicates that the unsynchronisation scheme
   * shall be applied while writing [p0,p1)
   *
   * \return The number of bytes that [p0,p1) would consume on disk as well as
   * the total padding contained in [p0, p1)
   *
   *
   *
   */

  template <typename forward_input_iterator>
  std::tuple<std::size_t, std::size_t>
  tagset_sizes(forward_input_iterator p0,
               forward_input_iterator p1,
               apply_unsync           unsync)
  {
    size_t cb_tot = 0, cb_pad = 0;
    for ( ; p0 != p1; ++p0) {
      bool do_unsync = detail::compute_apply_unsync(unsync, **p0);
      cb_tot += (*p0)->size(do_unsync) + 10;
      cb_pad += (*p0)->padding();
    }
    return std::make_tuple(cb_tot, cb_pad);
  }

  /**
   * \brief Get the size of a tagset on disk
   *
   *
   * \param pth [in] boost filesystem path naming the file of interest
   *
   * \return The number of bytes consumed by ID3v2 tags in \a pth
   *
   *
   */

  inline
  std::size_t
  tagset_size(const boost::filesystem::path &pth)
  {
    namespace fs = boost::filesystem;

    const std::ios::iostate EXC_MASK = std::ios::eofbit  |
                                       std::ios::failbit |
                                       std::ios::badbit;

    size_t cb = 0;
    fs::ifstream ifs;

    try {
      ifs.open(pth, fs::ifstream::binary);
      ifs.exceptions(EXC_MASK);
    } catch (const fs::ifstream::failure &) {
      return cb;
    }

    scribbu::id3v2_info id3v2 = scribbu::looking_at_id3v2(ifs, false);
    while (id3v2.present_) {
      cb += 10 + id3v2.size_;
      ifs.seekg(id3v2.size_, fs::ifstream::cur);
      id3v2 = scribbu::looking_at_id3v2(ifs, false);
    }

    return cb;
  }

  /**
   * \brief Replace a tagset
   *
   *
   * \param pth [in] path naming the file containing the tagset to be
   * replaced
   *
   * \param p0 [in] a forward input iterator dereferencing to an id3v2_tag*
   * (or any type for which operator-> resolves to an id3v2_tag&) pointing to
   * the first element in a range of tags to be written out
   *
   * \param p1 [in] an identical iterator pointint to the one-past-the-end
   * element of the range of tags to be written
   *
   * \param unsync [in] true indicates that the unsynchronisation scheme
   * shall be applied while writing [p0,p1) to \a pth
   *
   * \pre The serialized size of [p0,p1) must exactly match the size
   * on disk of the extant tagset in \a pth. Since this is, in general,
   * difficult to ensure, consider using maybe_emplace_tagset, instead
   *
   * \sa maybe_emplace_tagset
   *
   *
   */

  template <typename forward_input_iterator>
  void replace_tagset_emplace(const boost::filesystem::path &pth,
                              forward_input_iterator p0,
                              forward_input_iterator p1,
                              apply_unsync unsync)
  {
    using namespace boost::iostreams;

    // We begin by computing the size of the extant tagset; this is needless if
    // the caller has already done so, but given the cost of getting this wrong,
    // I'm going to code defensively.
    size_t cb_extant = tagset_size(pth);

    // Now let's compute the size of the new tagset.
    size_t cb_new, cb_pad;
    std::tie(cb_new, cb_pad) = tagset_sizes(p0, p1, unsync);

    if (cb_extant != cb_new) {
      throw invalid_tagset_request(invalid_tagset_request::emplace_mismatch_sz);
    }

    mapped_file mf(pth.native());
    stream<mapped_file> ofs(mf);

    for ( ; p0 != p1; ++p0) {
      bool do_unsync = detail::compute_apply_unsync(unsync, **p0);
      (*p0)->write(ofs, do_unsync);
    }

    // Done -- close on exit.
  }

  enum class emplace_strategy {
    only_with_full_padding,
    reduce_padding_evenly
    // LATER(sp1ff):
    // favor_first
    // favor_id3v23
    // penalize_last
    // penalize_id322
  };

  enum class padding_strategy {
    adjust_padding_evenly
    // LATER(sp1ff):
    // favor_first
    // favor_id3v23
    // penalize_last
    // penalize_id322
  };

  /**
   * \brief Adjust the padding in a tagset so that it occupies a desired number
   * of bytes
   *
   *
   * \param p0 [in] an interator referencing the beginning of a range of
   * pointers to id3v2_tag
   *
   * \param p1 [in] an interator referencing the one-past-the-end element of a
   * range of pointers to id3v2_tag
   *
   * \param cb_new [in] the desired serialized size of the tagset
   *
   * \param cb_curr [in] the current serialized size of the tagset
   *
   * \param strat [in] a member of the padding_strategy enumeration describing
   * how the padding in [p0, p1) shall be adjusted in order to make its
   * serialized size equal to \a cb_new
   *
   *
   */

  template <typename forward_input_iterator>
  void
  adjust_padding_to(forward_input_iterator p0,
                    forward_input_iterator p1,
                    size_t                 cb_new,
                    size_t                 cb_curr,
                    padding_strategy       strat)
  {
    if (strat != padding_strategy::adjust_padding_evenly) {
      throw std::logic_error("adjust_padding_evenly is the only padding "
                             "strategy currently supported");
    }

    ptrdiff_t total_adj = cb_new - cb_curr;
    ptrdiff_t num_tags = std::distance(p0, p1);
    ptrdiff_t adj = total_adj / num_tags, mu = total_adj % num_tags;

    size_t pad = (*p0)->padding();
    if (adj < 0 && -(adj + mu) > pad) {
      throw invalid_tagset_request(invalid_tagset_request::adj_pad_not_enough);
    }
    (*p0)->padding(pad + adj + mu);
    ++p0;

    for ( ; p0 != p1; ++p0) {
      pad = (*p0)->padding();
      if (adj < 0 && -adj > pad) {
        throw invalid_tagset_request(invalid_tagset_request::adj_pad_not_enough);
      }
      (*p0)->padding(pad + adj);
    }
  }

  /**
   * \brief Write a new tagset to a file, emplacing if possible, copying if
   * requested or need be
   *
   *
   * \param p0 [in] a forward input iterator dereferencing to an id3v2_tag*
   * (or any type for which operator-> resolves to an id3v2_tag&) pointing to
   * the first element in a range of tags to be written out
   *
   * \param p1 [in] an identical iterator pointint to the one-past-the-end
   * element of the range of tags to be written
   *
   * \param unsync [in] true indicates that the unsynchronisation scheme
   * shall be applied while writing [p0,p1) to \a pth
   *
   * \param estrat [in] emplace_strategy describing our strategy for emplacing
   * the tagset
   *
   * \param pstrat [in] padding_strategy describing how to redistribute padding
   * if we are emplacing
   *
   *
   */

  template <typename forward_input_iterator>
  void maybe_emplace_tagset(const boost::filesystem::path &pth,
                            forward_input_iterator p0,
                            forward_input_iterator p1,
                            apply_unsync unsync,
                            emplace_strategy estrat,
                            padding_strategy pstrat,
                            // TODO(sp1ff): would like this to be false
                            bool keep_backup_on_copy = true)
  {
    namespace fs = boost::filesystem;

    size_t curr_sz = tagset_size(pth);

    size_t subs_sz, subs_pad;
    std::tie(subs_sz, subs_pad) = tagset_sizes(p0, p1, unsync);

    if (subs_sz <= curr_sz) {
      // we can emplace
      if (subs_sz < curr_sz) {
        adjust_padding_to(p0, p1, curr_sz, subs_sz, pstrat);
      }
      replace_tagset_emplace(pth, p0, p1, unsync);
    }
    else if (curr_sz >= subs_sz - subs_pad &&
             emplace_strategy::only_with_full_padding != estrat) {
      // we can emplace, if we reduce the padding in [p0,p1)
      adjust_padding_to(p0, p1, curr_sz, subs_sz, pstrat);
      replace_tagset_emplace(pth, p0, p1, unsync);
    }
    else if (!fs::exists(pth)) {
      // Special case-- output file does not exist
      fs::ofstream ofs(pth, fs::ofstream::binary);
      for ( ; p0 != p1; ++p0) {
        (*p0)->write(ofs, detail::compute_apply_unsync(unsync, **p0));
      }

    }
    else {
      // no way-- we need to copy
      replace_tagset_copy(pth, p0, p1, unsync, keep_backup_on_copy);
    }

  }

} // End namespace scribbu.

#endif // not TAGSET_H_INCLUDED
