/**
 * \file scheme-serde.hh
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

#ifndef SCHEME_SERDE_HH_INCLUDED
#define SCHEME_SERDE_HH_INCLUDED 1

#include <memory>
#include <unordered_map>

#include <libguile.h>

#include <scribbu/framesv22.hh>
#include <scribbu/framesv23.hh>
#include <scribbu/framesv24.hh>

namespace scribbu {

  /**
   * \brief take a C++ range & make a Scheme list out of it
   *
   *
   * \param p0 [in] a forward input iterator defining the beginning of a range
   * of SCM
   *
   * \param p1 [in] a forward input iterator referencing the one-past-the-end
   * element of the range of SCM
   *
   * \return a SCM containing a Scheme list whose elements are [p0, p1)
   *
   *
   */

  template <typename forward_input_iterator>
  SCM
  scm_list_for_range(forward_input_iterator p0,
                     forward_input_iterator p1)
  {
    SCM lst = SCM_EOL;
    while (p0 != p1) {
      SCM args = scm_list_2(lst, scm_list_1(*p0++));
      lst = scm_append(args);

    }
    return lst;
  }

  inline
  SCM
  sym_for_utf8(const char *s)
  {
    return scm_string_to_symbol(scm_from_utf8_string(s));
  }

  /**
   * \brief Utility class for converting frames between Scheme & C++ in a
   * type-safe manner
   *
   *
   * In the context of this class, DEserialization meands converting from C++ to
   * Scheme. SERialization means going from Scheme to C++ (i.e. think from the
   * perspective of the Scheme programmer).
   *
   *
   */

  class scheme_serde_dispatcher
  {
  public:
    typedef std::unique_ptr<scribbu::id3v2_2_frame> (*pfn_from_scm_2_2)(SCM);
    typedef std::unique_ptr<scribbu::id3v2_3_frame> (*pfn_from_scm_2_3)(SCM);
    typedef std::unique_ptr<scribbu::id3v2_4_frame> (*pfn_from_scm_2_4)(SCM);

  public:
    /// ctor will register all serde implementations
    scheme_serde_dispatcher();

    //////////////////////////////////////////////////////////////////////////////
    //                           registration funcions                          //
    //////////////////////////////////////////////////////////////////////////////

    template <typename parameter,
              SCM (*pfn)(const parameter&, bool)>
    void reg_de_2_2(const scribbu::frame_id3 &id) {
      struct local {
        static SCM trampoline(const scribbu::id3v2_2_frame &f, bool unsync)
        { return pfn(dynamic_cast<const parameter&>(f), unsync); }
      };
      de_2_2_[id] = &local::trampoline;
    }

    template <typename parameter,
              SCM (*pfn)(const parameter&, bool)>
    void reg_de_2_3(const scribbu::frame_id4 &id) {
      struct local {
        static SCM trampoline(const scribbu::id3v2_3_frame &f, bool unsync)
        { return pfn(dynamic_cast<const parameter&>(f), unsync); }
      };
      de_2_3_[id] = &local::trampoline;
    }

    template <typename parameter,
              SCM (*pfn)(const parameter&, bool)>
    void reg_de_2_4(const scribbu::frame_id4 &id) {
      struct local {
        static SCM trampoline(const scribbu::id3v2_4_frame &f, bool unsync)
        { return pfn(dynamic_cast<const parameter&>(f), unsync); }
      };
      de_2_4_[id] = &local::trampoline;
    }

    void reg_ser_2_2(const std::string &id,
                     pfn_from_scm_2_2   pfn)
    { ser_2_2_[id] = pfn; }

    void reg_ser_2_3(const std::string &id,
                     pfn_from_scm_2_3   pfn)
    { ser_2_3_[id] = pfn; }

    void reg_ser_2_4(const std::string &id,
                     pfn_from_scm_2_4   pfn)
    { ser_2_4_[id] = pfn; }

    //////////////////////////////////////////////////////////////////////////////
    //                              SERDE functions                             //
    //////////////////////////////////////////////////////////////////////////////

    /// Deserialize an id3v2_2_frame to an <id3v2-frame>
    SCM de_2_2(const scribbu::id3v2_2_frame& frm, bool unsync) const;
    /// Deserialize an id3v2_3_frame to an <id3v2-frame>
    SCM de_2_3(const scribbu::id3v2_3_frame& frm, bool unsync) const;
    /// Deserialize an id3v2_34_frame to an <id3v2-frame>
    SCM de_2_4(const scribbu::id3v2_4_frame& frm, bool unsync) const;
    /// Serialize \a scm to an id3v2_2_frame
    std::unique_ptr<scribbu::id3v2_2_frame>
    ser_2_2(SCM scm) const;
    /// Serialize \a scm to an id3v2_3_frame
    std::unique_ptr<scribbu::id3v2_3_frame>
    ser_2_3(SCM scm) const;
    /// Serialize \a scm to an id3v2_4_frame
    std::unique_ptr<scribbu::id3v2_4_frame>
    ser_2_4(SCM scm) const;

  private:

    /// Fallback deserialization function
    SCM de_unknown_frame_2_2(const scribbu::id3v2_2_frame &f, bool unsync) const;
    /// Fallback deserialization function
    SCM de_unknown_frame_2_3(const scribbu::id3v2_3_frame &f, bool unsync) const;
    /// Fallback deserialization function
    SCM de_unknown_frame_2_4(const scribbu::id3v2_4_frame &f, bool unsync) const;
    /// Fallback serialization function
    std::unique_ptr<scribbu::id3v2_2_frame> ser_unknown_frame_2_2(SCM scm) const;
    /// Fallback serialization function
    std::unique_ptr<scribbu::id3v2_3_frame> ser_unknown_frame_2_3(SCM scm) const;
    /// Fallback serialization function
    std::unique_ptr<scribbu::id3v2_4_frame> ser_unknown_frame_2_4(SCM scm) const;


    typedef SCM (*pfn_to_scm_2_2)(const scribbu::id3v2_2_frame&, bool);
    typedef SCM (*pfn_to_scm_2_3)(const scribbu::id3v2_3_frame&, bool);
    typedef SCM (*pfn_to_scm_2_4)(const scribbu::id3v2_4_frame&, bool);

    std::unordered_map<scribbu::frame_id3, pfn_to_scm_2_2> de_2_2_;
    std::unordered_map<scribbu::frame_id4, pfn_to_scm_2_3> de_2_3_;
    std::unordered_map<scribbu::frame_id4, pfn_to_scm_2_4> de_2_4_;

    std::unordered_map<std::string, pfn_from_scm_2_2> ser_2_2_;
    std::unordered_map<std::string, pfn_from_scm_2_3> ser_2_3_;
    std::unordered_map<std::string, pfn_from_scm_2_4> ser_2_4_;

  }; // End class scheme_serde_dispatcher.

  void init_symbols();

  extern SCM sym_as_needed;
  extern SCM kw_apply_unsync;
  extern SCM kw_copy;

} // End namespace scribbu.

#endif /* SCHEME_SERDE_HH_INCLUDED */
