/**
 * \file ostream.hh
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

#ifndef OSTREAM_HH_INCLUDED
#define OSTREAM_HH_INCLUDED 1

#include <scribbu/scribbu.hh>
#include <scribbu/charsets.hh>

namespace scribbu {

  /**
   * \brief Generic basic_ostream insertion implementation
   *
   *
   * \param x [in] user-defined type to be inserted into an IOStreams output
   * stream
   *
   * \param F [in] a functor taking \a x & \a os & returning iostate; this is
   * presumed to do the actual printing
   *
   * \param os [in, out] basic_ostream into which \a x shall be inserted
   *
   * \return \a os after \a x has been inserted into it
   *
   *
   * Correctly implementing operator<< can be complex enough that I've emulated
   * \ref ref_02 "Langer & Kreft" and wrapped it up in an operator template.
   *
   * Considerations when implementing operator<<:
   *
   * 1. Prefix & suffix operations: IOStream insertion operators, by convention
   *    and by specification, carry out certain operations on entry to & exit
   *    from insertion operations. On entry, they flush any tied stream, and on
   *    exit they flush the buffer if the unitbuf flag is set. Implementations
   *    are allowed to peform additional operations at these points, such as
   *    acquiring & releasing locks.
   *
   *    The Standard provides for a nested class named sentry that encapsulates
   *    these operations.
   *
   * 2. Error handling: Operations invoked in the course of output can signal
   *    errors by throwing an exception, returning a failed status code, or
   *    setting the failbit or badbit in the stream's state flag, In the last
   *    case, this could result in an ios_base::failure being thrown (if the
   *    stream's exception mask calls for it).
   *
   *    In terms of the inserter's interface, the expectation in case of error
   *    is that:
   *
   *    - loss of stream integrity is indicated by setting badbit on return
   *
   *    - failure to insert the UDT into the stream is indicated by setting
   *      failbit on return
   *
   *    - if either of these bits is set in the stream's exception mask, the
   *      implementation should throw; as far as I can see, the Standard just
   *      requires us to throw an exception of some kind, not necessarily
   *      ios_base::failure. Langer & Kreft recommend re-throwing a more
   *      specific exception, where possible, as that will contain more
   *      meaningful information
   *
   * 3. Format control: the implementation should interpret the pre-defined
   *    formatting options in a manner generally constistent with the
   *    pre-defined inserters. These include:
   *
   *    - base (dec, oct, hex): the numeric base in which integers are output
   *
   *    - showbase: prefix integers with a symbol indicating base ("0x" for hex,
   *      e.g.); also affects the way money is output
   *
   *    - showpos: determines whether positive integers are indicated with the
   *      '+' sign, or not
   *
   *    - floating point format (scientific, fixed): floating-point format
   *
   *    - showpoint: affects whether the decimal point is always shown in
   *      floating point output
   *
   *    - precision: precision of floating point output
   *
   *    - uppercase: determines whether characters output in the course of
   *      printing numbers (floating point or integral) will be uppercase or
   *      lower.
   *
   *    - adjust (left, right, internal): justification within a field; left
   *      and right apply to any output, internal applies to integer,
   *      floating-point, and monetary output.
   *
   *    - width: field width (resets to zero after insertion)
   *
   *    - boolalpha: display booleans as 0/1 or true/false
   *
   * 4. Internationalisation: Numbers, dates & times, monetary values, and
   *    punctuation vary by country; use the stream's imbued locale for
   *    outputting such quantities. Non-standard facets may need to be defined
   *    for locale-dependent aspects of the user-defined type that aren't
   *    covered by the standard facets.
   *
   * This function template handles items 1 & 2 in this list; the functor is
   *  responsible for handling items 3 & 4.
   *
   * From 27.7.3.6.1 Formatted output functions, Common requirements
   * [ostream.formatted], [ostream.formatted.reqmts]:
   *
   * "Each formatted output function begins execution by constructing an object
   * of class sentry. If this object returns true when converted to a value of
   * type bool, the function endeavors to generate the requested output. If the
   * generation fails, then the formatted output function does
   * setstate(ios_base::failbit), which might throw an exception. If an
   * exception is thrown during output, then ios::badbit is turned on in
   * *this’s error state. If (exceptions()&badbit) != 0 then the exception is
   * rethrown. Whether or not an exception is thrown, the sentry object is
   * destroyed before leaving the formatted output function. If no exception is
   * thrown, the result of the formatted output function is *this."
   *
   * Let's take a look at how gcc implements their insertion operators:
   *
   \code

       ...
       *  @name Inserters
       *
       *  All the @c operator<< functions (aka <em>formatted output
       *  functions</em>) have some common behavior.  Each starts by
       *  constructing a temporary object of type std::basic_ostream::sentry.
       *  This can have several effects, concluding with the setting of a
       *  status flag; see the sentry documentation for more.
       *
       *  If the sentry status is good, the function tries to generate
       *  whatever data is appropriate for the type of the argument.
       *
       *  If an exception is thrown during insertion, ios_base::badbit
       *  will be turned on in the stream's error state without causing an
       *  ios_base::failure to be thrown.  The original exception will then
       *  be rethrown.
       ...

       template<typename _CharT, typename _Traits>
       template<typename _ValueT>
       basic_ostream<_CharT, _Traits>&
       basic_ostream<_CharT, _Traits>::
       _M_insert(_ValueT __v)
       {
         sentry __cerb(*this);
         if (__cerb)
           {
             ios_base::iostate __err = ios_base::goodbit;
             __try
               {
                 const __num_put_type& __np = __check_facet(this->_M_num_put);
                 if (__np.put(*this, *this, this->fill(), __v).failed())
                   __err |= ios_base::badbit;
               }
             __catch(__cxxabiv1::__forced_unwind&)
               {
                 this->_M_setstate(ios_base::badbit);
                 __throw_exception_again;
               }
             __catch(...)
               { this->_M_setstate(ios_base::badbit); }
             if (__err)
               this->setstate(__err);
           }
         return *this;
       }
   \endcode
   *
   * NB. _M_setstate will set the given bit in the stream state, but intead
   * of throwing ios_base::failure, will re-throw the current exception (if
   * the exception mask contains the given bit).
   *
   *
   */

  template <typename T,
            typename char_type,
            typename traits_type = std::char_traits<char_type>>
  std::basic_ostream<char_type, traits_type>&
  insert_into_ostream(
    const T &x,
    std::function<std::ios_base::iostate
                  (const T&, std::basic_ostream<char_type, traits_type>&)> F,
    std::basic_ostream<char_type, traits_type> &os)
  {
    using namespace std;

    // Flush the tied stream, if any. If not good(), set _M_ok to false & set
    // failbit. This will throw if 0!=(exceptions()&failbit), which is what we
    // want.
    typename basic_ostream<char_type, traits_type>::sentry cerb(os);

    ios_base::iostate err = ios_base::goodbit;

    if (cerb) {

      try {

        // Use the return value to indicate output generation failure
        // (failbit), as opoosed to loss of stream integrity (badbit). This is
        // because, per the Standard, any exceptions thrown will set badbit.
        err = f(x, os);
        os.width(0);

      }
      catch (...) {

        // An exception was thrown; per the Standard, set badbit & re-throw if
        // 0!=(exceptions()&badbit).
        try {
          os.setstate(ios_base::badbit);
        }
        catch (const ios_base::failure&) {
        }
        if (ios_base::badbit & os.exceptions()) {
          throw;
        }

        // Corner case-- we're here because someone set failbit and the
        // exception mask contained failbit. As I read the Standard, this
        // should *not* throw, but gcc's approach *does*-- I've chosen the
        // latter.
        if ((ios_base::failbit & os.exceptions()) &&
            (ios_base::failbit & os.rdstate())) {
          throw;
        }

      }
    }

    if (err) {
      os.setstate(err);
    }

    return os;

  } // End function template insert.

  namespace detail {

    /**
     * \brief Trivial sentry replacement
     *
     *
     * Use this class with manip_helper, below, when you want the insertion to
     * take place without prefix & suffic operations (in particular, if you
     * want the manipulator to be inserted/extracted regardless of the stream
     * state).
     *
     *
     */

    template <typename char_type,
              typename traits_type = std::char_traits<char_type>>
    struct trivial_sentry {
      trivial_sentry(std::basic_ios<char_type, traits_type>&)
      { }
      operator bool() const {
        return true;
      }
    };

  }

  /**
   * \brief Helper struct for authoring stream manipulators
   *
   *
   * This struct exists to concentrate in one place the logic for inserting or
   * extracting a manipulator from a basic_ostream or basic_istream,
   * respectively.
   *
   *
   */

  template <typename manip_type,
            typename stream_type,
            typename sentry_type = typename stream_type::sentry>
  struct manip_helper {

    typedef std::function<void (manip_type&, stream_type&)> functor_type;

    static stream_type& do_manip(manip_type  &manip,
                                 functor_type F,
                                 stream_type &os)
    {
      using namespace std;

      ios_base::iostate err = ios_base::goodbit;

      sentry_type cerb(os);

      if (cerb) {

        // This manipulator doesn't actually insert anything into the stream
        // buffer, so go ahead regardless of the stream state, and don't bother
        // constructing a sentry instance.
        try {
          F(manip, os);
        }
        catch (...) {

          // An exception was thrown; per the Standard, set badbit & re-throw
          // if 0!=(exceptions()&badbit).
          try {
            os.setstate(ios_base::badbit);
          }
          catch (const ios_base::failure&) {
          }
          if (ios_base::badbit & os.exceptions()) {
            throw;
          }
          // Corner case-- we're here because someone set failbit and the
          // exception mask contained failbit. As I read the Standard, this
          // should *not* throw, but gcc's approach *does*-- I've chosen the
          // latter.
          if ((ios_base::failbit & os.exceptions()) &&
              (ios_base::failbit & os.rdstate())) {
            throw;
          }
        }

      }

      if (err) {
        os.setstate(err);
      }

      return os;

    } // End operator().

  }; // End class template insert_manip.

  /// Embue an osteram with an encoding scheme
  class ienc
  {
  public:
    ienc(encoding enc, on_no_encoding rsp): enc_(enc), rsp_(rsp)
    { }

    static ienc retrieve(std::ios_base&);

    encoding get_encoding() const {
      return enc_;
    }
    on_no_encoding get_on_no_encoding() const {
      return rsp_;
    }

  private:
    template <typename char_type, typename traits_type>
    friend
    std::basic_ostream<char_type, traits_type>&
    operator<<(std::basic_ostream<char_type, traits_type> &os, const ienc &x);

    template <typename char_type, typename traits_type>
    friend
    std::basic_istream<char_type, traits_type>&
    operator>>(std::basic_istream<char_type, traits_type> &is, const ienc &x);

    static int index();
    static void insert(const ienc &manip, std::ios_base &ios);
    static void callback(std::ios_base::event e, std::ios_base &ios, int idx);

    encoding enc_;
    on_no_encoding rsp_;
  };

  template <typename char_type, typename traits_type>
  std::basic_ostream<char_type, traits_type>&
  operator<<(std::basic_ostream<char_type, traits_type> &os, const ienc &x);

  template <typename char_type, typename traits_type>
  std::basic_istream<char_type, traits_type>&
  operator>>(std::basic_istream<char_type, traits_type> &is, const ienc &x);

} // End namespace scribbu.

#endif // OSTREAM_HH_INCLUDED
