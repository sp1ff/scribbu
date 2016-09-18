#ifndef SCRIBBU_H_INCLUDED
#define SCRIBBU_H_INCLUDED 1

#include <iconv.h>
#include <iomanip>
#include <sstream>
#include <boost/filesystem.hpp>
#include <openssl/err.h>

namespace scribbu {

  /// Library initialization
  void static_initialize();

  /// Library cleanup
  void static_cleanup();

  /**
   * \class file_info
   *
   * \brief Information about a music track independent of tags or
   * content
   *
   *
   * Instantiate this class with a path naming a musical track; the
   * ctor will gather assorted data about the filesystem entity (as
   * opposed to ID3 tags, or information derived from the track data
   * itself).
   *
   * \todo Add atime & mtime
   *
   *
   */

  class file_info {

  public:
    file_info()
    { }
    file_info(boost::filesystem::path pth);

  public:
    /// Absolute path of the directory containing this file
    boost::filesystem::path parent() const
    { return parent_; }
    /// Filename (stem plus extension, if any) for this track
    boost::filesystem::path filename() const
    { return filename_; }
    /// The size, in bytes, of this file
    std::uintmax_t size() const
    { return size_; }

  private:
    boost::filesystem::path parent_;
    boost::filesystem::path filename_;
    std::uintmax_t size_;

  };

  /**
   * \brief Open a file & return a stream & a file_info instance
   * describing that file
   *
   *
   */

  std::pair<std::unique_ptr<std::istream>, file_info>
  open_file(boost::filesystem::path pth);

  class openssl_error: public virtual boost::exception,
                       public virtual std::runtime_error
  {
  public:
    openssl_error(): std::runtime_error(""), err_(ERR_get_error())
    { }
    virtual const char * what() const noexcept;
  private:
    unsigned long err_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  class iconv_error: public virtual boost::exception,
                     public virtual std::runtime_error
  {
  public:
    iconv_error(int err):
      std::runtime_error(""), errno_(err)
    { }
    int get_errno() const
    { return errno_; }
    virtual const char * what() const noexcept;

  private:
    int errno_;
    mutable std::shared_ptr<std::string> pwhat_;

  };

  namespace detail {

    /// guard class for iconv descriptors \todo Move this out of 'detail'
    class iconv_guard {
    public:
      iconv_guard(const char *tocode, const char *fromcode)
      {
        using std::stringstream;
        cd_ = iconv_open(tocode, fromcode);
        if ((iconv_t)-1 == cd_) {
          throw iconv_error(errno);
        }
      }
      ~iconv_guard() {
        if (-1 == iconv_close(cd_)) {
          throw iconv_error(errno);
        }
      }
      operator iconv_t() const {
        return cd_;
      }
    private:
      iconv_t cd_;
    }; // End class iconv_guard.

    // TODO: Re-design this API, once I figure out the details in all versions
    // of the ID3v2 spec
    std::string to_utf8(iconv_t              cd,
                        const unsigned char *pbuf,
                        std::size_t          cbbuf);

    /**
     * \brief Insert a user-defined type (AKA UDT) into an IOStreams output
     * stream
     *
     *
     * \param os [in,out] The IOStreams basic_ostream into which the UDT
     * instance shall be inserted
     *
     * \param x [in] The UDT instance to be inserted
     *
     * \return \a os, after having been inserted
     *
     * \pre os.state() is ios_base::good; if it is not, this function shall
     * immediately return having taken no action
     *
     *
     * This function template is an attempt to capture all the requirements for
     * writing an insertion operator for user-defined types:
     *
     * - if the os is not good(), don't do anything
     * - carry out the so-called prefix & suffix operations
     * - respecting the exception mask & updating the stream state
     *
     * So long as \a insertable_type defines a method named print_on, this
     * method will handle all of the above.
     *
     *
     * \todo Need reference for the above
     *
     *
     */

    template <typename char_type,
              class    traits_type,
              class    insertable_type>
    std::basic_ostream<char_type, traits_type>&
    insert(std::basic_ostream<char_type, traits_type> &os,
           const insertable_type                      &x)
    {
      using namespace std;

      static const ios_base::iostate EXC_MASK =
        ios_base::failbit | ios_base::badbit | ios_base::eofbit;

      if (!os.good()) {
        return os;
      }

      ios_base::iostate exc_mask = os.exceptions();

      typename basic_ostream<char_type, traits_type>::sentry opfx(os);

      ios_base::iostate err = (ios_base::iostate) 0;

      if (opfx) {

        // Delegate to insertable_type's implementation. insertable_type is
        // assumed to signal failures by throwing
        try {
          os.exceptions(EXC_MASK);
          x.print_on(os);
          // Conform to convention; width is reset to zero after it is used
          os.width(0);
        }
        // Patch up the stream state...
        catch (const std::ios_base::failure &ex) {
          // Someone set the stream state to something other than `good';
          // exception should be thrown when we call `exceptions', below.
        }
        catch (const std::bad_alloc &ex) {
          // Per Langer & Kreft (TODO: Reference needed here), we interpret
          // memory allocation failure as a loss of stream integrity-- hence we
          // need to set ios_base::badbit.
          err = ios_base::badbit;
        }
        catch (const std::exception &ex) {
          err = ios_base::failbit;
        }

      }

      os.exceptions(exc_mask);
      if (err) {
        os.setstate(err);
      }

      return os;

    } // End function template insert.

    /**
     * \class ios_base_manipulator
     *
     * \brief Abstract base class for building manipulators that take
     * parameters & manipulate their target stream in terms ios_base functions
     *
     *
     * This class is an attempt to collect the generic rules around writing a
     * parameterized IOStreams manipulator:
     *
     * - don't act if the stream state is not good
     * - sets up an environment where subclasses can safely signal error by
     *   just throwing
     * - handles exceptions, sets stream state, respects exceptions mask
     *
     * Subclasses need to provide an implementation of the pure virutal
     * do_manipulation. There, they can use any parameters with which they were
     * constructed to manipulate the stream state. The stream's exception mask
     * will have been set to throw on badbit, failbit, or eofbit being set, and
     * the implementation may signal any additional errors by throwing; this
     * class' insertion operator will handle catching them, re-setting the
     * exception mask, and patching up the stream state.
     *
     *
     * \note If the subclass is using the ios_base iword and/or pword indicies,
     * and needs to manage resources thereby, the subclass is solely
     * responsible for registering the callback function
     *
     *
     */

    class ios_base_manipulator
    {
    public:
      template <typename char_type, typename char_traits> friend
      std::basic_ostream<char_type, char_traits>&
      operator<<(std::basic_ostream<char_type, char_traits> &os,
                 const ios_base_manipulator                 &M)
      {
        if (os.good()) {
          M.manipulate(os);
        }
        return os;
      }
      virtual ~ios_base_manipulator()
      { }

    protected:
      virtual void do_manipulation(std::ios_base &ios) const = 0;

    private:
      template <typename char_type, typename char_traits>
      void manipulate(std::basic_ostream<char_type, char_traits>& os) const
      {
        // The logic here is very similar to that in 'insert', above, except
        // for the following:
        // 1. No sentry use
        // 2. A different core operation (invoke do_manipulation on 'this',
        // rather than print_on on an argument).
        // Might consider re-factoring once I am sure the logic is correct in
        // each case.
        using namespace std;

        static const ios_base::iostate EXC_MASK =
          ios_base::failbit | ios_base::badbit | ios_base::eofbit;

        ios_base::iostate exc_mask = os.exceptions();

        ios_base::iostate err = (ios_base::iostate) 0;

        try {
          os.exceptions(EXC_MASK);
          do_manipulation(os);
        }
        // Patch up the stream state...
        catch (const std::ios_base::failure &ex) {
          // Someone set the stream state to something other than `good';
          // exception should be thrown when we call `exceptions', below.
        }
        catch (const std::bad_alloc &ex) {
          // Per Langer & Kreft (TODO: Reference needed here), we interpret
          // memory allocation failure as a loss of stream integrity-- hence we
          // need to set ios_base::badbit.
          err = ios_base::badbit;
        }
        catch (const std::exception &ex) {
          err = ios_base::failbit;
        }

        os.exceptions(exc_mask);
        if (err) {
          os.setstate(err);
        }

      }

    }; // End class ios_base_manipulator.

    namespace experimental {

      /**
       * \class discriminated_subclass_manipulator
       *
       * \brief iose_base_manipulator that uses the iword & pword to store a
       * discriminated subclass of a given base class
       *
       *
       * This is an exerimental attempt to centralize the logic for a number of
       * cases where I'm creating families of manipulators governing the output
       * of user-defined types according to various "styles" (e.g. compact,
       * comma-separated variable, verbose, &c). For each such user-defined
       * type, there is an enumeration describing the set of styles in which
       * that UDT may be printed, a base class gathering all common
       * functionality for manipulators for printing that base class, and a
       * subclass thereof corresponding to each enumeration value.
       *
       * This class will store the enumeration value in each stream's iword,
       * and the address of a concrete subclass in the pword.
       *
       *
       * \note \a base_formatter shall provide a function named index which
       * returns the iword/pword index
       *
       */

      template <typename iword_type,
                class base_formatter>
      class discriminated_subclass_manipulator: public ios_base_manipulator
      {
      public:
        discriminated_subclass_manipulator(iword_type iword):
          iword_(iword)
        { }

      private:
        static void callback(std::ios_base::event event,
                             std::ios_base       &ios,
                             int                  idx)
        {
          // TODO: Exceptions?
          if (std::ios_base::erase_event == event) {
            void*& pword = ios.pword(idx);
            // TODO: Why not dynamic_cast?
            delete static_cast<base_formatter*>(pword);
          }
          else if (std::ios_base::copyfmt_event == event) {
            void*& pword = ios.pword(idx);
            base_formatter *pold;
            // TODO: Why not dynamic_cast?
            if (pold = static_cast<base_formatter*>(pword)) {
              pword = pold->clone();
            }
          }
        }

      public:
        static
        std::tuple<iword_type, base_formatter*>
        get(std::ios_base &ios, int idx) {
          iword_type D = static_cast<iword_type>(ios.iword(idx));
          // TODO: Why not dynamic_cast?
          base_formatter *p = static_cast<base_formatter*>(ios.pword(idx));
          return std::make_tuple(D, p);
        }

      protected:
        virtual base_formatter* clone() const = 0;
        void do_manipulation(std::ios_base &ios) const {
          int index = base_formatter::index();
          void*& pword = ios.pword(index);
          if (!pword) {
            ios.register_callback(callback, index);
          }
          pword = clone();
          ios.iword(index) = static_cast<long>(iword_);
        }

      private:
        iword_type iword_;

      }; // End class discriminated_subclass_manipulator.

    } // End namespace experimental.

  } // End namespace detail.

  /////////////////////////////////////////////////////////////////////////////
  //                           class track_data                              //
  /////////////////////////////////////////////////////////////////////////////

  /// supported IOStreams output formats
  enum class track_data_format: int {
    compact, csv, standard
  };

  enum class file_size_units {
    bytes, kilobytes, megabytes, gigabytes, terabytes, petabytes
  };

  template <typename char_type>
  std::basic_string<char_type>
  format_size(std::size_t                  cb,
              file_size_units              units,
              const std::ctype<char_type> &C,
              bool                         print_units = true)
  {
    using namespace std;

    const double KILO_FACTOR = 1.024e+3;
    const double MEGA_FACTOR = 1.048576e+6;
    const double GIGA_FACTOR = 1.073741824e+9;
    const double TERA_FACTOR = 1.099511627776e+12;
    const double PETA_FACTOR = 1.125899906842624e+15;

    char_type B = C.widen('B');
    char_type K = C.widen('K');
    char_type M = C.widen('M');
    char_type G = C.widen('G');
    char_type T = C.widen('T');
    char_type P = C.widen('P');

    char_type U;
    std::basic_stringstream<char_type> stm;

    if (file_size_units::bytes == units) {
      U = B;
      stm << cb;
    }
    else if (file_size_units::kilobytes == units) {
      double x = (double) cb / KILO_FACTOR;
      stm << fixed << setprecision(2) << x;
      U = K;
    }
    else if (file_size_units::megabytes == units) {
      double x = (double) cb / MEGA_FACTOR;
      stm << fixed << setprecision(2) << x;
      U = M;
    }
    else if (file_size_units::gigabytes == units) {
      double x = (double) cb / GIGA_FACTOR;
      stm << fixed << setprecision(2) << x;
      U = G;
    }
    else if (file_size_units::terabytes == units) {
      double x = (double) cb / TERA_FACTOR;
      stm << fixed << setprecision(2) << x;
      U = T;
    }
    else if (file_size_units::petabytes == units) {
      double x = (double) cb / PETA_FACTOR;
      stm << fixed << setprecision(2) << x;
      U = P;
    }
    else {
      throw std::logic_error("Unknown file_size_units value in format_size");
    }

    if (print_units) stm << U;

    return stm.str();
  }

  namespace detail {

    class track_data_formatter:
      public detail::experimental::discriminated_subclass_manipulator<
      track_data_format, track_data_formatter>
    {
    public:
      track_data_formatter(track_data_format fmt,
                           file_size_units   units):
        discriminated_subclass_manipulator<
          track_data_format, track_data_formatter>(fmt),
        units_(units)
      { }
    public:
      file_size_units units() const
      { return units_; }
      static int index()
      {
        static const int my_index = std::ios_base::xalloc();
        return my_index;
      }
    private:
      file_size_units units_;
    };

  }

  class compact_track_data_formatter: public detail::track_data_formatter
  {
  public:
    compact_track_data_formatter(file_size_units units,
                                 char            sep = ','):
      track_data_formatter(track_data_format::compact, units),
      sep_(sep)
    { }
    char sep() const
    { return sep_; }
  protected:
    track_data_formatter* clone() const
    { return new compact_track_data_formatter(*this); }
  private:
    char sep_;
  };

  class csv_track_data_formatter: public detail::track_data_formatter
  {
  public:
    csv_track_data_formatter(file_size_units units,
                             char            sep = ','):
      track_data_formatter(track_data_format::csv, units),
      sep_(sep)
    { }
    char sep() const
    { return sep_; }
  protected:
    track_data_formatter* clone() const
    { return new csv_track_data_formatter(*this); }
  private:
    char sep_;
  };

  class standard_track_data_formatter: public detail::track_data_formatter
  {
  public:
    standard_track_data_formatter(file_size_units units,
                                  std::size_t     indent = 4):
      track_data_formatter(track_data_format::standard, units),
      indent_(indent)
    { }
    std::size_t indent() const
    { return indent_; }
  protected:
    track_data_formatter* clone() const
    { return new standard_track_data_formatter(*this); }
  private:
    std::size_t indent_;
  };

  /**
   * \brief Container for information about the track data itself
   *
   *
   * Construct with an istream positioned just after any ID3v2 tags.  It will
   * scan the track data from the current position to the ID3v1 tag, if any (or
   * the end if no ID3v1 tag is present). After construction, assorted
   * information about the track may be retrieved.
   *
   *
   */

  class track_data {

  public:
    /// mnemonic constant for the # of bytes in an MD5 checksum
    static const std::size_t DIGEST_SIZE = 16;
    /// default output format
    static const track_data_format DEF_FORMAT = track_data_format::standard;
    static const standard_track_data_formatter DEF_FORMATTER;
    /// Construct with an input stream positioned just after any ID3v2 tags; on
    /// return, the stream will be positioned to the first byte after all the
    /// track data has been consumed
    track_data(std::istream &is);
    /// Retrieve the MD5 checksum computed on the track data
    template <typename forward_output_iterator>
    void get_md5(forward_output_iterator p) const
    { std::copy(md5_.begin(), md5_.end(), p); }
    /// Retrieve the # of bytes in the track data
    std::size_t size() const
    { return size_; }

    ///////////////////////////////////////////////////////////////////////////
    //                           IOStreams output                            //
    ///////////////////////////////////////////////////////////////////////////
    template <class char_type, class char_traits>
    std::basic_ostream<char_type, char_traits>&
    print_on(std::basic_ostream<char_type, char_traits> &os) const
    {
      typedef detail::track_data_formatter M;

      track_data_format fmt;
      const M *pfmt;
      std::tie(fmt, pfmt) = M::get(os, M::index());

      if (!pfmt) {
        fmt = DEF_FORMAT;
        pfmt = &DEF_FORMATTER;
      }

      switch (fmt) {
        case track_data_format::compact: {
          const compact_track_data_formatter &F =
            dynamic_cast<const compact_track_data_formatter&>(*pfmt);
          print_on(os, F);
          break;
        }
        case track_data_format::csv: {
          const csv_track_data_formatter &F =
            dynamic_cast<const csv_track_data_formatter&>(*pfmt);
          print_on(os, F);
          break;
        }
        case track_data_format::standard: {
          const standard_track_data_formatter &F =
            dynamic_cast<const standard_track_data_formatter&>(*pfmt);
          print_on(os, F);
          break;
        }
      default: {
        std::stringstream stm;
        stm << "Unknown ID3v1 format: " << (int) fmt;
        throw std::logic_error(stm.str());
      }
      }

    }

  private:

    ///////////////////////////////////////////////////////////////////////////
    //                            compact output                             //
    ///////////////////////////////////////////////////////////////////////////
    template <class char_type, class char_traits>
    std::basic_ostream<char_type, char_traits>&
    print_on(std::basic_ostream<char_type, char_traits> &os,
             const compact_track_data_formatter         &F) const
    {
      static const char * const TRACK = "Track";

      using namespace std;

      char_type col = os.widen(':');
      char_type cpr = os.widen(')');
      char_type opr = os.widen('(');
      char_type zer = os.widen('0');

      const ctype<char_type> &C = use_facet<ctype<char_type>>(os.getloc());

      char_type track[sizeof(TRACK)+1];

      C.widen(TRACK, TRACK + sizeof(TRACK), track);
      track[sizeof(TRACK)] = (char_type) 0;

      unsigned char md5[DIGEST_SIZE];
      get_md5(md5);

      basic_stringstream<char_type, char_traits> stm;
      stm << hex << setfill(zer);
      for (size_t i = 0; i < DIGEST_SIZE; ++i) {
        stm << setw(2) << (unsigned) md5[i];
      }

      return os << track << col << stm.str() << opr <<
        format_size(size(), F.units(), C) << cpr;
    }

    ///////////////////////////////////////////////////////////////////////////
    //                              CSV output                               //
    ///////////////////////////////////////////////////////////////////////////
    template <class char_type, class char_traits>
    std::basic_ostream<char_type, char_traits>&
    print_on(std::basic_ostream<char_type, char_traits> &os,
             const csv_track_data_formatter         &F) const
    {
      using namespace std;

      char_type com = os.widen(',');
      char_type zer = os.widen('0');

      const ctype<char_type> &C = use_facet<ctype<char_type>>(os.getloc());

      unsigned char md5[DIGEST_SIZE];
      get_md5(md5);

      basic_stringstream<char_type, char_traits> stm;
      stm << hex << setfill(zer);
      for (size_t i = 0; i < DIGEST_SIZE; ++i) {
        stm << setw(2) << (unsigned) md5[i];
      }

      return os << stm.str() << com << format_size(size(), F.units(), C, false);
    }

    ///////////////////////////////////////////////////////////////////////////
    //                            standard output                            //
    ///////////////////////////////////////////////////////////////////////////
    template <class char_type, class char_traits>
    std::basic_ostream<char_type, char_traits>&
    print_on(std::basic_ostream<char_type, char_traits> &os,
             const standard_track_data_formatter         &F) const
    {
      static const char * const MD5   = " MD5: ";
      static const char * const SIZE  = "size: ";
      static const char TRACK2[] = "Track Data:";

      using namespace std;

      char_type spc = os.widen(' ');
      char_type zer = os.widen('0');

      const ctype<char_type> &C = use_facet<ctype<char_type>>(os.getloc());

      std::size_t cb = sizeof(TRACK2);
      char_type track[sizeof(TRACK2)+1];
      char_type csize[sizeof(SIZE) +1];
      char_type md5  [sizeof(MD5)  +1];

      // C.widen(TRACK, TRACK + sizeof(TRACK), track);
      C.widen(TRACK2, TRACK2 + cb, track);
      C.widen(SIZE,  SIZE  + sizeof(SIZE),  csize);
      C.widen(MD5,   MD5   + sizeof(MD5),   md5  );

      track[sizeof(TRACK2)] =
        csize[sizeof(SIZE)] =
        md5[sizeof(MD5) ] =
        (char_type) 0;

      char_type indent[F.indent() + 1];
      fill(indent, indent + F.indent(), spc);
      indent[F.indent()] = (char_type)0;

      unsigned char checksum[DIGEST_SIZE];
      get_md5(checksum);

      basic_stringstream<char_type, char_traits> stm;
      stm << hex << setfill(zer);
      for (size_t i = 0; i < DIGEST_SIZE; ++i) {
        stm << setw(2) << (unsigned) checksum[i];
      }

      return os << track << endl <<
        indent << md5 << stm.str() << endl <<
        indent << csize << format_size(size(), F.units(), C) << endl;
    }

  private:
    std::array<unsigned char, DIGEST_SIZE> md5_;
    std::size_t size_;

  };

  template <typename char_type, typename char_traits>
  std::basic_ostream<char_type, char_traits>&
  operator<<(std::basic_ostream<char_type, char_traits>& os, const track_data &x)
  {
    return detail::insert(os, x);
  }

  /**
   * \brief Escape a string for CSV output
   *
   *
   * \param s [in] the text to be escaped
   *
   * \param sep [in] the delimiter to be used
   *
   * \return \a s if \a sep does not appear in \a s, otherwise, "s" with all
   * occurences of '"' in \a s doubled
   *
   *
   * Take some text & escape it for output in CSV format. It is assumed that
   * the input & output text are UTF-8 encoded.
   *
   *
   */

  std::string
  escape_for_csv(const std::string &s,
                 char               sep);

} // End namespace scribbu.

#endif // not SCRIBBU_H_INCLUDED
