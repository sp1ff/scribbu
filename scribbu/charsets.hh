#ifndef CHARSETS_HH_INCLUDED
#define CHARSETS_HH_INCLUDED
#include <locale>
#include <string>

#include <boost/exception/all.hpp>

#include <iconv.h>

#include <scribbu/errors.hh>

namespace scribbu {

  class iconv_error: public virtual boost::exception,
                     public virtual std::runtime_error
  {
  public:
    iconv_error(int err): std::runtime_error(""), errno_(err)
    { }
    int get_errno() const
    { return errno_; }
    virtual const char * what() const noexcept;

  private:
    int errno_;
    mutable std::shared_ptr<std::string> pwhat_;

  };

  enum class encoding {
	// Eurpoean & Russian languages
	ASCII, ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
    ISO_8859_7, ISO_8859_9, ISO_8859_10, ISO_8859_13, ISO_8859_14,
    ISO_8859_15, ISO_8859_16, KOI8_R, KOI8_U, KOI8_RU, CP1250, CP1251,
    CP1252, CP1253, CP1254, CP1257, CP850, CP866, CP1131, MacRoman,
    MacCentralEurope, MacIceland, MacCroatian, MacRomania, MacCyrillic,
    MacUkraine, MacGreek, MacTurkish, Macintosh,

	// Semitic languages
	ISO_8859_6, ISO_8859_8, CP1255, CP1256, CP862, MacHebrew, MacArabic,

	// Japanese
	EUC_JP, SHIFT_JIS, CP932, ISO_2022_JP, ISO_2022_JP_2, ISO_2022_JP_1,
    ISO_2022_JP_MS,

	// Chinese
	EUC_CN, HZ, GBK, CP936, GB18030, EUC_TW, BIG5, CP950, BIG5_HKSCS,
    BIG5_HKSCS_2004, BIG5_HKSCS_2001, BIG5_HKSCS_1999, ISO_2022_CN,
    ISO_2022_CN_EXT,

	// Korean
	EUC_KR, CP949, ISO_2022_KR, JOHAB,
	// Armenian
	ARMSCII_8,
	// Georgian
	Georgian_Academy, Georgian_PS,
	// Tajik
	KOI8_T,
	// Kazakh
	PT154, RK1048,
	// Thai
	TIS_620, CP874, MacThai,
	// Laotian
	MuleLao_1, CP1133,
	// Vietnamese
	VISCII, TCVN, CP1258,
	// Platform specifics
	HP_ROMAN8, NEXTSTEP,
	// Full Unicode
	UTF_8, UCS_2, UCS_2BE, UCS_2LE, UCS_4, UCS_4BE, UCS_4LE,
	UTF_16, UTF_16BE, UTF_16LE, UTF_32, UTF_32BE, UTF_32LE,
	UTF_7, C99, JAVA,
  };

  std::istream& operator>>(std::istream &is, encoding &x);
  std::ostream& operator<<(std::ostream &os, const encoding &x);

  template <typename char_type>
  struct char_traits
  {
    /// Returns true if char_type is a code unit for encoding \a x
    static bool is_code_unit(encoding x);
  };

  class bad_code_unit: public error {
  public:
    bad_code_unit(encoding enc, std::size_t cb);

  private:
    encoding enc_;
    std::size_t cb_;
  };

  /// Response when a byte sequence in the source encoding cannot
  /// be represented in the target encoding
  enum class on_no_encoding {
    fail, transliterate, ignore
  };

  namespace detail {

    ///////////////////////////////////////////////////////////////////////////
    //                        iconv-specific details                         //
    ///////////////////////////////////////////////////////////////////////////

    namespace iconv_specific {

      std::string string_for_encoding(encoding enc,
                                      on_no_encoding rsp =
                                      on_no_encoding::fail);
      char* str_for_encoding(encoding enc,
                             on_no_encoding rsp =
                             on_no_encoding::fail);

      class descriptor {

      public:
        descriptor(encoding from, encoding to,
				   on_no_encoding rsp = on_no_encoding::fail):
          from_(from), to_(to) , rsp_(rsp)
        {
          dsc_ = iconv_open(string_for_encoding(to_, rsp_).c_str(),
                            string_for_encoding(from_).c_str());
          if ((iconv_t)-1 == dsc_) {
            throw iconv_error(errno);
          }
        }
        ~descriptor() {
          // Return value intentionally ignored, as we're in a dtor
          iconv_close(dsc_);
        }
        operator iconv_t() const {
          return dsc_;
        }

      private:
        descriptor(const descriptor&) = delete;
        descriptor& operator=(const descriptor&) = delete;

      private:
        encoding from_;
        encoding to_;
        on_no_encoding rsp_;
        iconv_t dsc_;

      };

    } // End namespace iconv_specific.

  } // End namespace detail.

  /**
   * \brief Derive an encoding from the current system locale
   *
   *
   * TODO: I'm writing this in a plane over the Pacific, with no network
   * connection, so this is subject to revision!
   *
   *
   * This function attempts to guess the character encoding in use on the
   * system currently in the following way:
   *
   *   - construct an std::locale with the empty string as argument
   *
   *   - if that instance's name appears to contain a character encoding, and
   *     that encoding can be mapped to a member of the scribbu::encoding
   *     enumeration, return it
   *
   *   - otherwise, examine the LANG environment variable in the same way
   *
   *
   */

  encoding encoding_from_system_locale();

  /**
   * Template parameters can't be deduced from return values, so you'll have
   * to call this like:
   *
   \code

     std::string x = convert_encoding<std::string>(...)

   \endcode
   *
   *
   */

  template<typename string_type>
  string_type convert_encoding(const unsigned char *pbuf,
                               std::size_t cbbuf,
                               encoding srcenc,
                               encoding dstenc,
                               on_no_encoding rsp = on_no_encoding::fail);

} // End namespace scribbu.

namespace std {

  template <>
  class hash<scribbu::encoding> {
  public:
    std::size_t operator()(const scribbu::encoding &x) const noexcept {
      return static_cast<std::size_t>(x);
    }
  };

  template <>
  class hash<scribbu::on_no_encoding> {
  public:
    std::size_t
    operator()(const scribbu::on_no_encoding &x) const noexcept {
      return static_cast<std::size_t>(x);
    }
  };

}

#endif // not CHARSETS_HH_INCLUDED
