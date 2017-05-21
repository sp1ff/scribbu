#include "charsets.hh"

#include <unordered_map>


///////////////////////////////////////////////////////////////////////////////
//                                 utilities                                 //
///////////////////////////////////////////////////////////////////////////////

namespace {

  std::pair<bool, scribbu::encoding>
  encoding_from_locale_name(const std::string &name)
  {
    // TOOD: Write me, once I have an internet connection!
    return std::make_pair(false, scribbu::encoding::ASCII);
  }

}


///////////////////////////////////////////////////////////////////////////////
//                             class iconv_error                             //
///////////////////////////////////////////////////////////////////////////////

const char * scribbu::iconv_error::what() const noexcept
{
  if (! pwhat_) {
    std::stringstream stm;
    stm << "iconv failure: " << strerror(errno_);
    pwhat_.reset(new std::string(stm.str()));
  }
  return pwhat_->c_str();
}

std::string
scribbu::detail::iconv_specific::string_for_encoding(encoding enc,
													 on_no_encoding rsp
													 /*= on_no_encoding::fail*/)
{
  using namespace std;
  using scribbu::encoding;
  using scribbu::on_no_encoding;
  static const unordered_map<encoding, string, hash<encoding>> TBL{
    // Eurpoean languages
	{ encoding::ASCII,            "ASCII"            },
    { encoding::ISO_8859_1,       "ISO-8859-1"       },
    { encoding::ISO_8859_2,       "ISO-8859-2"       },
    { encoding::ISO_8859_3,       "ISO-8859-3"       },
    { encoding::ISO_8859_4,       "ISO-8859-4"       },
    { encoding::ISO_8859_5,       "ISO-8859-5"       },
    { encoding::ISO_8859_7,       "ISO-8859-7"       },
    { encoding::ISO_8859_9,       "ISO-8859-9"       },
    { encoding::ISO_8859_10,      "ISO-8859-10"      },
    { encoding::ISO_8859_13,      "ISO-8859-13"      },
    { encoding::ISO_8859_14,      "ISO-8859-14"      },
    { encoding::ISO_8859_15,      "ISO-8859-15"      },
    { encoding::ISO_8859_16,      "ISO-8859-16"      },
    { encoding::KOI8_R,           "KOI8-R"           },
    { encoding::KOI8_U,           "KOI8-U"           },
    { encoding::KOI8_RU,          "KOI8-RU"          },
    { encoding::CP1250,           "CP1250"           },
    { encoding::CP1251,           "CP1251"           },
    { encoding::CP1252,           "CP1252"           },
    { encoding::CP1253,           "CP1253"           },
    { encoding::CP1254,           "CP1254"           },
    { encoding::CP1257,           "CP1257"           },
    { encoding::CP850,            "CP850"            },
    { encoding::CP866,            "CP866"            },
    { encoding::CP1131,           "CP1131"           },
    { encoding::MacRoman,         "MacRoman"         },
    { encoding::MacCentralEurope, "MacCentralEurope" },
    { encoding::MacIceland,       "MacIceland"       },
    { encoding::MacCroatian,      "MacCroatian"      },
    { encoding::MacRomania,       "MacRomania"       },
    { encoding::MacCyrillic,      "MacCyrillic"      },
    { encoding::MacUkraine,       "MacUkraine"       },
    { encoding::MacGreek,         "MacGreek"         },
    { encoding::MacTurkish,       "MacTurkish"       },
    { encoding::Macintosh,        "Macintosh"        },
	// Semitic languages
	{ encoding::ISO_8859_6,       "ISO-8859-6"       },
    { encoding::ISO_8859_8,       "ISO-8859-8"       },
    { encoding::CP1255,           "CP1255"           },
    { encoding::CP1256,           "CP1256"           },
    { encoding::CP862,            "CP862"            },
    { encoding::MacHebrew,        "MacHebrew"        },
    { encoding::MacArabic,        "MacArabic"        },
	// Japanese
	{ encoding::EUC_JP,           "EUC-JP"           },
    { encoding::SHIFT_JIS,        "SHIFT-JIS"        },
    { encoding::CP932,            "CP932"            },
    { encoding::ISO_2022_JP,      "ISO-2022-JP"      },
    { encoding::ISO_2022_JP_2,    "ISO-2022-JP-2"    },
    { encoding::ISO_2022_JP_1,    "ISO-2022-JP-1"    },
    { encoding::ISO_2022_JP_MS,   "ISO-2022-JP-MS"   },
	// Chinese
	{ encoding::EUC_CN,           "EUC-CN"           },
    { encoding::HZ,               "HZ"               },
    { encoding::GBK,              "GBK"              },
    { encoding::CP936,            "CP936"            },
    { encoding::GB18030,          "GB18030"          },
    { encoding::EUC_TW,           "EUC-TW"           },
    { encoding::BIG5,             "BIG5"             },
    { encoding::CP950,            "CP950"            },
    { encoding::BIG5_HKSCS,       "BIG5-HKSCS"       },
    { encoding::BIG5_HKSCS_2004,  "BIG5-HKSCS:2004"  },
    { encoding::BIG5_HKSCS_2001,  "BIG5-HKSCS:2001"  },
    { encoding::BIG5_HKSCS_1999,  "BIG5-HKSCS:1999"  },
    { encoding::ISO_2022_CN,      "ISO-2022-CN"      },
    { encoding::ISO_2022_CN_EXT,  "ISO-2022-CN-EXT"  },
	// Korean
	{ encoding::EUC_KR,           "EUC-KR"           },
    { encoding::CP949,            "CP949"            },
    { encoding::ISO_2022_KR,      "ISO-2022-KR"      },
    { encoding::JOHAB,            "JOHAB"            },
	// Armenian
	{ encoding::ARMSCII_8,        "ARMSCII-8"        },
	// Georgian
	{ encoding::Georgian_Academy, "Georgian-Academy" },
    { encoding::Georgian_PS,      "Georgian-PS"      },
	// Tajik
	{ encoding::KOI8_T,           "KOI8-T"           },
	// Kazakh
	{ encoding::PT154,            "PT154"            },
    { encoding::RK1048,           "RK1048"           },
	// Thai
	{ encoding::TIS_620,          "TIS-620"          },
    { encoding::CP874,            "CP874"            },
    { encoding::MacThai,          "MacThai"          },
	// Laotian
	{ encoding::MuleLao_1,        "MuleLao-1"        },
    { encoding::CP1133,           "CP1133"           },
	// Vietnamese
	{ encoding::VISCII,           "VISCII"           },
    { encoding::TCVN,             "TCVN"             },
    { encoding::CP1258,           "CP1258"           },
	// Platform specifics
	{ encoding::HP_ROMAN8,        "HP-ROMAN8"        },
    { encoding::NEXTSTEP,         "NEXTSTEP"         },
	// Full Unicode
	{ encoding::UTF_8,            "UTF-8"            },
    { encoding::UCS_2,            "UCS-2"            },
    { encoding::UCS_2BE,          "UCS-2BE"          },
    { encoding::UCS_2LE,          "UCS-2LE"          },
    { encoding::UCS_4,            "UCS-4"            },
    { encoding::UCS_4BE,          "UCS-4BE"          },
    { encoding::UCS_4LE,          "UCS-4LE"          },
	{ encoding::UTF_16,           "UTF-16"           },
    { encoding::UTF_16BE,         "UTF-16BE"         },
    { encoding::UTF_16LE,         "UTF-16LE"         },
    { encoding::UTF_32,           "UTF-32"           },
    { encoding::UTF_32BE,         "UTF-32BE"         },
    { encoding::UTF_32LE,         "UTF-32LE"         },
	{ encoding::UTF_7,            "UTF-7"            },
    { encoding::C99,              "C99"              },
    { encoding::JAVA,             "JAVA"             },
  };
  static const unordered_map<on_no_encoding,
                             string,
                             hash<on_no_encoding>> SFX {
    { on_no_encoding::fail,          ""           },
    { on_no_encoding::transliterate, "//TRANSLIT" },
    { on_no_encoding::ignore,        "//IGNORE"   },
  };

  return TBL.at(enc) + SFX.at(rsp);
}

char*
scribbu::detail::iconv_specific::str_for_encoding(encoding enc,
												  on_no_encoding rsp
												  /*= on_no_encoding::fail*/)
{
  std::string s = string_for_encoding(enc, rsp);
  return strdup(s.c_str());
}

namespace scribbu {

  encoding encoding_from_system_locale() {

	using namespace std;

	bool ok;
	encoding enc;

	locale loc("");
	tie(ok, enc) = encoding_from_locale_name(loc.name());
	if (ok) {
	  return enc;
	}

	// tie(ok, enc) = encoding_from_locale_name(/* TOOD: Get $LANG */);
	// if (ok) {
	//   return enc;
	// }

	throw range_error("No matching encoding");
  }

  template <>
  std::string
  convert_encoding(const unsigned char *pbuf,
				   std::size_t cbbuf,
				   encoding srcenc,
				   encoding dstenc,
				   on_no_encoding rsp /*= on_no_encoding::fail*/)
  {
    // TODO: Check `dstenc' to be sure the code unit is 8 bits wide

    detail::iconv_specific::descriptor dsc(srcenc, dstenc, rsp);

    char *inbuf = const_cast<char*>(reinterpret_cast<const char*>(pbuf));
    std::size_t inbytesleft = cbbuf;

    // We can't know a priori how many octets the output buffer will require;
    // cf.
    // http://stackoverflow.com/questions/13297458/simple-utf8-utf16-string-conversion-with-iconv
    std::size_t cbout = cbbuf << 2;
    std::unique_ptr<char []> poutbuf(new char[cbout]);

    // "The iconv function converts one multibyte character at a time, and for
    // each character conversion it increments *inbuf and decrements *inbytesleft
    // by the number of converted input bytes, it increments *outbuf and
    // decrements *outbytesleft by the number of converted output bytes, and it
    // updates the conversion state contained in cd."
    std::size_t outbytesleft = cbout;
    char *outbuf = poutbuf.get();
    std::size_t status = iconv(dsc, &inbuf, &inbytesleft,
                               &outbuf, &outbytesleft);
    while (~0 == status && E2BIG == errno) {
      // If the "output buffer has no more room for the next converted
      // character. In this case it sets errno to E2BIG and returns
      // (size_t)(−1)." Try again with a bigger buffer :P
      cbout <<= 2;
      poutbuf.reset(new char[cbout]);

      inbuf = const_cast<char*>(reinterpret_cast<const char*>(pbuf));
      inbytesleft = cbbuf;
      outbytesleft = cbout;
      char *outbuf = poutbuf.get();
      status = iconv(dsc, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    }

    if (~0 == status) {
      throw iconv_error(errno);
    }

    // The GNU iconv docs don't say anything about Unicode BOMs. Empirically,
    // if there's a BOM present in the input text, `iconv' will produce a BOM
    // in the output text. That's not what we want, in this case.
    outbuf = poutbuf.get();
    cbout -= outbytesleft;

    if ((3 < cbout) &&
        ((0x00 == (unsigned char)outbuf[0] &&
          0x00 == (unsigned char)outbuf[1] &&
          0xfe == (unsigned char)outbuf[2] &&
          0xff == (unsigned char)outbuf[3]) ||
         (0xfe == (unsigned char)outbuf[0] &&
          0xff == (unsigned char)outbuf[1] &&
          0x00 == (unsigned char)outbuf[2] &&
          0x00 == (unsigned char)outbuf[3]))) {
      outbuf += 4;
      cbout -= 4;
    }
    else if (2 < cbout  &&
             0xef == (unsigned char)outbuf[0] &&
             0xbb == (unsigned char)outbuf[1] &&
             0xbf == (unsigned char)outbuf[2]) {
      outbuf += 3;
      cbout -= 3;
    }
    else if ((1 < cbout) &&
             ((0xfe == (unsigned char)outbuf[0] &&
               0xff == (unsigned char)outbuf[1]) ||
              (0xff == (unsigned char)outbuf[0] &&
               0xfe == (unsigned char)outbuf[1]))) {
      outbuf += 2;
      cbout -= 2;
    }

    // If there are trailing nulls and/or spaces, don't copy them, either.
    while (cbout && (0 == outbuf[cbout - 1] || 32 == outbuf[cbout - 1])) {
      --cbout;
    }

    return std::string(outbuf, outbuf + cbout);

  } // End convert_encoding<string>.

}
