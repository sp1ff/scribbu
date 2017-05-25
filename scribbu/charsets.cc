#include "charsets.hh"

#include <unordered_map>
#include <unordered_set>


///////////////////////////////////////////////////////////////////////////////
//                                 utilities                                 //
///////////////////////////////////////////////////////////////////////////////

namespace {

  std::pair<bool, scribbu::encoding>
  encoding_from_locale_name(const std::string &name)
  {
    using namespace std;
    using namespace scribbu;

    static const unordered_map<string, encoding> TBL {
      {"ASCII",      encoding::ASCII       },
      {"US-ASCII",   encoding::ASCII       },
      {"UTF-8",      encoding::UTF_8       },
      {"utf8",       encoding::UTF_8       },
      {"ISO8859-1",  encoding::ISO_8859_1  },
      {"ISO8859-2",  encoding::ISO_8859_2  },
      {"ISO8859-3",  encoding::ISO_8859_3  },
      {"ISO8859-4",  encoding::ISO_8859_4  },
      {"ISO8859-5",  encoding::ISO_8859_5  },
      {"ISO8859-7",  encoding::ISO_8859_7  },
      {"ISO8859-9",  encoding::ISO_8859_9  },
      {"ISO8859-10", encoding::ISO_8859_10 },
      {"ISO8859-13", encoding::ISO_8859_13 },
      {"ISO8859-14", encoding::ISO_8859_14 },
      {"ISO8859-15", encoding::ISO_8859_15 },
      {"ISO8859-16", encoding::ISO_8859_16 },
      {"CP1131",     encoding::CP1131      },
      {"CP1133",     encoding::CP1133      },
      {"CP1250,",    encoding::CP1250      },
      {"CP1251",     encoding::CP1251      },
      {"CP1252,",    encoding::CP1252      },
      {"CP1253,",    encoding::CP1253      },
      {"CP1254,",    encoding::CP1254      },
      {"CP1255,",    encoding::CP1255      },
      {"CP1256,",    encoding::CP1256      },
      {"CP1257,",    encoding::CP1257      },
      {"CP1258",     encoding::CP1258      },
      {"CP850,",     encoding::CP850       },
      {"CP862,",     encoding::CP862       },
      {"CP866,",     encoding::CP866       },
      {"CP874",      encoding::CP874       },
      {"CP932",      encoding::CP932       },
      {"CP936",      encoding::CP936       },
      {"CP949",      encoding::CP949       },
      {"CP950",      encoding::CP950       },
    };

    string::size_type i = name.rfind('.');
    if (string::npos == i) {
      return make_pair(true, encoding::UTF_8);
    }

    string key = name.substr(i);
    if (0 == TBL.count(key)) {
      return make_pair(false, encoding::UTF_8);
    }

    return make_pair(true, TBL.at(key));
  }

}

namespace scribbu {

  std::istream&
  operator>>(std::istream &is,
             encoding &x)
  {
    using namespace std;
    static const unordered_map<string, encoding, hash<string>> TBL{
      // Eurpoean languages
      { "ASCII",           encoding::ASCII             },
      { "ISO-8859-1",      encoding::ISO_8859_1        },
      { "ISO-8859-2",      encoding::ISO_8859_2        },
      { "ISO-8859-3",      encoding::ISO_8859_3        },
      { "ISO-8859-4",      encoding::ISO_8859_4        },
      { "ISO-8859-5",      encoding::ISO_8859_5        },
      { "ISO-8859-7",      encoding::ISO_8859_7        },
      { "ISO-8859-9",      encoding::ISO_8859_9        },
      { "ISO-8859-10",     encoding::ISO_8859_10       },
      { "ISO-8859-13",     encoding::ISO_8859_13       },
      { "ISO-8859-14",     encoding::ISO_8859_14       },
      { "ISO-8859-15",     encoding::ISO_8859_15       },
      { "ISO-8859-16",     encoding::ISO_8859_16       },
      { "KOI8-R",          encoding::KOI8_R            },
      { "KOI8-U",          encoding::KOI8_U            },
      { "KOI8-RU",         encoding::KOI8_RU           },
      { "CP1250",          encoding::CP1250            },
      { "CP1251",          encoding::CP1251            },
      { "CP1252",          encoding::CP1252            },
      { "CP1253",          encoding::CP1253            },
      { "CP1254",          encoding::CP1254            },
      { "CP1257",          encoding::CP1257            },
      { "CP850",           encoding::CP850             },
      { "CP866",           encoding::CP866             },
      { "CP1131",          encoding::CP1131            },
      { "MacRoman",        encoding::MacRoman          },
      { "MacCentralEurope",encoding::MacCentralEurope  },
      { "MacIceland",      encoding::MacIceland        },
      { "MacCroatian",     encoding::MacCroatian       },
      { "MacRomania",      encoding::MacRomania        },
      { "MacCyrillic",     encoding::MacCyrillic       },
      { "MacUkraine",      encoding::MacUkraine        },
      { "MacGreek",        encoding::MacGreek          },
      { "MacTurkish",      encoding::MacTurkish        },
      { "Macintosh",       encoding::Macintosh         },
      // Semitic languages
      { "ISO-8859-6",      encoding::ISO_8859_6        },
      { "ISO-8859-8",      encoding::ISO_8859_8        },
      { "CP1255",          encoding::CP1255            },
      { "CP1256",          encoding::CP1256            },
      { "CP862",           encoding::CP862             },
      { "MacHebrew",       encoding::MacHebrew         },
      { "MacArabic",       encoding::MacArabic         },
      // Japanese
      { "EUC-JP",          encoding::EUC_JP            },
      { "SHIFT-JIS",       encoding::SHIFT_JIS         },
      { "CP932",           encoding::CP932             },
      { "ISO-2022-JP",     encoding::ISO_2022_JP       },
      { "ISO-2022-JP-2",   encoding::ISO_2022_JP_2     },
      { "ISO-2022-JP-1",   encoding::ISO_2022_JP_1     },
      { "ISO-2022-JP-MS",  encoding::ISO_2022_JP_MS    },
      // Chinese
      { "EUC-CN",          encoding::EUC_CN            },
      { "HZ",              encoding::HZ                },
      { "GBK",             encoding::GBK               },
      { "CP936",           encoding::CP936             },
      { "GB18030",         encoding::GB18030           },
      { "EUC-TW",          encoding::EUC_TW            },
      { "BIG5",            encoding::BIG5              },
      { "CP950",           encoding::CP950             },
      { "BIG5-HKSCS",      encoding::BIG5_HKSCS        },
      { "BIG5-HKSCS:2004", encoding::BIG5_HKSCS_2004   },
      { "BIG5-HKSCS:2001", encoding::BIG5_HKSCS_2001   },
      { "BIG5-HKSCS:1999", encoding::BIG5_HKSCS_1999   },
      { "ISO-2022-CN",     encoding::ISO_2022_CN       },
      { "ISO-2022-CN-EXT", encoding::ISO_2022_CN_EXT   },
      // Korean
      { "EUC-KR",          encoding::EUC_KR            },
      { "CP949",           encoding::CP949             },
      { "ISO-2022-KR",     encoding::ISO_2022_KR       },
      { "JOHAB",           encoding::JOHAB             },
      // Armenian
      { "ARMSCII-8",       encoding::ARMSCII_8         },
      // Georgian
      { "Georgian-Academy",encoding::Georgian_Academy  },
      { "Georgian-PS",     encoding::Georgian_PS       },
      // Tajik
      { "KOI8-T",          encoding::KOI8_T            },
      // Kazakh
      { "PT154",           encoding::PT154             },
      { "RK1048",          encoding::RK1048            },
      // Thai
      { "TIS-620",         encoding::TIS_620           },
      { "CP874",           encoding::CP874             },
      { "MacThai",         encoding::MacThai           },
      // Laotian
      { "MuleLao-1",       encoding::MuleLao_1         },
      { "CP1133",          encoding::CP1133            },
      // Vietnamese
      { "VISCII",          encoding::VISCII            },
      { "TCVN",            encoding::TCVN              },
      { "CP1258",          encoding::CP1258            },
      // Platform specifics
      { "HP-ROMAN8",       encoding::HP_ROMAN8         },
      { "NEXTSTEP",        encoding::NEXTSTEP          },
      // Full Unicode
      { "UTF-8",           encoding::UTF_8             },
      { "UCS-2",           encoding::UCS_2             },
      { "UCS-2BE",         encoding::UCS_2BE           },
      { "UCS-2LE",         encoding::UCS_2LE           },
      { "UCS-4",           encoding::UCS_4             },
      { "UCS-4BE",         encoding::UCS_4BE           },
      { "UCS-4LE",         encoding::UCS_4LE           },
      { "UTF-16",          encoding::UTF_16            },
      { "UTF-16BE",        encoding::UTF_16BE          },
      { "UTF-16LE",        encoding::UTF_16LE          },
      { "UTF-32",          encoding::UTF_32            },
      { "UTF-32BE",        encoding::UTF_32BE          },
      { "UTF-32LE",        encoding::UTF_32LE          },
      { "UTF-7",           encoding::UTF_7             },
      { "C99",             encoding::C99               },
      { "JAVA",            encoding::JAVA              }
    };

    string text;
    is >> text;

    x = TBL.at(text);

    return is;
  }

  std::ostream& operator<<(std::ostream &os, const encoding &x)
  {
    using namespace std;
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

    return os << TBL.at(x);
  }
}

namespace scribbu {

  template <>
  bool char_traits<char>::is_code_unit(encoding x)
  {
    using namespace std;

    static const unordered_set<encoding> OK{
      encoding::UCS_2,    
      encoding::UCS_2BE,  
      encoding::UCS_2LE,  
      encoding::UCS_4,    
      encoding::UCS_4BE,  
      encoding::UCS_4LE,  
      encoding::UTF_16,   
      encoding::UTF_16BE, 
      encoding::UTF_16LE, 
      encoding::UTF_32,   
      encoding::UTF_32BE, 
      encoding::UTF_32LE, 
      encoding::UTF_7,    
      encoding::C99,      
      encoding::JAVA 
    };

    return OK.count(x) == 0;
  }

  template <>
  bool char_traits<wchar_t>::is_code_unit(encoding x)
  {
    using namespace std;

    static const unordered_set<encoding> OK{
      encoding::UCS_4,    
      encoding::UCS_4BE,  
      encoding::UCS_4LE,  
      encoding::UTF_32,   
      encoding::UTF_32BE, 
      encoding::UTF_32LE, 
    };

    return OK.count(x) != 0;
  }

  template <>
  bool char_traits<char16_t>::is_code_unit(encoding x)
  {
    using namespace std;

    static const unordered_set<encoding> OK{
      encoding::UCS_2,    
      encoding::UCS_2BE,  
      encoding::UCS_2LE,  
      encoding::UCS_4,    
      encoding::UCS_4BE,  
      encoding::UCS_4LE,  
      encoding::UTF_16,   
      encoding::UTF_16BE, 
      encoding::UTF_16LE, 
    };

    return OK.count(x) != 0;
  }

  template <>
  bool char_traits<char32_t>::is_code_unit(encoding x)
  {
    using namespace std;

    static const unordered_set<encoding> OK{
      encoding::UCS_4,    
      encoding::UCS_4BE,  
      encoding::UCS_4LE,  
      encoding::UTF_32,   
      encoding::UTF_32BE, 
      encoding::UTF_32LE, 
    };

    return OK.count(x) != 0;
  }

}

scribbu::bad_code_unit::bad_code_unit(encoding enc, std::size_t cb):
  enc_(enc), cb_(cb)
{ }


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

    // Try the global locale, first
    locale global;
    tie(ok, enc) = encoding_from_locale_name(global.name());
    if (ok) {
      return enc;
    }

    // Try the system locale
	locale loc("");
	tie(ok, enc) = encoding_from_locale_name(loc.name());
	if (ok) {
	  return enc;
	}

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
    if (!scribbu::char_traits<char>::is_code_unit(dstenc)) {
      throw bad_code_unit(dstenc, sizeof(char));
    }

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
