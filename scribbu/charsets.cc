/**
 * \file charsets.cc
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
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
      // European languages
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

///////////////////////////////////////////////////////////////////////////////
//                            class bad_code_unit                            //
///////////////////////////////////////////////////////////////////////////////

const char * scribbu::bad_code_unit::what() const noexcept
{
  if (! pwhat_) {
    std::stringstream stm;
    stm << cb_ << " is an invalid code unit for encoding " << enc_;
    pwhat_.reset(new std::string(stm.str()));
  }
  return pwhat_->c_str();
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
    
    if (0 == cbbuf) {
      return std::string();
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
    // each character conversion it increments *inbuf and decrements
    // *inbytesleft by the number of converted input bytes, it increments
    // *outbuf and decrements *outbytesleft by the number of converted output
    // bytes, and it updates the conversion state contained in cd."
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

  template <>
  std::string
  convert_encoding(const char *pbuf,
				   std::size_t cbbuf,
				   encoding srcenc,
				   encoding dstenc,
				   on_no_encoding rsp /*= on_no_encoding::fail*/) {
    return convert_encoding<std::string>((const unsigned char*)pbuf, cbbuf, 
                                         srcenc, dstenc, rsp);
  }
  
    /// Convert encodings from C strings to buffers of unsigned char
  template <>
  std::vector<unsigned char>
  convert_encoding(char const* const& text,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom /*= false*/,
                   on_no_encoding rsp /*= on_no_encoding::fail*/)
  {
    const unsigned char UTF16LE[] = { 0xff, 0xfe };
    const unsigned char UTF16BE[] = { 0xfe, 0xff };
    const unsigned char UTF8[]    = { 0xef, 0xbb, 0xbf };
    const unsigned char UTF32LE[] = { 0xff, 0xfe, 0x00, 0x00 };
    const unsigned char UTF32BE[] = { 0x00, 0x00, 0xff, 0xfe };

    using namespace std;

    if (!scribbu::char_traits<char>::is_code_unit(srcenc)) {
      throw bad_code_unit(dstenc, sizeof(char));
    }
    
    if (0 == strlen(text)) {
      return std::vector<unsigned char>();
    }

    size_t cbbom = 0;
    const unsigned char *pbom = nullptr;
    if (add_bom) {
      switch (dstenc) {
      case encoding::UCS_2BE:
      case encoding::UTF_16BE:
        cbbom = 2;
        pbom = UTF16BE;
        break;
      case encoding::UCS_2LE:
      case encoding::UTF_16LE:
        cbbom = 2;
        pbom = UTF16LE;
        break;
      case encoding::UTF_8:
        cbbom = 3;
        pbom = UTF8;
        break;
      case encoding::UCS_4BE:
      case encoding::UTF_32BE:
        cbbom = 4;
        pbom = UTF32BE;
        break;
      case encoding::UCS_4LE:
      case encoding::UTF_32LE:
        cbbom = 4;
        pbom = UTF32LE;
        break;
      default:
        // Fall-through intentionally: -Wswitch
        break;
      }
    }

    detail::iconv_specific::descriptor dsc(srcenc, dstenc, rsp);

    char *inbuf = const_cast<char*>(text);
    size_t ntext = strlen(text);
    size_t inbytesleft = ntext;
    // We can't know a priori how many octets the output buffer will require;
    // cf.
    // http://stackoverflow.com/questions/13297458/simple-utf8-utf16-string-conversion-with-iconv
    std::size_t cbout = cbbom + (ntext << 2);
    vector<unsigned char> out(cbout);
    std::size_t outbytesleft = cbout;
    char *outbuf = reinterpret_cast<char*>(&(out[cbbom]));
    size_t status = iconv(dsc, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    while (~0 == status && E2BIG == errno) {
      // If the "output buffer has no more room for the next converted
      // character. In this case it sets errno to E2BIG and returns
      // (size_t)(−1)." Try again with a bigger buffer :P
      cbout <<= 2;
      out.resize(cbout);

      inbuf = const_cast<char*>(text);
      inbytesleft = ntext;
      outbytesleft = cbout;
      char *outbuf = reinterpret_cast<char*>(&(out[cbbom]));
      std::size_t outbytesleft = cbout;
      status = iconv(dsc, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    }

    if (~0 == status) {
      throw iconv_error(errno);
    }

    // The GNU iconv docs don't say anything about Unicode BOMs. Empirically,
    // if there's a BOM present in the input text, `iconv' will produce a BOM
    // in the output text. That won't generally be the case, here. Add the BOM,
    // if requested & appropriate.
    if (add_bom && cbbom) {
      copy(pbom, pbom + cbbom, out.begin());
    }

    // Finally, trim the buffer to size before returning.
    cbout -= outbytesleft;
    out.resize(cbout);

    return out;
  }

  /// Convert encodings from C strings to buffers of unsigned char
  template <>
  std::vector<unsigned char>
  convert_encoding(char* const& text,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom /*= false*/,
                   on_no_encoding rsp /*= on_no_encoding::fail*/)
  {
    return convert_encoding<char const*>(text, srcenc, dstenc, add_bom, rsp);
  }
  
  std::vector<unsigned char>
  convert_encoding(const char *ptext,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom /*= false*/,
                   on_no_encoding rsp /*= on_no_encoding::fail*/)
  {
    return convert_encoding<char const*>(ptext, srcenc, dstenc, add_bom, rsp);
  }
  
  template <>
  std::vector<unsigned char>
  convert_encoding(const std::string &text,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom /*= false*/,
                   on_no_encoding rsp /*= on_no_encoding::fail*/)
  {
    return convert_encoding<const char*>(text.c_str(), srcenc, dstenc, add_bom, 
                                         rsp);
  }

} // End namespace scribbu.

scribbu::language
scribbu::language_from_iso_639_1(char code[2])
{
  static bool init = false;
  static std::unordered_map<uint16_t, language> lookup;
  if (!init) {
#   define H(a, b) lookup.insert(std::make_pair((#a[0]<<8)|#a[1], language:: b))
    H(aa, aar);
    H(ab, abk);
    H(af, afr);
    H(ak, aka);
    H(sq, alb);
    H(am, amh);
    H(ar, ara);
    H(an, arg);
    H(hy, arm);
    H(as, ASM);
    H(av, ava);
    H(ae, ave);
    H(ay, aym);
    H(az, aze);
    H(ba, bak);
    H(bm, bam);
    H(eu, baq);
    H(be, bel);
    H(bn, ben);
    H(bh, bih);
    H(bi, bis);
    H(bo, bod);
    H(bs, bos);
    H(br, bre);
    H(bg, bul);
    H(my, bur);
    H(ca, cat);
    H(cs, ces);
    H(ch, cha);
    H(ce, che);
    H(zh, chi);
    H(cu, chu);
    H(cv, chv);
    H(kw, cor);
    H(co, cos);
    H(cr, cre);
    H(cy, cym);
    H(cs, cze);
    H(da, dan);
    H(de, deu);
    H(dv, DIV);
    H(nl, dut);
    H(dz, dzo);
    H(el, ell);
    H(en, eng);
    H(eo, epo);
    H(et, est);
    H(eu, eus);
    H(ee, ewe);
    H(fo, fao);
    H(fa, fas);
    H(fj, fij);
    H(fi, fin);
    H(fr, fra);
    H(fr, fre);
    H(fy, fry);
    H(ff, ful);
    H(ka, geo);
    H(de, ger);
    H(gd, gla);
    H(ga, gle);
    H(gl, glg);
    H(gv, glv);
    H(el, gre);
    H(gn, grn);
    H(gu, guj);
    H(ht, hat);
    H(ha, hau);
    H(he, heb);
    H(hz, her);
    H(hi, hin);
    H(ho, hmo);
    H(hr, hrv);
    H(hu, hun);
    H(hy, hye);
    H(ig, ibo);
    H(is, ice);
    H(io, ido);
    H(ii, iii);
    H(iu, iku);
    H(ie, ile);
    H(ia, ina);
    H(id, ind);
    H(ik, ipk);
    H(is, isl);
    H(it, ita);
    H(jv, jav);
    H(ja, jpn);
    H(kl, kal);
    H(kn, kan);
    H(ks, kas);
    H(ka, kat);
    H(kr, kau);
    H(kk, kaz);
    H(km, khm);
    H(ki, kik);
    H(rw, kin);
    H(ky, kir);
    H(kv, kom);
    H(kg, kon);
    H(ko, kor);
    H(kj, kua);
    H(ku, kur);
    H(lo, lao);
    H(la, lat);
    H(lv, lav);
    H(li, lim);
    H(ln, lin);
    H(lt, lit);
    H(lb, ltz);
    H(lu, lub);
    H(lg, lug);
    H(mk, mac);
    H(mh, mah);
    H(ml, mal);
    H(mi, mao);
    H(mr, mar);
    H(ms, may);
    H(mk, mkd);
    H(mg, mlg);
    H(mt, mlt);
    H(mn, mon);
    H(mi, mri);
    H(ms, msa);
    H(my, mya);
    H(na, nau);
    H(nv, nav);
    H(nr, nbl);
    H(nd, nde);
    H(ng, ndo);
    H(ne, nep);
    H(nl, nld);
    H(nn, nno);
    H(nb, nob);
    H(no, nor);
    H(ny, nya);
    H(oc, oci);
    H(oj, oji);
    H(or, ori);
    H(om, orm);
    H(os, oss);
    H(pa, pan);
    H(fa, per);
    H(pi, pli);
    H(pl, pol);
    H(pt, por);
    H(ps, pus);
    H(qu, que);
    H(rm, roh);
    H(ro, ron);
    H(ro, rum);
    H(rn, run);
    H(ru, rus);
    H(sg, sag);
    H(sa, san);
    H(si, sin);
    H(sk, slo);
    H(sk, slk);
    H(sl, slv);
    H(se, sme);
    H(sm, smo);
    H(sn, sna);
    H(sd, snd);
    H(so, som);
    H(st, sot);
    H(es, spa);
    H(sq, sqi);
    H(sc, srd);
    H(sr, srp);
    H(ss, ssw);
    H(su, sun);
    H(sw, swa);
    H(sv, swe);
    H(ty, tah);
    H(ta, tam);
    H(tt, tat);
    H(te, tel);
    H(tg, tgk);
    H(tl, tgl);
    H(th, tha);
    H(bo, tib);
    H(ti, tir);
    H(to, ton);
    H(tn, tsn);
    H(ts, tso);
    H(tk, tuk);
    H(tr, tur);
    H(tw, twi);
    H(ug, uig);
    H(uk, ukr);
    H(ur, urd);
    H(uz, uzb);
    H(ve, ven);
    H(vi, vie);
    H(vo, vol);
    H(cy, wel);
    H(wa, wln);
    H(wo, wol);
    H(xh, xho);
    H(yi, yid);
    H(yo, yor);
    H(za, zha);
    H(zh, zho);
    H(zu, zul);
#   undef H
    init = true;
  }

  return lookup[(code[0]<<8)|code[1]];
}

scribbu::language
scribbu::language_from_locale()
{
  // Check LANGUAGE, LC_ALL & LANG, in that order. The first one defined and
  // non-empty will be used to derive a two-letter (ISO-869-1) code, which we
  // then attempt to map to a three-letter code (ISO-869-2).
  static const char * const ENVVARS[] = {"LANGUAGE", "LC_ALL", "LANG" };

  char iso_869_1[2] = {0, 0};
  for (auto p: ENVVARS) {
    char *pv = getenv(p);
    if (pv && 1 < strlen(pv)) {
      iso_869_1[0] = *pv++;
      iso_869_1[1] = *pv;
      break;
    }
  }

  if (0 == iso_869_1[0] && 0 == iso_869_1[1]) {
    throw std::runtime_error("Unable to determine language from locale");
  }

  return language_from_iso_639_1(iso_869_1);

}

void
scribbu::language_to_iso_639_2(language lang, unsigned char code[3])
{
  if (language::from_locale == lang) {
    lang = language_from_locale();
  }

  switch (lang) {
#   define G(x) code[0]=x[0];code[1]=x[1];code[2]=x[2]
#   define F(x) \
    case language:: x:                         \
      G(#x);    \
      break

    F(aar);
    F(abk);
    F(ace);
    F(ach);
    F(ada);
    F(ady);
    F(afa);
    F(afh);
    F(afr);
    F(ain);
    F(aka);
    F(akk);
    F(alb);
    F(ale);
    F(alg);
    F(alt);
    F(amh);
    F(ang);
    F(anp);
    F(apa);
    F(ara);
    F(arc);
    F(arg);
    F(arm);
    F(arn);
    F(arp);
    F(art);
    F(arw);
    F(ASM);
    F(ast);
    F(ath);
    F(aus);
    F(ava);
    F(ave);
    F(awa);
    F(aym);
    F(aze);
    F(bad);
    F(bai);
    F(bak);
    F(bal);
    F(bam);
    F(ban);
    F(baq);
    F(bas);
    F(bat);
    F(bej);
    F(bel);
    F(bem);
    F(ben);
    F(ber);
    F(bho);
    F(bih);
    F(bik);
    F(bin);
    F(bis);
    F(bla);
    F(bnt);
    F(bod);
    F(bos);
    F(bra);
    F(bre);
    F(btk);
    F(bua);
    F(bug);
    F(bul);
    F(bur);
    F(byn);
    F(cad);
    F(cai);
    F(car);
    F(cat);
    F(cau);
    F(ceb);
    F(cel);
    F(ces);
    F(cha);
    F(chb);
    F(che);
    F(chg);
    F(chi);
    F(chk);
    F(chm);
    F(chn);
    F(cho);
    F(chp);
    F(chr);
    F(chu);
    F(chv);
    F(chy);
    F(cmc);
    F(cnr);
    F(cop);
    F(cor);
    F(cos);
    F(cpe);
    F(cpf);
    F(cpp);
    F(cre);
    F(crh);
    F(crp);
    F(csb);
    F(cus);
    F(cym);
    F(cze);
    F(dak);
    F(dan);
    F(dar);
    F(day);
    F(del);
    F(den);
    F(deu);
    F(dgr);
    F(din);
    F(DIV);
    F(doi);
    F(dra);
    F(dsb);
    F(dua);
    F(dum);
    F(dut);
    F(dyu);
    F(dzo);
    F(efi);
    F(egy);
    F(eka);
    F(ell);
    F(elx);
    F(eng);
    F(enm);
    F(epo);
    F(est);
    F(eus);
    F(ewe);
    F(ewo);
    F(fan);
    F(fao);
    F(fas);
    F(fat);
    F(fij);
    F(fil);
    F(fin);
    F(fiu);
    F(fon);
    F(fra);
    F(fre);
    F(frm);
    F(fro);
    F(frr);
    F(frs);
    F(fry);
    F(ful);
    F(fur);
    F(gaa);
    F(gay);
    F(gba);
    F(gem);
    F(geo);
    F(ger);
    F(gez);
    F(gil);
    F(gla);
    F(gle);
    F(glg);
    F(glv);
    F(gmh);
    F(goh);
    F(gon);
    F(gor);
    F(got);
    F(grb);
    F(grc);
    F(gre);
    F(grn);
    F(gsw);
    F(guj);
    F(gwi);
    F(hai);
    F(hat);
    F(hau);
    F(haw);
    F(heb);
    F(her);
    F(hil);
    F(him);
    F(hin);
    F(hit);
    F(hmn);
    F(hmo);
    F(hrv);
    F(hsb);
    F(hun);
    F(hup);
    F(hye);
    F(iba);
    F(ibo);
    F(ice);
    F(ido);
    F(iii);
    F(ijo);
    F(iku);
    F(ile);
    F(ilo);
    F(ina);
    F(inc);
    F(ind);
    F(ine);
    F(inh);
    F(ipk);
    F(ira);
    F(iro);
    F(isl);
    F(ita);
    F(jav);
    F(jbo);
    F(jpn);
    F(jpr);
    F(jrb);
    F(kaa);
    F(kab);
    F(kac);
    F(kal);
    F(kam);
    F(kan);
    F(kar);
    F(kas);
    F(kat);
    F(kau);
    F(kaw);
    F(kaz);
    F(kbd);
    F(kha);
    F(khi);
    F(khm);
    F(kho);
    F(kik);
    F(kin);
    F(kir);
    F(kmb);
    F(kok);
    F(kom);
    F(kon);
    F(kor);
    F(kos);
    F(kpe);
    F(krc);
    F(krl);
    F(kro);
    F(kru);
    F(kua);
    F(kum);
    F(kur);
    F(kut);
    F(lad);
    F(lah);
    F(lam);
    F(lao);
    F(lat);
    F(lav);
    F(lez);
    F(lim);
    F(lin);
    F(lit);
    F(lol);
    F(loz);
    F(ltz);
    F(lua);
    F(lub);
    F(lug);
    F(lui);
    F(lun);
    F(luo);
    F(lus);
    F(mac);
    F(mad);
    F(mag);
    F(mah);
    F(mai);
    F(mak);
    F(mal);
    F(man);
    F(mao);
    F(MAP);
    F(mar);
    F(mas);
    F(may);
    F(mdf);
    F(mdr);
    F(men);
    F(mga);
    F(mic);
    F(MIN);
    F(mis);
    F(mkd);
    F(mkh);
    F(mlg);
    F(mlt);
    F(mnc);
    F(mni);
    F(mno);
    F(moh);
    F(mon);
    F(mos);
    F(mri);
    F(msa);
    F(mul);
    F(mun);
    F(mus);
    F(mwl);
    F(mwr);
    F(mya);
    F(myn);
    F(myv);
    F(nah);
    F(nai);
    F(nap);
    F(nau);
    F(nav);
    F(nbl);
    F(nde);
    F(ndo);
    F(nds);
    F(nep);
    F(NEW);
    F(nia);
    F(nic);
    F(niu);
    F(nld);
    F(nno);
    F(nob);
    F(nog);
    F(non);
    F(nor);
    F(nqo);
    F(nso);
    F(nub);
    F(nwc);
    F(nya);
    F(nym);
    F(nyn);
    F(nyo);
    F(nzi);
    F(oci);
    F(oji);
    F(ori);
    F(orm);
    F(osa);
    F(oss);
    F(ota);
    F(oto);
    F(paa);
    F(pag);
    F(pal);
    F(pam);
    F(pan);
    F(pap);
    F(pau);
    F(peo);
    F(per);
    F(phi);
    F(phn);
    F(pli);
    F(pol);
    F(pon);
    F(por);
    F(pra);
    F(pro);
    F(pus);
    F(qaa);
    F(que);
    F(raj);
    F(rap);
    F(rar);
    F(roa);
    F(roh);
    F(rom);
    F(ron);
    F(rum);
    F(run);
    F(rup);
    F(rus);
    F(sad);
    F(sag);
    F(sah);
    F(sai);
    F(sal);
    F(sam);
    F(san);
    F(sas);
    F(sat);
    F(scn);
    F(sco);
    F(sel);
    F(sem);
    F(sga);
    F(sgn);
    F(shn);
    F(sid);
    F(sin);
    F(sio);
    F(sit);
    F(sla);
    F(slo);
    F(slk);
    F(slv);
    F(sma);
    F(sme);
    F(smi);
    F(smj);
    F(smn);
    F(smo);
    F(sms);
    F(sna);
    F(snd);
    F(snk);
    F(sog);
    F(som);
    F(son);
    F(sot);
    F(spa);
    F(sqi);
    F(srd);
    F(srn);
    F(srp);
    F(srr);
    F(ssa);
    F(ssw);
    F(suk);
    F(sun);
    F(sus);
    F(sux);
    F(swa);
    F(swe);
    F(syc);
    F(syr);
    F(tah);
    F(tai);
    F(tam);
    F(tat);
    F(tel);
    F(tem);
    F(ter);
    F(tet);
    F(tgk);
    F(tgl);
    F(tha);
    F(tib);
    F(tig);
    F(tir);
    F(tiv);
    F(tkl);
    F(tlh);
    F(tli);
    F(tmh);
    F(tog);
    F(ton);
    F(tpi);
    F(tsi);
    F(tsn);
    F(tso);
    F(tuk);
    F(tum);
    F(tup);
    F(tur);
    F(tut);
    F(tvl);
    F(twi);
    F(tyv);
    F(udm);
    F(uga);
    F(uig);
    F(ukr);
    F(umb);
    F(und);
    F(urd);
    F(uzb);
    F(vai);
    F(ven);
    F(vie);
    F(vol);
    F(vot);
    F(wak);
    F(wal);
    F(war);
    F(was);
    F(wel);
    F(wen);
    F(wln);
    F(wol);
    F(xal);
    F(xho);
    F(yao);
    F(yap);
    F(yid);
    F(yor);
    F(ypk);
    F(zap);
    F(zbl);
    F(zen);
    F(zgh);
    F(zha);
    F(zho);
    F(znd);
    F(zul);
    F(zun);
    F(zxx);
    F(zza);

# undef F
# undef G
  case language::from_locale:
    // Should never be here
    throw std::logic_error("language_to_iso_639_2 invoked with from_locale");
  }

} // End scribbu::language_to_iso_639_2.

std::istream&
scribbu::operator>>(std::istream &is, language &x)
{
  using namespace std;
  static const unordered_map<string, language, hash<string>> TBL{
    { "aar", language::aar },
    { "abk", language::abk },
    { "ace", language::ace },
    { "ach", language::ach },
    { "ada", language::ada },
    { "ady", language::ady },
    { "afa", language::afa },
    { "afh", language::afh },
    { "afr", language::afr },
    { "ain", language::ain },
    { "aka", language::aka },
    { "akk", language::akk },
    { "alb", language::alb },
    { "ale", language::ale },
    { "alg", language::alg },
    { "alt", language::alt },
    { "amh", language::amh },
    { "ang", language::ang },
    { "anp", language::anp },
    { "apa", language::apa },
    { "ara", language::ara },
    { "arc", language::arc },
    { "arg", language::arg },
    { "arm", language::arm },
    { "arn", language::arn },
    { "arp", language::arp },
    { "art", language::art },
    { "arw", language::arw },
    { "ASM", language::ASM },
    { "ast", language::ast },
    { "ath", language::ath },
    { "aus", language::aus },
    { "ava", language::ava },
    { "ave", language::ave },
    { "awa", language::awa },
    { "aym", language::aym },
    { "aze", language::aze },
    { "bad", language::bad },
    { "bai", language::bai },
    { "bak", language::bak },
    { "bal", language::bal },
    { "bam", language::bam },
    { "ban", language::ban },
    { "baq", language::baq },
    { "bas", language::bas },
    { "bat", language::bat },
    { "bej", language::bej },
    { "bel", language::bel },
    { "bem", language::bem },
    { "ben", language::ben },
    { "ber", language::ber },
    { "bho", language::bho },
    { "bih", language::bih },
    { "bik", language::bik },
    { "bin", language::bin },
    { "bis", language::bis },
    { "bla", language::bla },
    { "bnt", language::bnt },
    { "bod", language::bod },
    { "bos", language::bos },
    { "bra", language::bra },
    { "bre", language::bre },
    { "btk", language::btk },
    { "bua", language::bua },
    { "bug", language::bug },
    { "bul", language::bul },
    { "bur", language::bur },
    { "byn", language::byn },
    { "cad", language::cad },
    { "cai", language::cai },
    { "car", language::car },
    { "cat", language::cat },
    { "cau", language::cau },
    { "ceb", language::ceb },
    { "cel", language::cel },
    { "ces", language::ces },
    { "cha", language::cha },
    { "chb", language::chb },
    { "che", language::che },
    { "chg", language::chg },
    { "chi", language::chi },
    { "chk", language::chk },
    { "chm", language::chm },
    { "chn", language::chn },
    { "cho", language::cho },
    { "chp", language::chp },
    { "chr", language::chr },
    { "chu", language::chu },
    { "chv", language::chv },
    { "chy", language::chy },
    { "cmc", language::cmc },
    { "cnr", language::cnr },
    { "cop", language::cop },
    { "cor", language::cor },
    { "cos", language::cos },
    { "cpe", language::cpe },
    { "cpf", language::cpf },
    { "cpp", language::cpp },
    { "cre", language::cre },
    { "crh", language::crh },
    { "crp", language::crp },
    { "csb", language::csb },
    { "cus", language::cus },
    { "cym", language::cym },
    { "cze", language::cze },
    { "dak", language::dak },
    { "dan", language::dan },
    { "dar", language::dar },
    { "day", language::day },
    { "del", language::del },
    { "den", language::den },
    { "deu", language::deu },
    { "dgr", language::dgr },
    { "din", language::din },
    { "DIV", language::DIV },
    { "doi", language::doi },
    { "dra", language::dra },
    { "dsb", language::dsb },
    { "dua", language::dua },
    { "dum", language::dum },
    { "dut", language::dut },
    { "dyu", language::dyu },
    { "dzo", language::dzo },
    { "efi", language::efi },
    { "egy", language::egy },
    { "eka", language::eka },
    { "ell", language::ell },
    { "elx", language::elx },
    { "eng", language::eng },
    { "enm", language::enm },
    { "epo", language::epo },
    { "est", language::est },
    { "eus", language::eus },
    { "ewe", language::ewe },
    { "ewo", language::ewo },
    { "fan", language::fan },
    { "fao", language::fao },
    { "fas", language::fas },
    { "fat", language::fat },
    { "fij", language::fij },
    { "fil", language::fil },
    { "fin", language::fin },
    { "fiu", language::fiu },
    { "fon", language::fon },
    { "fra", language::fra },
    { "fre", language::fre },
    { "frm", language::frm },
    { "fro", language::fro },
    { "frr", language::frr },
    { "frs", language::frs },
    { "fry", language::fry },
    { "ful", language::ful },
    { "fur", language::fur },
    { "gaa", language::gaa },
    { "gay", language::gay },
    { "gba", language::gba },
    { "gem", language::gem },
    { "geo", language::geo },
    { "ger", language::ger },
    { "gez", language::gez },
    { "gil", language::gil },
    { "gla", language::gla },
    { "gle", language::gle },
    { "glg", language::glg },
    { "glv", language::glv },
    { "gmh", language::gmh },
    { "goh", language::goh },
    { "gon", language::gon },
    { "gor", language::gor },
    { "got", language::got },
    { "grb", language::grb },
    { "grc", language::grc },
    { "gre", language::gre },
    { "grn", language::grn },
    { "gsw", language::gsw },
    { "guj", language::guj },
    { "gwi", language::gwi },
    { "hai", language::hai },
    { "hat", language::hat },
    { "hau", language::hau },
    { "haw", language::haw },
    { "heb", language::heb },
    { "her", language::her },
    { "hil", language::hil },
    { "him", language::him },
    { "hin", language::hin },
    { "hit", language::hit },
    { "hmn", language::hmn },
    { "hmo", language::hmo },
    { "hrv", language::hrv },
    { "hsb", language::hsb },
    { "hun", language::hun },
    { "hup", language::hup },
    { "hye", language::hye },
    { "iba", language::iba },
    { "ibo", language::ibo },
    { "ice", language::ice },
    { "ido", language::ido },
    { "iii", language::iii },
    { "ijo", language::ijo },
    { "iku", language::iku },
    { "ile", language::ile },
    { "ilo", language::ilo },
    { "ina", language::ina },
    { "inc", language::inc },
    { "ind", language::ind },
    { "ine", language::ine },
    { "inh", language::inh },
    { "ipk", language::ipk },
    { "ira", language::ira },
    { "iro", language::iro },
    { "isl", language::isl },
    { "ita", language::ita },
    { "jav", language::jav },
    { "jbo", language::jbo },
    { "jpn", language::jpn },
    { "jpr", language::jpr },
    { "jrb", language::jrb },
    { "kaa", language::kaa },
    { "kab", language::kab },
    { "kac", language::kac },
    { "kal", language::kal },
    { "kam", language::kam },
    { "kan", language::kan },
    { "kar", language::kar },
    { "kas", language::kas },
    { "kat", language::kat },
    { "kau", language::kau },
    { "kaw", language::kaw },
    { "kaz", language::kaz },
    { "kbd", language::kbd },
    { "kha", language::kha },
    { "khi", language::khi },
    { "khm", language::khm },
    { "kho", language::kho },
    { "kik", language::kik },
    { "kin", language::kin },
    { "kir", language::kir },
    { "kmb", language::kmb },
    { "kok", language::kok },
    { "kom", language::kom },
    { "kon", language::kon },
    { "kor", language::kor },
    { "kos", language::kos },
    { "kpe", language::kpe },
    { "krc", language::krc },
    { "krl", language::krl },
    { "kro", language::kro },
    { "kru", language::kru },
    { "kua", language::kua },
    { "kum", language::kum },
    { "kur", language::kur },
    { "kut", language::kut },
    { "lad", language::lad },
    { "lah", language::lah },
    { "lam", language::lam },
    { "lao", language::lao },
    { "lat", language::lat },
    { "lav", language::lav },
    { "lez", language::lez },
    { "lim", language::lim },
    { "lin", language::lin },
    { "lit", language::lit },
    { "lol", language::lol },
    { "loz", language::loz },
    { "ltz", language::ltz },
    { "lua", language::lua },
    { "lub", language::lub },
    { "lug", language::lug },
    { "lui", language::lui },
    { "lun", language::lun },
    { "luo", language::luo },
    { "lus", language::lus },
    { "mac", language::mac },
    { "mad", language::mad },
    { "mag", language::mag },
    { "mah", language::mah },
    { "mai", language::mai },
    { "mak", language::mak },
    { "mal", language::mal },
    { "man", language::man },
    { "mao", language::mao },
    { "MAP", language::MAP },
    { "mar", language::mar },
    { "mas", language::mas },
    { "may", language::may },
    { "mdf", language::mdf },
    { "mdr", language::mdr },
    { "men", language::men },
    { "mga", language::mga },
    { "mic", language::mic },
    { "MIN", language::MIN },
    { "mis", language::mis },
    { "mkd", language::mkd },
    { "mkh", language::mkh },
    { "mlg", language::mlg },
    { "mlt", language::mlt },
    { "mnc", language::mnc },
    { "mni", language::mni },
    { "mno", language::mno },
    { "moh", language::moh },
    { "mon", language::mon },
    { "mos", language::mos },
    { "mri", language::mri },
    { "msa", language::msa },
    { "mul", language::mul },
    { "mun", language::mun },
    { "mus", language::mus },
    { "mwl", language::mwl },
    { "mwr", language::mwr },
    { "mya", language::mya },
    { "myn", language::myn },
    { "myv", language::myv },
    { "nah", language::nah },
    { "nai", language::nai },
    { "nap", language::nap },
    { "nau", language::nau },
    { "nav", language::nav },
    { "nbl", language::nbl },
    { "nde", language::nde },
    { "ndo", language::ndo },
    { "nds", language::nds },
    { "nep", language::nep },
    { "NEW", language::NEW },
    { "nia", language::nia },
    { "nic", language::nic },
    { "niu", language::niu },
    { "nld", language::nld },
    { "nno", language::nno },
    { "nob", language::nob },
    { "nog", language::nog },
    { "non", language::non },
    { "nor", language::nor },
    { "nqo", language::nqo },
    { "nso", language::nso },
    { "nub", language::nub },
    { "nwc", language::nwc },
    { "nya", language::nya },
    { "nym", language::nym },
    { "nyn", language::nyn },
    { "nyo", language::nyo },
    { "nzi", language::nzi },
    { "oci", language::oci },
    { "oji", language::oji },
    { "ori", language::ori },
    { "orm", language::orm },
    { "osa", language::osa },
    { "oss", language::oss },
    { "ota", language::ota },
    { "oto", language::oto },
    { "paa", language::paa },
    { "pag", language::pag },
    { "pal", language::pal },
    { "pam", language::pam },
    { "pan", language::pan },
    { "pap", language::pap },
    { "pau", language::pau },
    { "peo", language::peo },
    { "per", language::per },
    { "phi", language::phi },
    { "phn", language::phn },
    { "pli", language::pli },
    { "pol", language::pol },
    { "pon", language::pon },
    { "por", language::por },
    { "pra", language::pra },
    { "pro", language::pro },
    { "pus", language::pus },
    { "qaa", language::qaa },
    { "que", language::que },
    { "raj", language::raj },
    { "rap", language::rap },
    { "rar", language::rar },
    { "roa", language::roa },
    { "roh", language::roh },
    { "rom", language::rom },
    { "ron", language::ron },
    { "rum", language::rum },
    { "run", language::run },
    { "rup", language::rup },
    { "rus", language::rus },
    { "sad", language::sad },
    { "sag", language::sag },
    { "sah", language::sah },
    { "sai", language::sai },
    { "sal", language::sal },
    { "sam", language::sam },
    { "san", language::san },
    { "sas", language::sas },
    { "sat", language::sat },
    { "scn", language::scn },
    { "sco", language::sco },
    { "sel", language::sel },
    { "sem", language::sem },
    { "sga", language::sga },
    { "sgn", language::sgn },
    { "shn", language::shn },
    { "sid", language::sid },
    { "sin", language::sin },
    { "sio", language::sio },
    { "sit", language::sit },
    { "sla", language::sla },
    { "slo", language::slo },
    { "slk", language::slk },
    { "slv", language::slv },
    { "sma", language::sma },
    { "sme", language::sme },
    { "smi", language::smi },
    { "smj", language::smj },
    { "smn", language::smn },
    { "smo", language::smo },
    { "sms", language::sms },
    { "sna", language::sna },
    { "snd", language::snd },
    { "snk", language::snk },
    { "sog", language::sog },
    { "som", language::som },
    { "son", language::son },
    { "sot", language::sot },
    { "spa", language::spa },
    { "sqi", language::sqi },
    { "srd", language::srd },
    { "srn", language::srn },
    { "srp", language::srp },
    { "srr", language::srr },
    { "ssa", language::ssa },
    { "ssw", language::ssw },
    { "suk", language::suk },
    { "sun", language::sun },
    { "sus", language::sus },
    { "sux", language::sux },
    { "swa", language::swa },
    { "swe", language::swe },
    { "syc", language::syc },
    { "syr", language::syr },
    { "tah", language::tah },
    { "tai", language::tai },
    { "tam", language::tam },
    { "tat", language::tat },
    { "tel", language::tel },
    { "tem", language::tem },
    { "ter", language::ter },
    { "tet", language::tet },
    { "tgk", language::tgk },
    { "tgl", language::tgl },
    { "tha", language::tha },
    { "tib", language::tib },
    { "tig", language::tig },
    { "tir", language::tir },
    { "tiv", language::tiv },
    { "tkl", language::tkl },
    { "tlh", language::tlh },
    { "tli", language::tli },
    { "tmh", language::tmh },
    { "tog", language::tog },
    { "ton", language::ton },
    { "tpi", language::tpi },
    { "tsi", language::tsi },
    { "tsn", language::tsn },
    { "tso", language::tso },
    { "tuk", language::tuk },
    { "tum", language::tum },
    { "tup", language::tup },
    { "tur", language::tur },
    { "tut", language::tut },
    { "tvl", language::tvl },
    { "twi", language::twi },
    { "tyv", language::tyv },
    { "udm", language::udm },
    { "uga", language::uga },
    { "uig", language::uig },
    { "ukr", language::ukr },
    { "umb", language::umb },
    { "und", language::und },
    { "urd", language::urd },
    { "uzb", language::uzb },
    { "vai", language::vai },
    { "ven", language::ven },
    { "vie", language::vie },
    { "vol", language::vol },
    { "vot", language::vot },
    { "wak", language::wak },
    { "wal", language::wal },
    { "war", language::war },
    { "was", language::was },
    { "wel", language::wel },
    { "wen", language::wen },
    { "wln", language::wln },
    { "wol", language::wol },
    { "xal", language::xal },
    { "xho", language::xho },
    { "yao", language::yao },
    { "yap", language::yap },
    { "yid", language::yid },
    { "yor", language::yor },
    { "ypk", language::ypk },
    { "zap", language::zap },
    { "zbl", language::zbl },
    { "zen", language::zen },
    { "zgh", language::zgh },
    { "zha", language::zha },
    { "zho", language::zho },
    { "znd", language::znd },
    { "zul", language::zul },
    { "zun", language::zun },
    { "zxx", language::zxx },
    { "zza", language::zza },
  };

  string text;
  is >> text;

  x = TBL.at(text);

  return is;
}

std::ostream&
scribbu::operator<<(std::ostream &os, const language &x)
{
  using namespace std;
  static const unordered_map<language, string, hash<language>> TBL{
    { language::aar, "aar" },
    { language::abk, "abk" },
    { language::ace, "ace" },
    { language::ach, "ach" },
    { language::ada, "ada" },
    { language::ady, "ady" },
    { language::afa, "afa" },
    { language::afh, "afh" },
    { language::afr, "afr" },
    { language::ain, "ain" },
    { language::aka, "aka" },
    { language::akk, "akk" },
    { language::alb, "alb" },
    { language::ale, "ale" },
    { language::alg, "alg" },
    { language::alt, "alt" },
    { language::amh, "amh" },
    { language::ang, "ang" },
    { language::anp, "anp" },
    { language::apa, "apa" },
    { language::ara, "ara" },
    { language::arc, "arc" },
    { language::arg, "arg" },
    { language::arm, "arm" },
    { language::arn, "arn" },
    { language::arp, "arp" },
    { language::art, "art" },
    { language::arw, "arw" },
    { language::ASM, "ASM" },
    { language::ast, "ast" },
    { language::ath, "ath" },
    { language::aus, "aus" },
    { language::ava, "ava" },
    { language::ave, "ave" },
    { language::awa, "awa" },
    { language::aym, "aym" },
    { language::aze, "aze" },
    { language::bad, "bad" },
    { language::bai, "bai" },
    { language::bak, "bak" },
    { language::bal, "bal" },
    { language::bam, "bam" },
    { language::ban, "ban" },
    { language::baq, "baq" },
    { language::bas, "bas" },
    { language::bat, "bat" },
    { language::bej, "bej" },
    { language::bel, "bel" },
    { language::bem, "bem" },
    { language::ben, "ben" },
    { language::ber, "ber" },
    { language::bho, "bho" },
    { language::bih, "bih" },
    { language::bik, "bik" },
    { language::bin, "bin" },
    { language::bis, "bis" },
    { language::bla, "bla" },
    { language::bnt, "bnt" },
    { language::bod, "bod" },
    { language::bos, "bos" },
    { language::bra, "bra" },
    { language::bre, "bre" },
    { language::btk, "btk" },
    { language::bua, "bua" },
    { language::bug, "bug" },
    { language::bul, "bul" },
    { language::bur, "bur" },
    { language::byn, "byn" },
    { language::cad, "cad" },
    { language::cai, "cai" },
    { language::car, "car" },
    { language::cat, "cat" },
    { language::cau, "cau" },
    { language::ceb, "ceb" },
    { language::cel, "cel" },
    { language::ces, "ces" },
    { language::cha, "cha" },
    { language::chb, "chb" },
    { language::che, "che" },
    { language::chg, "chg" },
    { language::chi, "chi" },
    { language::chk, "chk" },
    { language::chm, "chm" },
    { language::chn, "chn" },
    { language::cho, "cho" },
    { language::chp, "chp" },
    { language::chr, "chr" },
    { language::chu, "chu" },
    { language::chv, "chv" },
    { language::chy, "chy" },
    { language::cmc, "cmc" },
    { language::cnr, "cnr" },
    { language::cop, "cop" },
    { language::cor, "cor" },
    { language::cos, "cos" },
    { language::cpe, "cpe" },
    { language::cpf, "cpf" },
    { language::cpp, "cpp" },
    { language::cre, "cre" },
    { language::crh, "crh" },
    { language::crp, "crp" },
    { language::csb, "csb" },
    { language::cus, "cus" },
    { language::cym, "cym" },
    { language::cze, "cze" },
    { language::dak, "dak" },
    { language::dan, "dan" },
    { language::dar, "dar" },
    { language::day, "day" },
    { language::del, "del" },
    { language::den, "den" },
    { language::deu, "deu" },
    { language::dgr, "dgr" },
    { language::din, "din" },
    { language::DIV, "DIV" },
    { language::doi, "doi" },
    { language::dra, "dra" },
    { language::dsb, "dsb" },
    { language::dua, "dua" },
    { language::dum, "dum" },
    { language::dut, "dut" },
    { language::dyu, "dyu" },
    { language::dzo, "dzo" },
    { language::efi, "efi" },
    { language::egy, "egy" },
    { language::eka, "eka" },
    { language::ell, "ell" },
    { language::elx, "elx" },
    { language::eng, "eng" },
    { language::enm, "enm" },
    { language::epo, "epo" },
    { language::est, "est" },
    { language::eus, "eus" },
    { language::ewe, "ewe" },
    { language::ewo, "ewo" },
    { language::fan, "fan" },
    { language::fao, "fao" },
    { language::fas, "fas" },
    { language::fat, "fat" },
    { language::fij, "fij" },
    { language::fil, "fil" },
    { language::fin, "fin" },
    { language::fiu, "fiu" },
    { language::fon, "fon" },
    { language::fra, "fra" },
    { language::fre, "fre" },
    { language::frm, "frm" },
    { language::fro, "fro" },
    { language::frr, "frr" },
    { language::frs, "frs" },
    { language::fry, "fry" },
    { language::ful, "ful" },
    { language::fur, "fur" },
    { language::gaa, "gaa" },
    { language::gay, "gay" },
    { language::gba, "gba" },
    { language::gem, "gem" },
    { language::geo, "geo" },
    { language::ger, "ger" },
    { language::gez, "gez" },
    { language::gil, "gil" },
    { language::gla, "gla" },
    { language::gle, "gle" },
    { language::glg, "glg" },
    { language::glv, "glv" },
    { language::gmh, "gmh" },
    { language::goh, "goh" },
    { language::gon, "gon" },
    { language::gor, "gor" },
    { language::got, "got" },
    { language::grb, "grb" },
    { language::grc, "grc" },
    { language::gre, "gre" },
    { language::grn, "grn" },
    { language::gsw, "gsw" },
    { language::guj, "guj" },
    { language::gwi, "gwi" },
    { language::hai, "hai" },
    { language::hat, "hat" },
    { language::hau, "hau" },
    { language::haw, "haw" },
    { language::heb, "heb" },
    { language::her, "her" },
    { language::hil, "hil" },
    { language::him, "him" },
    { language::hin, "hin" },
    { language::hit, "hit" },
    { language::hmn, "hmn" },
    { language::hmo, "hmo" },
    { language::hrv, "hrv" },
    { language::hsb, "hsb" },
    { language::hun, "hun" },
    { language::hup, "hup" },
    { language::hye, "hye" },
    { language::iba, "iba" },
    { language::ibo, "ibo" },
    { language::ice, "ice" },
    { language::ido, "ido" },
    { language::iii, "iii" },
    { language::ijo, "ijo" },
    { language::iku, "iku" },
    { language::ile, "ile" },
    { language::ilo, "ilo" },
    { language::ina, "ina" },
    { language::inc, "inc" },
    { language::ind, "ind" },
    { language::ine, "ine" },
    { language::inh, "inh" },
    { language::ipk, "ipk" },
    { language::ira, "ira" },
    { language::iro, "iro" },
    { language::isl, "isl" },
    { language::ita, "ita" },
    { language::jav, "jav" },
    { language::jbo, "jbo" },
    { language::jpn, "jpn" },
    { language::jpr, "jpr" },
    { language::jrb, "jrb" },
    { language::kaa, "kaa" },
    { language::kab, "kab" },
    { language::kac, "kac" },
    { language::kal, "kal" },
    { language::kam, "kam" },
    { language::kan, "kan" },
    { language::kar, "kar" },
    { language::kas, "kas" },
    { language::kat, "kat" },
    { language::kau, "kau" },
    { language::kaw, "kaw" },
    { language::kaz, "kaz" },
    { language::kbd, "kbd" },
    { language::kha, "kha" },
    { language::khi, "khi" },
    { language::khm, "khm" },
    { language::kho, "kho" },
    { language::kik, "kik" },
    { language::kin, "kin" },
    { language::kir, "kir" },
    { language::kmb, "kmb" },
    { language::kok, "kok" },
    { language::kom, "kom" },
    { language::kon, "kon" },
    { language::kor, "kor" },
    { language::kos, "kos" },
    { language::kpe, "kpe" },
    { language::krc, "krc" },
    { language::krl, "krl" },
    { language::kro, "kro" },
    { language::kru, "kru" },
    { language::kua, "kua" },
    { language::kum, "kum" },
    { language::kur, "kur" },
    { language::kut, "kut" },
    { language::lad, "lad" },
    { language::lah, "lah" },
    { language::lam, "lam" },
    { language::lao, "lao" },
    { language::lat, "lat" },
    { language::lav, "lav" },
    { language::lez, "lez" },
    { language::lim, "lim" },
    { language::lin, "lin" },
    { language::lit, "lit" },
    { language::lol, "lol" },
    { language::loz, "loz" },
    { language::ltz, "ltz" },
    { language::lua, "lua" },
    { language::lub, "lub" },
    { language::lug, "lug" },
    { language::lui, "lui" },
    { language::lun, "lun" },
    { language::luo, "luo" },
    { language::lus, "lus" },
    { language::mac, "mac" },
    { language::mad, "mad" },
    { language::mag, "mag" },
    { language::mah, "mah" },
    { language::mai, "mai" },
    { language::mak, "mak" },
    { language::mal, "mal" },
    { language::man, "man" },
    { language::mao, "mao" },
    { language::MAP, "MAP" },
    { language::mar, "mar" },
    { language::mas, "mas" },
    { language::may, "may" },
    { language::mdf, "mdf" },
    { language::mdr, "mdr" },
    { language::men, "men" },
    { language::mga, "mga" },
    { language::mic, "mic" },
    { language::MIN, "MIN" },
    { language::mis, "mis" },
    { language::mkd, "mkd" },
    { language::mkh, "mkh" },
    { language::mlg, "mlg" },
    { language::mlt, "mlt" },
    { language::mnc, "mnc" },
    { language::mni, "mni" },
    { language::mno, "mno" },
    { language::moh, "moh" },
    { language::mon, "mon" },
    { language::mos, "mos" },
    { language::mri, "mri" },
    { language::msa, "msa" },
    { language::mul, "mul" },
    { language::mun, "mun" },
    { language::mus, "mus" },
    { language::mwl, "mwl" },
    { language::mwr, "mwr" },
    { language::mya, "mya" },
    { language::myn, "myn" },
    { language::myv, "myv" },
    { language::nah, "nah" },
    { language::nai, "nai" },
    { language::nap, "nap" },
    { language::nau, "nau" },
    { language::nav, "nav" },
    { language::nbl, "nbl" },
    { language::nde, "nde" },
    { language::ndo, "ndo" },
    { language::nds, "nds" },
    { language::nep, "nep" },
    { language::NEW, "NEW" },
    { language::nia, "nia" },
    { language::nic, "nic" },
    { language::niu, "niu" },
    { language::nld, "nld" },
    { language::nno, "nno" },
    { language::nob, "nob" },
    { language::nog, "nog" },
    { language::non, "non" },
    { language::nor, "nor" },
    { language::nqo, "nqo" },
    { language::nso, "nso" },
    { language::nub, "nub" },
    { language::nwc, "nwc" },
    { language::nya, "nya" },
    { language::nym, "nym" },
    { language::nyn, "nyn" },
    { language::nyo, "nyo" },
    { language::nzi, "nzi" },
    { language::oci, "oci" },
    { language::oji, "oji" },
    { language::ori, "ori" },
    { language::orm, "orm" },
    { language::osa, "osa" },
    { language::oss, "oss" },
    { language::ota, "ota" },
    { language::oto, "oto" },
    { language::paa, "paa" },
    { language::pag, "pag" },
    { language::pal, "pal" },
    { language::pam, "pam" },
    { language::pan, "pan" },
    { language::pap, "pap" },
    { language::pau, "pau" },
    { language::peo, "peo" },
    { language::per, "per" },
    { language::phi, "phi" },
    { language::phn, "phn" },
    { language::pli, "pli" },
    { language::pol, "pol" },
    { language::pon, "pon" },
    { language::por, "por" },
    { language::pra, "pra" },
    { language::pro, "pro" },
    { language::pus, "pus" },
    { language::qaa, "qaa" },
    { language::que, "que" },
    { language::raj, "raj" },
    { language::rap, "rap" },
    { language::rar, "rar" },
    { language::roa, "roa" },
    { language::roh, "roh" },
    { language::rom, "rom" },
    { language::ron, "ron" },
    { language::rum, "rum" },
    { language::run, "run" },
    { language::rup, "rup" },
    { language::rus, "rus" },
    { language::sad, "sad" },
    { language::sag, "sag" },
    { language::sah, "sah" },
    { language::sai, "sai" },
    { language::sal, "sal" },
    { language::sam, "sam" },
    { language::san, "san" },
    { language::sas, "sas" },
    { language::sat, "sat" },
    { language::scn, "scn" },
    { language::sco, "sco" },
    { language::sel, "sel" },
    { language::sem, "sem" },
    { language::sga, "sga" },
    { language::sgn, "sgn" },
    { language::shn, "shn" },
    { language::sid, "sid" },
    { language::sin, "sin" },
    { language::sio, "sio" },
    { language::sit, "sit" },
    { language::sla, "sla" },
    { language::slo, "slo" },
    { language::slk, "slk" },
    { language::slv, "slv" },
    { language::sma, "sma" },
    { language::sme, "sme" },
    { language::smi, "smi" },
    { language::smj, "smj" },
    { language::smn, "smn" },
    { language::smo, "smo" },
    { language::sms, "sms" },
    { language::sna, "sna" },
    { language::snd, "snd" },
    { language::snk, "snk" },
    { language::sog, "sog" },
    { language::som, "som" },
    { language::son, "son" },
    { language::sot, "sot" },
    { language::spa, "spa" },
    { language::sqi, "sqi" },
    { language::srd, "srd" },
    { language::srn, "srn" },
    { language::srp, "srp" },
    { language::srr, "srr" },
    { language::ssa, "ssa" },
    { language::ssw, "ssw" },
    { language::suk, "suk" },
    { language::sun, "sun" },
    { language::sus, "sus" },
    { language::sux, "sux" },
    { language::swa, "swa" },
    { language::swe, "swe" },
    { language::syc, "syc" },
    { language::syr, "syr" },
    { language::tah, "tah" },
    { language::tai, "tai" },
    { language::tam, "tam" },
    { language::tat, "tat" },
    { language::tel, "tel" },
    { language::tem, "tem" },
    { language::ter, "ter" },
    { language::tet, "tet" },
    { language::tgk, "tgk" },
    { language::tgl, "tgl" },
    { language::tha, "tha" },
    { language::tib, "tib" },
    { language::tig, "tig" },
    { language::tir, "tir" },
    { language::tiv, "tiv" },
    { language::tkl, "tkl" },
    { language::tlh, "tlh" },
    { language::tli, "tli" },
    { language::tmh, "tmh" },
    { language::tog, "tog" },
    { language::ton, "ton" },
    { language::tpi, "tpi" },
    { language::tsi, "tsi" },
    { language::tsn, "tsn" },
    { language::tso, "tso" },
    { language::tuk, "tuk" },
    { language::tum, "tum" },
    { language::tup, "tup" },
    { language::tur, "tur" },
    { language::tut, "tut" },
    { language::tvl, "tvl" },
    { language::twi, "twi" },
    { language::tyv, "tyv" },
    { language::udm, "udm" },
    { language::uga, "uga" },
    { language::uig, "uig" },
    { language::ukr, "ukr" },
    { language::umb, "umb" },
    { language::und, "und" },
    { language::urd, "urd" },
    { language::uzb, "uzb" },
    { language::vai, "vai" },
    { language::ven, "ven" },
    { language::vie, "vie" },
    { language::vol, "vol" },
    { language::vot, "vot" },
    { language::wak, "wak" },
    { language::wal, "wal" },
    { language::war, "war" },
    { language::was, "was" },
    { language::wel, "wel" },
    { language::wen, "wen" },
    { language::wln, "wln" },
    { language::wol, "wol" },
    { language::xal, "xal" },
    { language::xho, "xho" },
    { language::yao, "yao" },
    { language::yap, "yap" },
    { language::yid, "yid" },
    { language::yor, "yor" },
    { language::ypk, "ypk" },
    { language::zap, "zap" },
    { language::zbl, "zbl" },
    { language::zen, "zen" },
    { language::zgh, "zgh" },
    { language::zha, "zha" },
    { language::zho, "zho" },
    { language::znd, "znd" },
    { language::zul, "zul" },
    { language::zun, "zun" },
    { language::zxx, "zxx" },
    { language::zza, "zza" },
  };

  return os << TBL.at(x);
}
