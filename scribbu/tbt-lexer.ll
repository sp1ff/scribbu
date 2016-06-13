%{
  /**
   * \file tbt.ll
   *
   * \brief GNU flex lexer specification for tag-based templates
   *
   *
   * Note that this will be processed into a .cc file by autoconf.
   *
   *
   */

  #include "tbt-parser.hh"

  static std::string * unescape_and_copy(const char *);

%}

%option prefix="tbt" outfile="tbt-lexer.cc" header-file="tbt-lexer.hh" noyywrap

text          ([^?%:&)(=\\]|\\[?%:&)(=\\])+

album        album
artist       artist
basename     (file-)?basename
content_type content-type|genre
encoded_by   encoded-by
extension    (file-)?extension
languages    languages
md5          md5
size         size
title        title
year         year

base         base
cap          capitalization
compress     compress
hex_case     hex-case
output       output
the          the
v1_encoding  v1-encoding

all_lower    all-lower
all_upper    all-upper
ascii        ascii
auto         auto
capitalize   capitalize
cp1252       cp1252
decimal      decimal
four_digits  yyyy
hex          hex
iso8859_1    iso-8859-1
lower        L
pref_v1      prefer-v1
pref_v2      prefer-v2
prefix       prefix
suffix       suffix
two_digits   yy
upper        U
utf8         utf-8
utf16be      utf-16-be
utf16le      utf-16-le
utf32        utf-32
v1_only      v1-only
v2_only      v2-only
ws           ws

album_abbrev        L
artist_abbrev       A
basename_abbrev     b
content_type_abbrev G
encoded_by_abbrev   e
extension_abbrev    E
languages_abbrev    a
md5_abbrev          5
size_abbrev         S
title_abbrev        T
year_abbrev         Y

%x REPLACEMENT
%x PARAMETER

%%

{text} { tbtlval.text = unescape_and_copy(yytext); return TEXT; }

"%"                         { BEGIN(REPLACEMENT); return PCT; }
<REPLACEMENT,INITIAL>"("    { return LPAREN;                  }
<REPLACEMENT,INITIAL>")"    { BEGIN(INITIAL); return RPAREN;  }
<REPLACEMENT>":"            { return COLON;                   }
<REPLACEMENT>"&"            { return AMP;                     }
<REPLACEMENT,INITIAL>"?"    { return QUESTION;                }
<REPLACEMENT>"="            { BEGIN(PARAMETER); return EQUAL; }

<REPLACEMENT>{album}        { return ALBUM;         }
<REPLACEMENT>{artist}       { return ARTIST;        }
<REPLACEMENT>{basename}     { return BASENAME;      }
<REPLACEMENT>{content_type} { return CONTENT_TYPE;  }
<REPLACEMENT>{encoded_by}   { return ENCODED_BY;    }
<REPLACEMENT>{extension}    { return EXTENSION;     }
<REPLACEMENT>{md5}          { return MD5;           }
<REPLACEMENT>{size}         { return SIZE;          }
<REPLACEMENT>{the}          { return THE;           }
<REPLACEMENT>{title}        { return TITLE;         }
<REPLACEMENT>{year}         { return YEAR;          }

<REPLACEMENT>{base}         { return BASE;        }
<REPLACEMENT>{cap}          { return CAP;         }
<REPLACEMENT>{output}       { return OUTPUT;      }
<REPLACEMENT>{compress}     { return COMPRESS_WS; }
<REPLACEMENT>{pref_v1}      { return PREF_V1;     }
<REPLACEMENT>{pref_v2}      { return PREF_V2;     }
<REPLACEMENT>{v1_encoding}  { return V1ENCODING;  }

<PARAMETER>{two_digits}     { BEGIN(REPLACEMENT); return TWO_DIGITS;  }
<PARAMETER>{four_digits}    { BEGIN(REPLACEMENT); return FOUR_DIGITS; }
<PARAMETER>{ascii}          { BEGIN(REPLACEMENT); return ASCII;       }
<PARAMETER>{auto}           { BEGIN(REPLACEMENT); return AUTO;        }
<PARAMETER>{all_lower}      { BEGIN(REPLACEMENT); return ALL_LOWER;   }
<PARAMETER>{all_upper}      { BEGIN(REPLACEMENT); return ALL_UPPER;   }
<PARAMETER>{capitalize}     { BEGIN(REPLACEMENT); return CAPITALIZE;  }
<PARAMETER>{cp1252}         { BEGIN(REPLACEMENT); return CP1252;      }
<PARAMETER>{decimal}        { BEGIN(REPLACEMENT); return DECIMAL;     }
<PARAMETER>{hex}            { BEGIN(REPLACEMENT); return HEX;         }
<PARAMETER>{hex_case}       { BEGIN(REPLACEMENT); return HEX_CASE;    }
<PARAMETER>{iso8859_1}      { BEGIN(REPLACEMENT); return ISO8859_1;   }
<PARAMETER>{lower}          { BEGIN(REPLACEMENT); return LOWER;       }
<PARAMETER>{prefix}         { BEGIN(REPLACEMENT); return PREFIX;      }
<PARAMETER>{suffix}         { BEGIN(REPLACEMENT); return SUFFIX;      }
<PARAMETER>{upper}          { BEGIN(REPLACEMENT); return UPPER;       }
<PARAMETER>{utf8}           { BEGIN(REPLACEMENT); return UTF8;        }
<PARAMETER>{utf16be}        { BEGIN(REPLACEMENT); return UTF16_BE;    }
<PARAMETER>{utf16le}        { BEGIN(REPLACEMENT); return UTF16_LE;    }
<PARAMETER>{utf32}          { BEGIN(REPLACEMENT); return UTF32;       }
<PARAMETER>{v1_only}        { BEGIN(REPLACEMENT); return V1_ONLY;     }
<PARAMETER>{v2_only}        { BEGIN(REPLACEMENT); return V2_ONLY;     }
<PARAMETER>{ws}             { BEGIN(REPLACEMENT); return WS;          }

<PARAMETER>{text} {
  BEGIN(REPLACEMENT);
  tbtlval.text = unescape_and_copy(yytext);
  return PARAM_TEXT;
}

<REPLACEMENT>{album_abbrev}        {BEGIN(INITIAL);return ALBUM_ABBREV;       }
<REPLACEMENT>{artist_abbrev}       {BEGIN(INITIAL);return ARTIST_ABBREV;      }
<REPLACEMENT>{basename_abbrev}     {BEGIN(INITIAL);return BASENAME_ABBREV;    }
<REPLACEMENT>{content_type_abbrev} {BEGIN(INITIAL);return CONTENT_TYPE_ABBREV;}
<REPLACEMENT>{encoded_by_abbrev}   {BEGIN(INITIAL);return ENCODED_BY_ABBREV;  }
<REPLACEMENT>{extension_abbrev}    {BEGIN(INITIAL);return EXTENSION_ABBREV;   }
<REPLACEMENT>{md5_abbrev}          {BEGIN(INITIAL);return MD5_ABBREV;         }
<REPLACEMENT>{size_abbrev}         {BEGIN(INITIAL);return SIZE_ABBREV;        }
<REPLACEMENT>{title_abbrev}        {BEGIN(INITIAL);return TITLE_ABBREV;       }
<REPLACEMENT>{year_abbrev}         {BEGIN(INITIAL);return YEAR_ABBREV;        }

<INITIAL,REPLACEMENT,PARAMETER>. {
  std::cerr << "Matched unknown character: " << yytext[0] << std::endl;
  return UNKNOWN;
}

%%

static std::string * unescape_and_copy(const char *text) {

  std::string *p = new std::string();

  int saw_backslash = 0;
  size_t n = strlen(text), i = 0;

  for (i = 0; i < n; ++i) {
    if (1 == saw_backslash) {
      switch (text[i]) {
      case '%':
      case '(':
      case ')':
      case ':':
      case '?':
      case '&':
      case '\\':
        break;
      default:
        *p += '\\';
        break;
      }
      *p += text[i];
      saw_backslash = 0;
    } else {
      if ('\\' == text[i]) {
        saw_backslash = 1;
      } else {
        *p += text[i];
      }
    }
  }
  if (1 == saw_backslash) {
    *p += '\\';
  }

  return p;
}
