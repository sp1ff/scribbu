%{
  /**
   * \file tbt.yy
   *
   * \brief GNU bison grammar specification for tag-based-templates
   *
   *
   * Note that this will be processed into a .cc file by autoconf.
   *
   * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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
   * along with scribbu.  If not, see <http://www.gnu.org/licenses/>.
   *
   *
   */

  #include <scribbu/tbt-lexer.hh>
  #include <vector>

  #include <iostream>
    // #define YYDEBUG 1  // Un-comment to enable debug output

%}

%define api.prefix {tbt}
%define parse.lac full
%define parse.error verbose

%code requires {

  #include <scribbu/tbt-support.hh>

}

%union {

  std::vector<scribbu::tbt_support::term*> *term_list;

  scribbu::tbt_support::aacet_term *aacet_term;
  scribbu::tbt_support::file_term *file_term;
  scribbu::tbt_support::term *term;

  std::map<scribbu::tbt_support::aacet_opt, boost::any> *aacet_opts;
  std::pair<const scribbu::tbt_support::aacet_opt, boost::any> *aacet_opt;
  std::map<scribbu::tbt_support::file_opt, boost::any> *file_opts;
  std::pair<const scribbu::tbt_support::file_opt, boost::any> *file_opt;
  std::map<scribbu::tbt_support::md5_opt, boost::any> *md5_opts;
  std::pair<const scribbu::tbt_support::md5_opt, boost::any> *md5_opt;
  std::map<scribbu::tbt_support::size_opt, boost::any> *size_opts;
  std::pair<const scribbu::tbt_support::size_opt, boost::any> *size_opt;
  std::map<scribbu::tbt_support::ws_xform, boost::any> *ws_xforms;
  std::pair<const scribbu::tbt_support::ws_xform, boost::any> *ws_xform;

  std::string *text;

  scribbu::tbt_support::year_format year_format;
  scribbu::tbt_support::all_source_preference all_source_preference;
  scribbu::tbt_support::v1_encoding v1_encoding;
  scribbu::tbt_support::the_xform the_xform;
  scribbu::tbt_support::capitalization capitalization;
  scribbu::tbt_support::output_encoding output_encoding;
  scribbu::tbt_support::base base;
  scribbu::tbt_support::hex_case hex_case;


}

%{

  static void
  tbterror(std::vector<scribbu::tbt_support::term*>**, const char *s);

%}

%token ALBUM
%token ALBUM_ABBREV
%token ALL_LOWER
%token ALL_UPPER
%token AMP
%token ARTIST
%token ARTIST_ABBREV
%token ASCII
%token AUTO
%token BASE
%token BASENAME
%token BASENAME_ABBREV
%token CAP
%token CAPITALIZE
%token COLON
%token COMPRESS_WS
%token CONTENT_TYPE
%token CONTENT_TYPE_ABBREV
%token CP1252
%token DECIMAL
%token ENCODED_BY
%token ENCODED_BY_ABBREV
%token EQUAL
%token EXTENSION
%token EXTENSION_ABBREV
%token FOUR_DIGITS
%token HEX
%token HEX_CASE
%token ISO8859_1
%token LOWER
%token LPAREN
%token MD5
%token MD5_ABBREV
%token OUTPUT
%token PCT
%token PREFIX
%token PREF_V1
%token PREF_V2
%token QUESTION
%token RPAREN
%token SIZE
%token SIZE_ABBREV
%token SUFFIX
%token THE
%token TITLE
%token TITLE_ABBREV
%token TWO_DIGITS
%token UNKNOWN
%token UPPER
%token UTF16_LE
%token UTF16_BE
%token UTF32
%token UTF8
%token V1ENCODING
%token V1_ONLY
%token V2_ONLY
%token WS
%token YEAR
%token YEAR_ABBREV

%token<text> PARAM_TEXT
%token<text> TEXT

%type <term_list>             term_list;

%type <term>                  term;
%type <term>                  replacement;
%type <term>                  replacement_name;
%type <term>                  replacement_abbrev;
%type <aacet_term>            aacet_replacement_name;
%type <file_term>             file_replacement_name;

%type <aacet_opts>            aacet_opts;
%type <aacet_opt>             aacet_opt;
%type <file_opts>             file_opts;
%type <file_opt>              file_opt;
%type <md5_opts>              md5_opts;
%type <md5_opt>               md5_opt;
%type <size_opts>             size_opts;
%type <size_opt>              size_opt;
%type <ws_xforms>             ws_xforms;
%type <ws_xform>              ws_xform;

%type <year_format>           year_opt;
%type <all_source_preference> all_source_preference;
%type <v1_encoding>           v1_encoding;
%type <the_xform>             the_xform;
%type <capitalization>        cap_xform;
%type <text>                  replace_ws;
%type <output_encoding>       output_encoding;
%type <base>                  base;
%type <hex_case>              hex_case;

%parse-param {std::vector<scribbu::tbt_support::term*> **pterms}

%%

start: term_list {
         *pterms = $1;
       };

term_list: term_list term {
             $1->push_back($2);
             $$ = $1;
           }
           | term {
             $$ = new std::vector<scribbu::tbt_support::term*>();
             $$->push_back($1);
           };

term: TEXT {
        $$ = new scribbu::tbt_support::text_term(*$1);
        delete $1;
      }
      | replacement {
        $$ = $1;
      }
      ;

replacement: PCT LPAREN replacement_name RPAREN {
               $$ = $3;
             }
           | PCT LPAREN aacet_replacement_name COLON aacet_opts RPAREN {
               $3->set_options($5->begin(), $5->end());
               $$ = $3;
               delete $5;
             }
           | PCT LPAREN file_replacement_name COLON file_opts RPAREN {
               $3->set_options($5->begin(), $5->end());
               $$ = $3;
               delete $5;
             }
           | PCT LPAREN MD5 COLON md5_opts RPAREN {
               $$ = new scribbu::tbt_support::md5($5->begin(), $5->end());
               delete $5;
             }
           | PCT LPAREN SIZE COLON size_opts RPAREN {
               $$ = new scribbu::tbt_support::size($5->begin(), $5->end());
               delete $5;
             }
           | PCT LPAREN YEAR COLON year_opt RPAREN {
               $$ = new scribbu::tbt_support::year($5);
             }
           | PCT replacement_abbrev {
               $$ = $2;
           }
           | LPAREN term_list RPAREN QUESTION {
             $$ = new scribbu::tbt_support::subclause($2);
             delete $2;
           }
           ;

///////////////////////////////////////////////////////////////////////////////
//                               replacements                                //
///////////////////////////////////////////////////////////////////////////////

replacement_name: ALBUM {
                    $$ = new scribbu::tbt_support::album();
                  }
                | ARTIST {
                    $$ = new scribbu::tbt_support::artist();
                  }
                | BASENAME {
                    $$ = new scribbu::tbt_support::basename();
                  }
                | CONTENT_TYPE {
                    $$ = new scribbu::tbt_support::content_type();
                  }
                | ENCODED_BY {
                    $$ = new scribbu::tbt_support::encoded_by();
                  }
                | EXTENSION {
                    $$ = new scribbu::tbt_support::extension();
                  }
                | MD5 {
                    $$ = new scribbu::tbt_support::md5();
                  }
                | SIZE {
                    $$ = new scribbu::tbt_support::size();
                  }
                | TITLE {
                    $$ = new scribbu::tbt_support::title();
                  }
                | YEAR {
                    $$ = new scribbu::tbt_support::year();
                  }
                ;

replacement_abbrev: ALBUM_ABBREV {
                      $$ = new scribbu::tbt_support::album();
                    }
                  | ARTIST_ABBREV {
                      $$ = new scribbu::tbt_support::artist();
                    }
                  | BASENAME_ABBREV {
                      $$ = new scribbu::tbt_support::basename();
                    }
                  | CONTENT_TYPE_ABBREV {
                      $$ = new scribbu::tbt_support::content_type();
                    }
                  | ENCODED_BY_ABBREV {
                      $$ = new scribbu::tbt_support::encoded_by();
                    }
                  | EXTENSION_ABBREV {
                      $$ = new scribbu::tbt_support::extension();
                    }
                  | MD5_ABBREV {
                      $$ = new scribbu::tbt_support::md5();
                    }
                  | SIZE_ABBREV {
                      $$ = new scribbu::tbt_support::size();
                    }
                  | TITLE_ABBREV {
                      $$ = new scribbu::tbt_support::title();
                    }
                  | YEAR_ABBREV {
                      $$ = new scribbu::tbt_support::year();
                    }
                  ;

aacet_replacement_name: ALBUM {
                          $$ = new scribbu::tbt_support::album();
                        }
                      | ARTIST {
                          $$ = new scribbu::tbt_support::artist();
                        }
                      | CONTENT_TYPE {
                          $$ = new scribbu::tbt_support::content_type();
                        }
                      | TITLE {
                          $$ = new scribbu::tbt_support::title();
                        }
                      ;

file_replacement_name: BASENAME {
                         $$ = new scribbu::tbt_support::basename();
                       }
                     | EXTENSION {
                         $$ = new scribbu::tbt_support::extension();
                       }
                     ;


///////////////////////////////////////////////////////////////////////////////
//                                  options                                  //
///////////////////////////////////////////////////////////////////////////////

aacet_opts: aacet_opt {
              using namespace scribbu::tbt_support;
              $$ = new std::map<aacet_opt, boost::any>();
              $$->insert(*$1);
              delete $1;
            }
          | aacet_opts AMP aacet_opt {
              using namespace scribbu::tbt_support;

              $$ = $1;

              bool inserted;
              std::map<aacet_opt, boost::any>::iterator p;
              std::tie(p, inserted) = $$->insert(*$3);
              if (!inserted) {
                throw scribbu::tbt_support::duplicate_option($3->first);
              }

              delete $3;
            }
          ;

aacet_opt: all_source_preference {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::all_source_preference, boost::any($1));
           }
         | v1_encoding {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::v1_encoding, boost::any($1));
           }
         | the_xform {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::the_xform, boost::any($1));
           }
         | cap_xform {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::cap_xform, boost::any($1));
           }
         | ws_xforms {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::ws_xforms, boost::any($1));
           }
         | output_encoding {
             using namespace scribbu::tbt_support;
             $$ = new std::pair<const aacet_opt, boost::any>(aacet_opt::output_encoding, boost::any($1));
           }
         ;

file_opts: file_opt {
             using namespace scribbu::tbt_support;
             $$ = new std::map<file_opt, boost::any>();
             $$->insert(*$1);
             delete $1;
           }
         | file_opts AMP file_opt {
             using namespace scribbu::tbt_support;

             $$ = $1;

             bool inserted;
             std::map<file_opt, boost::any>::iterator p;
             std::tie(p, inserted) = $$->insert(*$3);
             if (!inserted) {
               throw scribbu::tbt_support::duplicate_option($3->first);
             }

             delete $3;
           }
         ;

file_opt: the_xform {
            using namespace scribbu::tbt_support;
            $$ = new std::pair<const file_opt, boost::any>(file_opt::the_xform, boost::any($1));
          }
        | cap_xform {
            using namespace scribbu::tbt_support;
            $$ = new std::pair<const file_opt, boost::any>(file_opt::cap_xform, boost::any($1));
          }
        | ws_xforms {
            using namespace scribbu::tbt_support;
            $$ = new std::pair<const file_opt, boost::any>(file_opt::ws_xform, boost::any($1));
          }
        ;

md5_opts: md5_opt {
            using namespace scribbu::tbt_support;
            $$ = new std::map<md5_opt, boost::any>();
            $$->insert(*$1);
            delete $1;
          }
        | md5_opts AMP md5_opt {
            using namespace scribbu::tbt_support;

            $$ = $1;

            bool inserted;
            std::map<md5_opt, boost::any>::iterator p;
            std::tie(p, inserted) = $$->insert(*$3);
            if (!inserted) {
              throw scribbu::tbt_support::duplicate_option($3->first);
            }

            delete $3;
          }
        ;

md5_opt: base {
           using namespace scribbu::tbt_support;
           $$ = new std::pair<const md5_opt, boost::any>(md5_opt::base, boost::any($1));
         }
       | hex_case {
           using namespace scribbu::tbt_support;
           $$ = new std::pair<const md5_opt, boost::any>(md5_opt::hex_case, boost::any($1));
         }
       ;

size_opts: size_opt {
             using namespace scribbu::tbt_support;
             $$ = new std::map<size_opt, boost::any>();
             $$->insert(*$1);
             delete $1;
           }
         | size_opts AMP size_opt {
             using namespace scribbu::tbt_support;

             $$ = $1;

             bool inserted;
             std::map<size_opt, boost::any>::iterator p;
             std::tie(p, inserted) = $$->insert(*$3);
             if (!inserted) {
               throw scribbu::tbt_support::duplicate_option($3->first);
             }

             delete $3;
           }
           ;

size_opt: base {
            using namespace scribbu::tbt_support;
            $$ = new std::pair<const size_opt, boost::any>(size_opt::base, boost::any($1));
          }
        | hex_case  {
            using namespace scribbu::tbt_support;
            $$ = new std::pair<const size_opt, boost::any>(size_opt::hex_case, boost::any($1));
          }
        ;


year_opt: TWO_DIGITS {
            $$ = scribbu::tbt_support::year_format::two_digits;
          }
        | FOUR_DIGITS {
            $$ = scribbu::tbt_support::year_format::four_digits;
          }
          ;

all_source_preference: PREF_V2 {
                         $$ = scribbu::tbt_support::all_source_preference::prefer_id3v2;
                       }
                       | PREF_V1 {
                         $$ = scribbu::tbt_support::all_source_preference::prefer_id3v1;
                       }
                       | V2_ONLY {
                         $$ = scribbu::tbt_support::all_source_preference::id3v2_only;
                       }
                       | V1_ONLY {
                         $$ = scribbu::tbt_support::all_source_preference::id3v1_only;
                       }
                       ;

v1_encoding: V1ENCODING EQUAL AUTO {
               $$ = scribbu::tbt_support::v1_encoding::automatic;
             }
           | V1ENCODING EQUAL ISO8859_1 {
               $$ = scribbu::tbt_support::v1_encoding::iso8859_1;
             }
           | V1ENCODING EQUAL ASCII {
               $$ = scribbu::tbt_support::v1_encoding::ascii;
             }
           | V1ENCODING EQUAL CP1252 {
               $$ = scribbu::tbt_support::v1_encoding::cp1252;
             }
           | V1ENCODING EQUAL UTF8 {
               $$ = scribbu::tbt_support::v1_encoding::utf_8;
             }
           ;

the_xform: THE {
             $$ = scribbu::tbt_support::the_xform::make_suffix;
           }
         | THE EQUAL PREFIX {
             $$ = scribbu::tbt_support::the_xform::make_prefix;
           }
         | THE EQUAL SUFFIX {
             $$ = scribbu::tbt_support::the_xform::make_suffix;
           }
         ;

cap_xform: CAP {
             $$ = scribbu::tbt_support::capitalization::capitalize;
           }
         | CAP EQUAL ALL_UPPER {
             $$ = scribbu::tbt_support::capitalization::all_upper;
           }
         | CAP EQUAL ALL_LOWER{
             $$ = scribbu::tbt_support::capitalization::all_lower;
           }
         | CAP EQUAL CAPITALIZE{
             $$ = scribbu::tbt_support::capitalization::capitalize;
           }
         ;

ws_xforms: ws_xform {
             using namespace scribbu::tbt_support;
             $$ = new std::map<ws_xform, boost::any>();
             $$->insert(*$1);
             delete $1;
           }
         | ws_xforms ws_xform {
             using namespace scribbu::tbt_support;

             $$ = $1;

             bool inserted;
             std::map<ws_xform, boost::any>::iterator p;
             std::tie(p, inserted) = $$->insert(*$2);
             if (!inserted) {
               throw duplicate_option($2->first);
             }

             delete $2;
           }
         ;

ws_xform: COMPRESS_WS {
            using namespace scribbu::tbt_support;
            typedef std::pair<const ws_xform, boost::any> pair_type;
            $$ = new pair_type(ws_xform::compress, boost::any(true));
          }
        | replace_ws {
            using namespace scribbu::tbt_support;
            typedef std::pair<const ws_xform, boost::any> pair_type;
            $$ = new pair_type(ws_xform::replace, boost::any(*$1));
            delete $1;
          }
        ;

replace_ws: WS EQUAL PARAM_TEXT {
              $$ = $3;
            }
            ;

output_encoding: OUTPUT EQUAL UTF8 {
                   $$ = scribbu::tbt_support::output_encoding::utf_8;
                 }
               | OUTPUT EQUAL ASCII {
                   $$ = scribbu::tbt_support::output_encoding::ascii;
                 }
               | OUTPUT EQUAL CP1252 {
                   $$ = scribbu::tbt_support::output_encoding::cp1252;
                 }
               | OUTPUT EQUAL ISO8859_1 {
                   $$ = scribbu::tbt_support::output_encoding::iso8859_1;
                 }
               ;

base: BASE EQUAL DECIMAL {
        $$ = scribbu::tbt_support::base::decimal;
      }
    | BASE EQUAL HEX {
        $$ = scribbu::tbt_support::base::hex;
      }
    ;

hex_case: HEX_CASE EQUAL LOWER {
            $$ = scribbu::tbt_support::hex_case::lower;
          }
        | HEX_CASE EQUAL UPPER {
            $$ = scribbu::tbt_support::hex_case::upper;
          }
        ;

%%

static void tbterror(std::vector<scribbu::tbt_support::term*>**,
                     const char *s) {
  using namespace std;
  cerr << s << endl;
}
