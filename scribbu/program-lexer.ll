%{
  /**
   * \file program-lexer.ll
   *
   * \brief Gnu flex lexer specifications for the scribbu programming language
   *
   *
   * Note that this will be processed into a .cc file by autoconf.
   *
   *
   */

  #include "program-parser.hh"

%}

%option prefix="program" outfile="program-lexer.cc" header-file="program-lexer.hh" noyywrap debug

album              album
and                &&
artist             artist
contains           contains
content_type       content-type
else               else
encoded_by         encoded-by
equal              ==
greater_than       >
greater_than_equal >=
if                 if
integer            [0-9]+
less_than          <
less_than_equal    <=
not_equal          !=
not_present        not-present
or                 \|\|
present            present
print              print
set                set
title              title
year               year

%x T

%%

\"                   { BEGIN(T);                    }
<T>\\\"              {                              }
<T>\"                { BEGIN(INITIAL); return TEXT; }
<T>.                 {                              }

{album}              { return ALBUM;              }
{and}                { return AND;                }
{artist}             { return ARTIST;             }
{contains}           { return CONTAINS;           }
{content_type}       { return CONTENT_TYPE;       }
{else}               { return ELSE;               }
{encoded_by}         { return ENCODED_BY;         }
{equal}              { return EQUAL;              }
{greater_than}       { return GREATER_THAN;       }
{greater_than_equal} { return GREATER_THAN_EQUAL; }
{if}                 { return IF;                 }
{integer}            { return INTEGER;            }
{less_than}          { return LESS_THAN;          }
{less_than_equal}    { return LESS_THAN_EQUAL;    }
{not_equal}          { return NOT_EQUAL;          }
{not_present}        { return NOT_PRESENT;        }
{or}                 { return OR;                 }
{present}            { return PRESENT;            }
{print}              { return PRINT;              }
{set}                { return SET;                }
{title}              { return TITLE;              }
{year}               { return YEAR;               }

[(){}!=] { return yytext[0]; }

[ \t\n]

%%

// WTF?
