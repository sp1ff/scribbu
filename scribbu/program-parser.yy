%{
  /**
   * \file program-parser.yy
   *
   * \brief GNU bison grammar specification for scribbu programs
   *
   *
   * Note that this file will be processed into a .cc file by autoconf.
   *
   */

  #include <scribbu/program-lexer.hh>

  #include <stdexcept>
  #include <iostream>
  #include <vector>
  // Un-comment to enable debugging
  #define YYDEBUG 1

%}

%define api.prefix {program}
%define parse.lac full
%define parse.error verbose
%debug

%code requires {

  #include <string>
  #include <scribbu/program-support.hh>

  static scribbu::program::tag_type
  map_text_tag_to_tag(scribbu::program::text_tag_type);

  static void
  programerror(scribbu::program::start_symbol*, const char *s);

}

%union {
  std::string 	                       	    *text;
  long        	                       	     integer;
  scribbu::program::tag_type           	     tag;
  scribbu::program::term               	    *term;
  scribbu::program::expression         	    *expression;
  scribbu::program::text_expression    	    *text_expression;
  scribbu::program::text_tag_type      	     text_tag;
  scribbu::program::integer_expression 	    *integer_expression;
  scribbu::program::directive          	    *directive;
  scribbu::program::set_tag            	    *set_tag;
  scribbu::program::condition          	    *condition;
  scribbu::program::conditional        	    *conditional;
  scribbu::program::statement          	    *statement;
  std::vector<scribbu::program::statement*> *statement_list;
  scribbu::program::start_symbol*            start_symbol;
}

%token ALBUM
%token AND
%token ARTIST
%token CONTAINS
%token CONTENT_TYPE
%token ELSE
%token ENCODED_BY
%token EQUAL
%token GREATER_THAN
%token GREATER_THAN_EQUAL
%token IF
%token LESS_THAN
%token LESS_THAN_EQUAL
%token NOT_EQUAL
%token NOT_PRESENT
%token OR
%token PRESENT
%token PRINT
%token SET
%token TITLE
%token YEAR

%token<integer> INTEGER
%token<text> TEXT

// %token<union_member_naem> TOKEN
// %type <union_member_name> term_name
%type<tag>                tag
%type<term>               term
%type<expression>         expression
%type<text_expression>    text_expression
%type<text_tag>           text_tag
%type<integer_expression> integer_expression
%type<directive>          directive
%type<set_tag>            set_tag
%type<condition>          condition
%type<conditional>        conditional
%type<statement>          statement
%type<statement_list>     statement_list
%type<start_symbol>       start;

%parse-param {scribbu::program::start_symbol *p}

%%

start: condition {
         $$ = new scribbu::program::start_symbol;
	     $$->fcondition_ = true;
	     $$->pcondition_ = $1;
       }
     | statement_list {
         $$ = new scribbu::program::start_symbol;
	     $$->fcondition_ = false;
	     $$->pstatement_list_ = $1;
       }
     ;

statement_list: statement {
		  $$ = new std::vector<scribbu::program::statement*>();
		  $$->push_back($1);
                }
              | statement_list statement {
	          $$ = $1;
		  $$->push_back($2);
                }
              ;

statement: conditional {
	     $$ = new scribbu::program::statement($1);
           }
         | directive {
	     $$ = new scribbu::program::statement($1);
           }
         ;

conditional: IF '(' condition ')' '{' directive '}' {
               $$ = new scribbu::program::conditional($3, $6);
             }
           | IF '(' condition ')' '{' directive '}' ELSE '{' directive '}' {
               $$ = new scribbu::program::conditional($3, $6, $10);
             }
           ;

condition: term {
             $$ = new scribbu::program::term_condition($1);
           }
         | condition AND condition {
             $$ = new scribbu::program::and_condition($1, $3);
           }
         | condition OR condition {
             $$ = new scribbu::program::or_condition($1, $3);
           }
         | '!' condition {
             $$ = new scribbu::program::not_condition($2);
           }
         | '(' condition ')' {
             $$ = $2;
           }
         ;

term: expression EQUAL expression {
        $$ = new scribbu::program::equality_term($1, $3);
      }
    | expression NOT_EQUAL expression {
        $$ = new scribbu::program::equality_term($1, $3,
          scribbu::program::equality_term::relation::inequality);
      }
    | tag PRESENT {
        $$ = new scribbu::program::presence_term($1);
      }
    | tag NOT_PRESENT {
        $$ = new scribbu::program::presence_term($1, true);
      }
    | integer_expression LESS_THAN integer_expression {
        $$ = new scribbu::program::equality_term($1, $3,
          scribbu::program::equality_term::relation::less_than);
      }
    | integer_expression LESS_THAN_EQUAL integer_expression {
        $$ = new scribbu::program::equality_term($1, $3,
          scribbu::program::equality_term::relation::less_than_equal);
      }
    | integer_expression GREATER_THAN integer_expression {
        $$ = new scribbu::program::equality_term($1, $3,
          scribbu::program::equality_term::relation::greater_than);
      }
    | integer_expression GREATER_THAN_EQUAL integer_expression {
        $$ = new scribbu::program::equality_term($1, $3,
          scribbu::program::equality_term::relation::greater_than_equal);
      }
    | text_expression CONTAINS text_expression {
        $$ = new scribbu::program::textual_term($1, $3,
	  scribbu::program::textual_term::relation::contains);
      }
    ;

expression: text_expression {
              $$ = $1;
            }
          | integer_expression  {
              $$ = $1;
            };

text_expression: text_tag {
                   $$ = new scribbu::program::get_text_tag($1);
                 }
               | TEXT {
                   $$ = new scribbu::program::static_text($1);
                 }
               ;

tag: text_tag {
       $$ = map_text_tag_to_tag($1);
     }
     ;

text_tag: ALBUM {
            $$ = scribbu::program::text_tag_type::album;
          }
        | ARTIST {
            $$ = scribbu::program::text_tag_type::artist;
          }
        | CONTENT_TYPE {
            $$ = scribbu::program::text_tag_type::content_type;
          }
        | ENCODED_BY {
            $$ = scribbu::program::text_tag_type::encoded_by;
          }
        | TITLE {
            $$ = scribbu::program::text_tag_type::title;
          }
        | YEAR {
            $$ = scribbu::program::text_tag_type::year;
          }
        ;

integer_expression: INTEGER {
                      $$ = new scribbu::program::integer_literal($1);
                    }
                    ;

directive: PRINT {
             $$ = new scribbu::program::print();
           }
         | set_tag {
             $$ = $1;
           }
         ;

set_tag: SET text_tag '=' text_expression {
           $$ = new scribbu::program::set_text_tag($2, $4);
         }
         ;

%%

static scribbu::program::tag_type
map_text_tag_to_tag(scribbu::program::text_tag_type ttt)
{
  switch (ttt) {
  case scribbu::program::text_tag_type::album:
    return scribbu::program::tag_type::album;
  case scribbu::program::text_tag_type::artist:
    return scribbu::program::tag_type::artist;
  case scribbu::program::text_tag_type::content_type:
    return scribbu::program::tag_type::content_type;
  case scribbu::program::text_tag_type::encoded_by:
    return scribbu::program::tag_type::encoded_by;
  case scribbu::program::text_tag_type::title:
    return scribbu::program::tag_type::title;
  case scribbu::program::text_tag_type::year:
    return scribbu::program::tag_type::year;
  default:
    throw std::logic_error("Unknown text tag type!");
  }
}

static void programerror(scribbu::program::start_symbol*, const char *s) {
  // TODO: Replace this!
  using namespace std;
  cerr << s << endl;
}
