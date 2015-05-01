/*
 * parser.mly
 *
 *     bogoscheme parser.
 *
 */

%{

(* header *)

%}
  
/* declarations */

%token TOK_LPAREN TOK_RPAREN
%token          TOK_UNIT
%token <bool>   TOK_BOOL
%token <int>    TOK_INT
%token <string> TOK_ID
%token          TOK_EOF

%start parse
%type <Sexpr.expr option> parse
%type <Sexpr.expr>        sexpr
%type <Sexpr.atom>        atom
%type <Sexpr.expr list>   slist
%type <Sexpr.expr list>   sexpr_list

%%

/* rules */

parse:
  /* TODO */

sexpr:
  /* TODO */

atom:  
  /* TODO */

slist:
  /* TODO */

sexpr_list:
  /* TODO */

%%

(* trailer *)



