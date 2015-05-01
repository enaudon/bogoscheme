(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
module P = Parser
}

(* Some useful definitions. *)
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']*

(* The lexer definition itself. *)
rule lex = parse
  | whitespace      { lex lexbuf }
  | ';'             { comment lexbuf }

  | '('             { P.TOK_LPAREN }
  | ')'             { P.TOK_RPAREN }

  | "#u"            { P.TOK_UNIT }
  | "#t"            { P.TOK_BOOL true }
  | "#f"            { P.TOK_BOOL false }
  | integer as n    { P.TOK_INT (int_of_string n) }
  | id_chars as id  { P.TOK_ID id }

  | eof             { P.TOK_EOF }

  | _          {
      raise (Failure (" token: " ^ (Lexing.lexeme lexbuf)))
    }

(* Handle comments (by ignoring them) *)
and comment = parse
  | '\n'        { lex lexbuf }
  | _           { comment lexbuf }

{
(* Nothing. *)
}

