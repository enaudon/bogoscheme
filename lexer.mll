(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
open Parser
}

(* Some useful definitions. *)
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']

(* The lexer definition itself. *)
rule lex = parse
  (* TODO: Fill in rules for different token types. *)

  (* lexer error -- this should never happen *)
  | _               { 
      raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) 
    }

{
(* Nothing. *)
}

      
