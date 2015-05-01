(*
 * ast.ml
 *
 *     Abstract syntax tree.
 *
 *)

module S = Sexpr

type id = string

type expr =
   | Expr_unit
   | Expr_bool   of bool
   | Expr_int    of int
   | Expr_id     of id
   | Expr_define of id * expr
   | Expr_if     of expr * expr * expr
   | Expr_lambda of id list * expr list
   | Expr_apply  of expr * expr list


let rec ast_of_sexpr sx = ast_of_expr sx

and ast_of_expr ex = match ex with
    | S.Expr_atom a -> ast_of_atom a
    | S.Expr_list (S.Expr_atom a :: tl) ->
      begin match a with
        | S.Atom_id ("define") -> ast_of_define tl
        | S.Atom_id ("if") -> ast_of_if tl
        | S.Atom_id ("lambda") -> ast_of_lambda tl
        | _ -> ast_of_apply (S.Expr_atom a :: tl)
      end
    | _ -> failwith "bad expression"

and ast_of_atom a = match a with
    | S.Atom_unit -> Expr_unit
    | S.Atom_bool b -> Expr_bool b
    | S.Atom_int i -> Expr_int i
    | S.Atom_id id -> Expr_id id

and  ast_of_define tl = match tl with
  | S.Expr_atom (S.Atom_id id) :: body :: [] ->
    Expr_define (id, ast_of_expr body)
  | _ -> failwith "bad define"

and ast_of_if tl = match tl with
  | e1 :: e2 :: e3 :: body ->
    let e1' = ast_of_expr e1 in
    let e2' = ast_of_expr e2 in
    let e3' = ast_of_expr e3 in
    Expr_if (e1', e2', e3')
  | _ -> failwith "bad if"

and ast_of_lambda tl =
  let id_of_expr a = match a with
    | S.Expr_atom (S.Atom_id id) -> id
    | _ -> failwith "invalid id"
  in match tl with
    | S.Expr_list l1 :: ex :: [] ->
      let ids = List.map id_of_expr l1 in
      let body = [ast_of_expr ex] in
      Expr_lambda (ids, body)
    | _ -> failwith "bad lambda"

and ast_of_apply tl = match tl with
  | fn :: args ->
    let fn' = ast_of_expr fn in
    let args' = List.map ast_of_expr args in
    Expr_apply (fn', args')
  | _ -> failwith "bad apply"


let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left (^) ""
             (List.map
                 (fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Expr_bool b  -> sprintf "%sBOOL[ %b ]" (spaces indent) b
         | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Expr_id   id -> sprintf "%sID[ %s ]"   (spaces indent) id
         | Expr_define (id, e) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
         | Expr_if (test_clause, then_clause, else_clause) ->
              sprintf "%sIF[\n%s\n%s\n%s ]"
                 (spaces indent) 
                 (iter test_clause (indent + 2))
                 (iter then_clause (indent + 2))
                 (iter else_clause (indent + 2))
         | Expr_lambda (ids, body) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Expr_apply (operator, operands) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
   in
      "\n" ^ iter ast 0 ^ "\n"


let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let sexpr  = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = ast_of_sexpr s in
                    Printf.printf "%s\n" (string_of_ast expr); 
                    flush stdout;
                    loop ()
   in
      loop ()


