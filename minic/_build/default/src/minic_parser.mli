
(* The type of tokens. *)

type token = 
  | SET
  | SEMI
  | RPAR
  | RETURN
  | LPAR
  | INT
  | IDENT of (string)
  | EOF
  | END
  | CST of (int)
  | BOOL_CST of (bool)
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Minic_ast.prog)
