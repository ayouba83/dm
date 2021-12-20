
(* The type of tokens. *)

type token = 
  | XOR
  | WHILE
  | VOID
  | SUB
  | SET
  | SEMI
  | RPAR
  | RETURN
  | PUTCHAR
  | PLUS
  | ORL
  | OR
  | NOT
  | NE
  | MUL
  | LT
  | LSL
  | LPAR
  | LE
  | INT
  | INCR
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | DECR
  | CST of (int)
  | COMA
  | BOOL_CST of (bool)
  | BOOL
  | BEGIN
  | ASR
  | ANDL
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Minic_ast.prog)
