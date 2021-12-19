(* Représentation des types. *)
type typ =
  | Int
  | Bool
  | Void

(* Représentation des expressions.
   Ajouté : les constantes booléennes. *)
type expr =
  | Cst of int
  | BCst of bool
  | Get of string
  | Call of string * expr list
  | Par of expr
 (* opérateurs arithemétiques *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Opp of expr
 (* opérateurs de comparaison *)
  | Lt  of expr * expr
  | Gt  of expr * expr
  | Eq  of expr * expr
  | Ne  of expr * expr
  | Le  of expr * expr
  | Ge  of expr * expr
 (* opérateurs logiques (booléens) *)
  | Not of expr
  | Andl of expr * expr
  | Orl of expr * expr
 (* sucres sytaxiques *)
  | Incr of string
  | Decr of string

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr
  | Set of string * expr      (*let x = 4* in *)
  | If  of expr * seq * seq
  | While of expr * seq
  | For of instr*expr * instr*seq
  | Return of expr
  | Expr of expr
and seq = instr list

(* Représentétion des fonctions. *)
type fun_def = {
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code: seq;
}

(* Représentation des programmes.
   En réponse à l'indication de l'énoncé, j'associe une valeur entière
   à chaque variable globale. Mais vous voudrez peut-être faire évoluer
   cela (et procéder de même pour les variables locales des fonctions). *)
type prog = {
  globals: (string * typ * int) list;
  functions: fun_def list;
}
