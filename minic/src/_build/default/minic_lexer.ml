# 1 "minic_lexer.mll"
 
  open Lexing
  open Minic_parser

  (* Fonction auxiliaire pour rassembler les mots-clés 
     À COMPLÉTER
   *)
let keyword_or_ident =
let h = Hashtbl.create 17 in
List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "return",   RETURN;
    "true",     BOOL_CST true;
    "false",     BOOL_CST false;
    "int",      INT;
    "bool",     BOOL;
    "void",     VOID;
    "if",       IF;
    "else",     ELSE;
    "while",    WHILE;
    "putchar", PUTCHAR;
    "for",      FOR
    ];
fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)

(* Affiche la sequence de lexeme pour visualisation *)
  let print_token = function
    | CST n       -> Printf.printf "CST %i\n" n
    | IDENT s     -> Printf.printf "IDENT %s\n" s
    | INT         -> Printf.printf "INT\n"
    | BOOL         -> Printf.printf "BOOL\n"
    | VOID         -> Printf.printf "VOID\n"
    | BOOL_CST b  -> Printf.printf "BOOL_CST %b\n" b
    | RETURN      -> Printf.printf "RETURN\n"
    | WHILE       -> Printf.printf "WHILE\n"
    | FOR       -> Printf.printf "FOR\n"
    | IF       -> Printf.printf "IF\n"
    | ELSE       -> Printf.printf "ELSE\n"
    | SEMI        -> Printf.printf "SEMI\n"
    | SET         -> Printf.printf "SET\n"
    | LPAR        -> Printf.printf "LPAR\n"
    | RPAR        -> Printf.printf "RPAR\n"
    | BEGIN       -> Printf.printf "BEGIN\n"
    | END         -> Printf.printf "END\n"
    | PUTCHAR     -> Printf.printf "PUTCHAR \n"
  (* opérateurs arithemétiques *)
    | MUL   -> Printf.printf "MUL\n"
    | DIV   -> Printf.printf "DIV\n"
    | PLUS  -> Printf.printf "PLUS\n"
    | SUB   -> Printf.printf "SUB\n"
  (* opérateurs de comparaison *)
    | LT -> Printf.printf "LT\n"
    | GT -> Printf.printf "GT\n"
    | EQ -> Printf.printf "EQ\n"
    | NE -> Printf.printf "NE\n"
    | LE -> Printf.printf "PLUS\n"
    | GE -> Printf.printf "PLUS\n"
  (* opérateurs logiques (booléens) *)
    | NOT  -> Printf.printf "NOT\n"
    | ANDL -> Printf.printf "ANDL\n"
    | ORL  -> Printf.printf "ORL\n"
  (* opérateurs bit à bit *)
    | AND -> Printf.printf "AND\n"
    | OR  -> Printf.printf "OR"
    | XOR -> Printf.printf "XOR\n"
    | LSL -> Printf.printf "LSL\n"
    | ASR -> Printf.printf "ASR\n"
  (* sucres sytaxique *)
    | INCR -> Printf.printf "INCR\n"
    | DECR -> Printf.printf "DECR\n"
    | COMA -> Printf.printf "COMA\n"
  | _ -> Printf.printf "###\n"
        

# 78 "minic_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\223\255\224\255\229\255\002\000\001\000\002\000\030\000\
    \035\000\003\000\243\255\244\255\245\255\246\255\247\255\248\255\
    \003\000\250\255\251\255\079\000\154\000\167\000\002\000\255\255\
    \225\255\238\255\226\255\228\255\236\255\227\255\235\255\237\255\
    \233\255\232\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\025\000\024\000\021\000\016\000\
    \015\000\013\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \006\000\255\255\255\255\003\000\002\000\014\000\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\022\000\023\000\022\000\000\000\022\000\000\000\022\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \022\000\006\000\022\000\000\000\000\000\000\000\005\000\032\000\
    \015\000\014\000\011\000\009\000\017\000\021\000\026\000\010\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\018\000\008\000\016\000\007\000\031\000\
    \025\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\030\000\029\000\000\000\003\000\027\000\
    \028\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\013\000\004\000\012\000\033\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\000\000\000\000\000\000\000\000\019\000\000\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\024\000\000\000\000\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\022\000\255\255\000\000\255\255\022\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\022\000\255\255\255\255\255\255\000\000\005\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\006\000\
    \016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\007\000\007\000\255\255\000\000\008\000\
    \008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\004\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\255\255\255\255\255\255\255\255\019\000\255\255\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\021\000\255\255\255\255\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 88 "minic_lexer.mll"
      ( new_line lexbuf; token lexbuf )
# 229 "minic_lexer.ml"

  | 1 ->
# 90 "minic_lexer.mll"
      ( token lexbuf )
# 234 "minic_lexer.ml"

  | 2 ->
let
# 91 "minic_lexer.mll"
              n
# 240 "minic_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 92 "minic_lexer.mll"
      ( CST(int_of_string n) )
# 244 "minic_lexer.ml"

  | 3 ->
let
# 93 "minic_lexer.mll"
             id
# 250 "minic_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 94 "minic_lexer.mll"
      ( keyword_or_ident id )
# 254 "minic_lexer.ml"

  | 4 ->
# 96 "minic_lexer.mll"
      ( SEMI )
# 259 "minic_lexer.ml"

  | 5 ->
# 98 "minic_lexer.mll"
      ( COMA )
# 264 "minic_lexer.ml"

  | 6 ->
# 100 "minic_lexer.mll"
      ( SET )
# 269 "minic_lexer.ml"

  | 7 ->
# 102 "minic_lexer.mll"
      ( LPAR )
# 274 "minic_lexer.ml"

  | 8 ->
# 104 "minic_lexer.mll"
      ( RPAR )
# 279 "minic_lexer.ml"

  | 9 ->
# 106 "minic_lexer.mll"
      ( BEGIN )
# 284 "minic_lexer.ml"

  | 10 ->
# 108 "minic_lexer.mll"
      ( END )
# 289 "minic_lexer.ml"

  | 11 ->
# 111 "minic_lexer.mll"
      ( MUL )
# 294 "minic_lexer.ml"

  | 12 ->
# 113 "minic_lexer.mll"
      ( DIV )
# 299 "minic_lexer.ml"

  | 13 ->
# 115 "minic_lexer.mll"
      ( PLUS )
# 304 "minic_lexer.ml"

  | 14 ->
# 118 "minic_lexer.mll"
      ( SUB )
# 309 "minic_lexer.ml"

  | 15 ->
# 121 "minic_lexer.mll"
      ( LT )
# 314 "minic_lexer.ml"

  | 16 ->
# 123 "minic_lexer.mll"
      ( GT )
# 319 "minic_lexer.ml"

  | 17 ->
# 125 "minic_lexer.mll"
      ( EQ )
# 324 "minic_lexer.ml"

  | 18 ->
# 127 "minic_lexer.mll"
      ( NE )
# 329 "minic_lexer.ml"

  | 19 ->
# 129 "minic_lexer.mll"
      ( LE )
# 334 "minic_lexer.ml"

  | 20 ->
# 131 "minic_lexer.mll"
      ( GE )
# 339 "minic_lexer.ml"

  | 21 ->
# 134 "minic_lexer.mll"
      ( NOT )
# 344 "minic_lexer.ml"

  | 22 ->
# 136 "minic_lexer.mll"
      ( ANDL )
# 349 "minic_lexer.ml"

  | 23 ->
# 138 "minic_lexer.mll"
      ( ORL )
# 354 "minic_lexer.ml"

  | 24 ->
# 141 "minic_lexer.mll"
      ( AND )
# 359 "minic_lexer.ml"

  | 25 ->
# 143 "minic_lexer.mll"
      ( OR )
# 364 "minic_lexer.ml"

  | 26 ->
# 145 "minic_lexer.mll"
      ( XOR )
# 369 "minic_lexer.ml"

  | 27 ->
# 147 "minic_lexer.mll"
      ( LSL )
# 374 "minic_lexer.ml"

  | 28 ->
# 149 "minic_lexer.mll"
      ( ASR )
# 379 "minic_lexer.ml"

  | 29 ->
# 152 "minic_lexer.mll"
      ( INCR )
# 384 "minic_lexer.ml"

  | 30 ->
# 154 "minic_lexer.mll"
      ( DECR )
# 389 "minic_lexer.ml"

  | 31 ->
# 157 "minic_lexer.mll"
      ( failwith ("Unknown character : " ^ (lexeme lexbuf)) )
# 394 "minic_lexer.ml"

  | 32 ->
# 159 "minic_lexer.mll"
      ( EOF )
# 399 "minic_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

# 161 "minic_lexer.mll"
 
  let lexbuf = Lexing.from_channel(open_in Sys.argv.(1))
        
  let rec loop () =
    let t = token lexbuf in
    if t <> EOF
    then begin print_token t; loop () end
      
  let _ =
    loop ()

# 418 "minic_lexer.ml"
