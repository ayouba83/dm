{
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
    | RBRK        -> Printf.printf "RBRACKET \n"
    | LBRK        -> Printf.printf "LBRACKET \n"
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
        
}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*

(* Règles de reconnaissance 
   À COMPLÉTER
*)
rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | ident as id
      { keyword_or_ident id }
  | ";"
      { SEMI }
  | ","
      { COMA }
  | "="
      { SET }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | "{"
      { BEGIN }
  | "}"
      { END }
  | "]"
      { RBRK }
  | "["
      { LBRK }
(* opérateurs arithemétiques *)
  | "*"
      { MUL }
  | "/"
      { DIV }
  | "+"
      { PLUS }
(* cas de la soustrion à traiter *)
  | "-"
      { SUB }
(* opérateurs de comparaison *)
  | "<"
      { LT }
  | ">"
      { GT }
  | "=="
      { EQ }
  | "!="
      { NE }
  | "<="
      { LE } 
  | ">="
      { GE }
  (* opérateurs logiques (booléens) *)
  | "!"
      { NOT }
  | "&&"
      { ANDL }
  | "||"
      { ORL } 
  (* opérateurs bit à bit *)
  | "&"
      { AND }
  | "|"
      { OR }
  | "^"
      { XOR }
  | "<<"
      { LSL }
  | ">>"
      { ASR }
  (* sucres sytaxique *)
  | "++"
      { INCR }
  | "--"
      { DECR }
                                                                                                              
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

{
  let lexbuf = Lexing.from_channel(open_in Sys.argv.(1))
        
  let rec loop () =
    let t = token lexbuf in
    if t <> EOF
    then begin print_token t; loop () end
      
  let _ =
    loop ()
}
