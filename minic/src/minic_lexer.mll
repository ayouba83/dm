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
    "for",      FOR;
    "sizeof",       LEN
    ];
fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)

}

(* Règles auxiliaires *)
let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*
let ptr_int = ['i']['n']['t'][' ' '\t' '\r']*['*']
let ptr_bool = ['b']['o']['o']['l'][' ' '\t' '\r']*['*']

(* Règles de reconnaissance 
   À COMPLÉTER
*)
rule token = parse
  | "/*" 
      { comment lexbuf }
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | number as n
      { CST(int_of_string n) }
  | ident as id
      { keyword_or_ident id }
  | ptr_bool
        { PTRB }
  | ptr_int
        {PTRI} 
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
  | "]"
      { RBRK }
  | "["
      { LBRK }
  | "{"
      { BEGIN }
  | "}"
      { END }
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
  
  (* sucres sytaxique *)
  | "++"
      { INCR }
  | "--"
      { DECR }
                                                                                                              
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }
 and comment = parse
  | "*/" 
      { token lexbuf }
  | _ 
      { comment lexbuf }
  | eof 
      { failwith "commentaire non termin´e" }
{
  
}
