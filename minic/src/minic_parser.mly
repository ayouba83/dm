%{

  open Lexing
  open Minic_ast

%}

(** Déclaration des lexèmes **)

(* valeurs constantes *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT

(* delimitants *)
%token LPAR RPAR BEGIN END RBRK LBRK PTRB PTRI

(* mots clés, affectation, fin d'instruction *)
%token RETURN IF ELSE WHILE SET SEMI PUTCHAR COMA FOR LEN

(* types *)
%token INT BOOL VOID
%token EOF

(* opérateurs arithemétiques *)
%token MUL DIV PLUS SUB

(* opérateurs de comparaison *)
%token LT GT EQ NE LE GE

(* opérateurs logiques (booléens) *)
%token NOT ANDL ORL

(* opérateurs bit à bit *)
%token AND OR XOR

(* sucres sytaxiques *)
%token INCR DECR

%start program
%type <Minic_ast.prog> program

%left ORL
%left ANDL
%left OR
%left XOR
%left AND
%left EQ NE
%left LT GT LE GE
%left PLUS SUB
%left MUL DIV
%nonassoc NOT
%nonassoc UMINUS
%%

(* Un programme est une liste de déclarations.
   On ajoute une règle déclenchée en cas d'erreur, donnant une
   information minimale : la position. *)
program:
| dl=declaration_list EOF
       { let var_list, fun_list = dl in
         { globals = var_list; functions = fun_list; } }
| error { let pos = $startpos in
          let message =
            Printf.sprintf
              "Syntax error at %d, %d"
              pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
          in
          failwith message }
;

(* Chaque déclaration peut concerner une variable ou une fonction. *)
declaration_list:
| (* vide *) { [], [] }    
| vd=variable_decl dl=declaration_list { let vl, fl = dl in
                                         vd :: vl, fl }
| fd=function_decl dl=declaration_list { let vl, fl = dl in
                                         vl, fd :: fl }
;

(* Déclaration de variable*)

variable_decl:
| var_dcl=variable_decl_form SEMI { var_dcl }
| variable_decl_form { let pos = $startpos in
                        let message =
                           Printf.sprintf
                           "Missing \";\" after instruction at ( %d : %d )"
                           pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
                        in
                        failwith message }
;
variable_decl_form:
| typ=typ var=IDENT SET init=expression { (var, typ, Some(init)) }  (*  Int a = 1+2;  *)
| typ=typ tab=IDENT LBRK RBRK SET BEGIN s=args_list END { let init = Array.of_list s in (tab, Tab(typ), (Some (Array(init)))) }   (*  Int tab[] = {a+3, fact(a-1), 0};  *)
| typ=typ var=IDENT { (var, typ, None) }  (*Int a;*)
;
(* declaration de paramètres de fonction *)
param_list:
| (* empty *) { [] }
| p=param_dcl { [p] } 
| vd=param_dcl COMA dl=param_list { vd :: dl }
;
param_dcl:
|t=typ x=IDENT { (x, t) }   (*  Int a  *)
|t=typ x=IDENT LBRK RBRK { (x, Tab(t)) }    (*  Int t[]*)
;

(* Indication de type. *)
typ:
| INT { Int }
| BOOL { Bool }
| VOID { Void }
| PTRB { Ptr(Bool) }
| PTRI { Ptr(Int) }
;

(* Déclaration de fonction.
   Note : on ne traite ici que le cas d'une fonction sans argument et
   sans variable locale.
*)
function_decl:
| t=typ f=IDENT LPAR opt=param_list RPAR BEGIN vars=list(variable_decl) s=list(instruction) END
    { { name=f; params=opt; return=t; locals=vars; code=s } }
;

(* Instructions. *)
instruction:
| instr=instr_form SEMI { instr }
| instr=bloc_instr_form END { instr }
| instr_form { let pos = $startpos in
               let message =
                  Printf.sprintf
                  "Missing \";\" after instruction at ( %d : %d )"
                  pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
               in
               failwith message }
;
instr_form:
| tab=IDENT LBRK pos=expression RBRK SET e=expression { Insert(pos, e, tab) } (*  tab[a+2] = 7 *)
| PUTCHAR LPAR e=expression RPAR { Putchar(e) }  (*  putchar(3/4)  *)
| id=IDENT SET e=expression { Set(id, e) }  (*  a = 2+3  *)
| e=expr_form { Expr(e) }
| RETURN e=expression { Return(e) }   (*  return a+b;  *)
;
bloc_instr_form:
| IF LPAR e=expression RPAR BEGIN s1=list(instruction) END ELSE BEGIN s2=list(instruction)  { If(e, s1, s2) }  (*  if(b == true){plein d'instr}else{plein d'instr}  *)
| WHILE LPAR e=expression RPAR BEGIN s=list(instruction) { While(e, s) }   (*  while(i < 7){plein d'instr}  *)
(*  for(i = 0; a < 4; i = i+1) {plein d'instr}  *)
| FOR LPAR init=instr_form SEMI test=expression SEMI iter=instr_form RPAR BEGIN s=list(instruction) { For(init, test, iter, s) } 
;

(* Expressions.*)
expression:
| n=CST { Cst(n) }       (* 4 *)
| b=BOOL_CST { BCst(b) }    (* true ou false *)
| BEGIN l=args_list END { let arr = Array.of_list l in Array(arr) } (* {1, 2, 3, 4} *)
| a=expression MUL  b=expression { Mul(a, b) }    (*  a*b  *)
| a=expression PLUS b=expression { Add(a, b) }    (*  a+b  *)
| a=expression DIV  b=expression { Div(a, b) }    (*  a/b  *)
| a=expression SUB  b=expression { Sub(a, b) }    (*  a-b  *)
| SUB a=expression %prec UMINUS { Opp(a) }        (*  -a   *)
| a=expression LT   b=expression { Lt(a, b) }     (*  a<b  *)
| a=expression GT   b=expression { Gt(a, b) }     (*  a>b  *)
| a=expression EQ   b=expression { Eq(a, b) }     (*  a==b *)
| a=expression NE   b=expression { Ne(a, b) }     (*  a!=b *)
| a=expression LE   b=expression { Le(a, b) }     (*  a<=b *)
| a=expression GE   b=expression { Ge(a, b) }     (*  a>=b *)
| NOT  b=expression { Not(b) }                    (*  !b   *)
| a=expression ANDL b=expression { Andl(a, b) }   (*  a&&b *)
| a=expression ORL  b=expression { Orl(a, b) }    (*  a||b *)
| a=expression AND b=expression  { And(a, b) }    (*  a&b *)
| a=expression OR  b=expression  { Or(a, b) }     (*  a|b *)
| a=expression XOR  b=expression { Xor(a, b) }     (*  a xor b *)
| e=expr_form { e }
| n=IDENT   { Get(n) }                            (*   id  *)
| LPAR e=expression RPAR { Par(e) }               (*  (a*b)  *)
| tab=IDENT LBRK n=expression RBRK { Elm(n, tab) }  (*  tab[modulo(i, j)]  *)
| LEN LPAR tab=IDENT RPAR    { Len(tab) }           (*  sizeof(tab)  *)
;
(* form d'expression expression qui peuvent etre aussi utilisés comme des instructions: fonction(param); ou i++; ou i--;*)
expr_form:
| id=IDENT INCR { Incr( id ) }                    (*  id++ *)
| id=IDENT DECR { Decr( id ) }                    (*  id-- *)
| f=IDENT LPAR params=args_list RPAR { Call(f, params) }  (*  f(a+b, c-d, {2, 6})  *)
;
(*declaration de paramètres effectifs de fonction*)
args_list:
| (* empty *) { [] }
| e=expression { [e] }   
| vd=expression COMA dl=args_list { vd :: dl }
;
