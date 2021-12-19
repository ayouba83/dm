%{

  open Lexing
  open Minic_ast

%}

(** Déclaration des lexèmes **)

(* valeurs constantes *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT
%token <typ_elem array> TAB

(* delimitants *)
%token LPAR RPAR BEGIN END RBRK LBRK

(* mots clés, affectation, fin d'instruction *)
%token RETURN IF ELSE WHILE SET SEMI PUTCHAR COMA FOR

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
%token AND OR XOR LSL ASR

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
%nonassoc INCR DECR
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

(* Déclaration de variable.
   Note : on ne traite ici que le cas où une valeur initiale est fournie.

   À COMPLÉTER => fait
*)
variable_decl:
| t=typ x=IDENT SET n=CST SEMI { (x, t, n) }
| t=typ x=IDENT SET b=BOOL_CST SEMI { (x, t, b) }
| t=typ x=IDENT LBRK RBRK SET BEGIN s=seq END { (x, t, a) }
(*| t=typ x=IDENT SEMI { (x, t) }*)
;

(* declaration de variables locales *)
locale_dcl:
|t=typ x=IDENT SEMI { (x, t) }
;
(* declaration de paramètres de fonction *)
param_list:
| (* empty *) { [] }
| p=param_dcl { [p] } (* à corriger*)
| vd=param_dcl COMA dl=param_list { vd :: dl }
;
param_dcl:
|t=typ x=IDENT { (x, t) }
;

(* Indication de type.

   À COMPLÉTER => fait
*)
typ_elem:
| INT { Int }
| BOOL { Bool }
| TAB { Tab }
;

typ:
| VOID { Void }
| t=typ_elem { t }
;

(* Déclaration de fonction.
   Note : on ne traite ici que le cas d'une fonction sans argument et
   sans variable locale.

   À COMPLÉTER => fait
*)
function_decl:
| t=typ f=IDENT LPAR opt=param_list RPAR BEGIN vars=list(locale_dcl) s=list(instruction) END
    { { name=f; params=opt; return=t; locals=vars; code=s } }
;
(* Instructions.

   À COMPLÉTER => fait
*)
instruction:
| instr=instr_form SEMI { instr }
| IF LPAR e=expression RPAR BEGIN s1=list(instruction) END ELSE BEGIN s2=list(instruction) END { If(e, s1, s2) }
| WHILE LPAR e=expression RPAR BEGIN s=list(instruction) END { While(e, s) }
| FOR LPAR init=instr_form SEMI test=expression SEMI iter=instr_form RPAR BEGIN s=list(instruction) END { For(init, test, iter, s) }
| RETURN e=expression SEMI { Return(e) }
;

instr_form:
| PUTCHAR LPAR e=expression RPAR { Putchar(e) }
| id=IDENT SET e=expression { Set(id, e) }
| e=expression { Expr(e) }
;

(* Expressions.

   À COMPLÉTER => fait
*)
expression:
| n=CST { Cst(n) }
| b=BOOL_CST { BCst(b) }
| a=expression MUL  b=expression { Mul(a, b) }
| a=expression PLUS b=expression { Add(a, b) }
| a=expression DIV  b=expression { Div(a, b) } (* à modifier pour traiter le cas de division par zero *)
| a=expression SUB  b=expression { Sub(a, b) }
| SUB a=expression %prec UMINUS { Opp(a) }
| a=expression LT   b=expression { Lt(a, b) }
| a=expression GT   b=expression { Gt(a, b) }
| a=expression EQ   b=expression { Eq(a, b) }
| a=expression NE   b=expression { Ne(a, b) }
| a=expression LE   b=expression { Le(a, b) }
| a=expression GE   b=expression { Ge(a, b) }
| NOT  b=expression { Not(b) }
| a=expression ANDL b=expression{ Andl(a, b) }
| a=expression ORL  b=expression { Orl(a, b) }
| id=IDENT INCR { Incr( id ) }
| id=IDENT DECR { Decr( id ) }
| n=IDENT   { Get(n) }
| f=IDENT LPAR params=args_list RPAR { Call(f, params) }
| LPAR e=expression RPAR { Par(e) }
;

(*declaration de paramètres effectifs de fonction*)
args_list:
| (* empty *) { [] }
| e=expression { [e] }   
| vd=expression COMA dl=args_list { vd :: dl }
;
