%{

  open Lexing
  open Minic_ast

%}

(* Déclaration des lexèmes *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT
%token LPAR RPAR BEGIN END
%token RETURN SET SEMI
%token INT
%token EOF

%start program
%type <Minic_ast.prog> program

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

   À COMPLÉTER
*)
variable_decl:
| t=typ x=IDENT SET n=CST SEMI { (x, t, n) }
;

(* Indication de type.

   À COMPLÉTER
*)
typ:
| INT { Int }
;

(* Déclaration de fonction.
   Note : on ne traite ici que le cas d'une fonction sans argument et
   sans variable locale.

   À COMPLÉTER
*)
function_decl:
| t=typ f=IDENT LPAR RPAR BEGIN s=list(instruction) END
    { { name=f; code=s; params=[]; return=t; locals=[] } }
;

(* Instructions.

   À COMPLÉTER
*)
instruction:
| RETURN e=expression SEMI { Return(e) }
;

(* Expressions.

   À COMPLÉTER
*)
expression:
| n=CST { Cst(n) }
| b=BOOL_CST { BCst(b) }
;
