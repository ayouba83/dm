open Minic_ast

let print (programme: prog) =
  let level = ref 1 in
  let decal n =
    let x = ref n in
    while !x <> 0 do
      Printf.printf "    ";
      x:= !x - 1;
    done;
  in
  (* affiche un type *)
  let rec typ_to_str = function
    | Int  -> "int" 
    | Bool -> "bool"
    | Void -> "void"
    | Tab(t) -> (typ_to_str t)
    | Ptr(t) -> (typ_to_str t)^"*"
  in

  (* affiche une expression *)
  let rec affiche_expr = function
  | Cst n -> Printf.printf "%n" n
  | BCst b -> Printf.printf "%b" b
  | Array (a) -> Printf.printf "{"; affiche_params_eff (Array.to_list a); Printf.printf "}"
  | Get var -> Printf.printf "%s" var
  | Par e -> Printf.printf "("; affiche_expr e; Printf.printf ")"
  | Call(f, params) -> begin
                          Printf.printf "%s(" f;
                          affiche_params_eff params;
                          Printf.printf ")";
                        end
 (* opérateurs arithemétiques *)
  | Add(e1, e2) -> affiche_expr e1; Printf.printf " + "; affiche_expr e2
  | Sub(e1, e2) -> affiche_expr e1; Printf.printf " - "; affiche_expr e2
  | Mul(e1, e2) -> affiche_expr e1; Printf.printf " * "; affiche_expr e2
  | Div(e1, e2) -> affiche_expr e1; Printf.printf " / "; affiche_expr e2
  | Opp(e) ->  Printf.printf "-"; affiche_expr e
 (* opérateurs de comparaison *)
  | Lt(e1, e2) -> affiche_expr e1; Printf.printf " < "; affiche_expr e2
  | Gt(e1, e2) -> affiche_expr e1; Printf.printf " > "; affiche_expr e2
  | Eq(e1, e2) -> affiche_expr e1; Printf.printf " == "; affiche_expr e2
  | Ne(e1, e2) -> affiche_expr e1; Printf.printf " != "; affiche_expr e2
  | Le(e1, e2) -> affiche_expr e1; Printf.printf " <= "; affiche_expr e2
  | Ge(e1, e2) -> affiche_expr e1; Printf.printf " >= "; affiche_expr e2
 (* opérateurs logiques (booléens) *)
  | Not(e) -> Printf.printf "!"; affiche_expr e
  | Andl(e1, e2) -> affiche_expr e1; Printf.printf " && "; affiche_expr e2
  | Orl(e1, e2) -> affiche_expr e1; Printf.printf " || "; affiche_expr e2
 (* sucres sytaxiques *)
  | Incr(x) -> Printf.printf "%s++" x
  | Decr(x) -> Printf.printf "%s--" x
  | Elm(pos, tab) -> Printf.printf "%s" tab; Printf.printf "["; affiche_expr pos; Printf.printf "]";
  | Len(tab) -> Printf.printf "sizeof(%s)" tab;
and affiche_params_eff = function
  |[] -> Printf.printf ""
  |[e] -> affiche_expr e
  |e::fin -> affiche_expr e; Printf.printf ", "; affiche_params_eff fin
in
  
  (*affichage des variables globales du programme*)
  
  let affiche_var_init (x, t, v) =
    begin match t with
    |Int |Bool |Void |Ptr(_) -> Printf.printf "%s %s" (typ_to_str t) x; Printf.printf " = "; affiche_expr v;
    |Tab(_) -> Printf.printf "%s %s" (typ_to_str t) x; Printf.printf "[]"; Printf.printf " = "; affiche_expr v;
    end
  in 
  let affiche_var_not_init (x, t) =
    begin match t with
    |Int |Bool |Void |Ptr(_) -> Printf.printf "%s %s" (typ_to_str t) x;
    |Tab(_) -> Printf.printf "%s %s" (typ_to_str t) x; Printf.printf "[]";
    end
  in
   let affiche_var (x, t, v) =
    begin match v with
    | None -> affiche_var_not_init (x, t)
    | Some(va) -> affiche_var_init (x, t, va)
      end
  in
  List.iter (fun x -> Printf.printf " "; affiche_var x; Printf.printf ";\n") programme.globals;

  (* affichage des fonctions du programme *)
  let affiche_fun (def: fun_def) =
    (*afficher <type> <nom> (<paramètres>) { *)
    Printf.printf " %s %s(" (typ_to_str def.return) def.name;
    let rec affiche_params (l: (string * typ) list) =
      match l with
      |[] -> Printf.printf ""
      |[(x, t)] -> affiche_var (x, t, None)
      |(x,t)::fin -> affiche_var (x, t, None); Printf.printf ", "; affiche_params fin
    in
    affiche_params def.params;
    Printf.printf "){\n";

    (* afficher les variables locales *)
    List.iter (fun x -> decal !level; affiche_var x; Printf.printf ";\n") def.locals;

    let affiche_instr_form = function
    | Insert(pos, e, tab) -> Printf.printf "%s" tab; Printf.printf "["; affiche_expr pos;
                             Printf.printf "]" ; Printf.printf "= "; affiche_expr e;
    | Putchar(e) ->  Printf.printf "putchar("; affiche_expr e; Printf.printf ")"
    | Set(var, e) -> Printf.printf "%s = " var; affiche_expr e
    | Expr e -> affiche_expr e
    |_-> failwith "not_instr_form"
    in

    let rec affiche_instr = function
    | If(cond, s1, s2) -> begin decal !level;
                        Printf.printf "if("; affiche_expr cond; Printf.printf"){\n";
                        level := !level+1; affiche_seq s1; level := !level-1;
                        decal !level; Printf.printf "} else{\n"; level := !level+1; affiche_seq s2; level := !level-1;
                        decal !level; Printf.printf "}\n"
                      end
    | While(cond, s) -> begin decal !level;
                        Printf.printf "while("; affiche_expr cond; Printf.printf"){\n";
                        level := !level+1; affiche_seq s; level := !level-1;
                        decal !level; Printf.printf "}\n"
                      end
    | For(init, test, iter, s) -> begin decal !level;
                      Printf.printf "for("; affiche_instr_form init; Printf.printf "; ";
                      affiche_expr test; Printf.printf "; "; affiche_instr_form iter; 
                      Printf.printf"){\n";
                      level := !level+1; affiche_seq s; level := !level-1;
                      decal !level; Printf.printf "}\n"
                    end
    | Return e -> decal !level; Printf.printf "return "; affiche_expr e; Printf.printf ";\n"
    | instr -> decal !level; affiche_instr_form instr; Printf.printf ";\n"
    and affiche_seq s =
        List.iter affiche_instr s;
    in
    (* affichage du code de la fonction *)
    affiche_seq def.code;
    Printf.printf " }\n"; (* fermer le bloc contenant le code de la fonction *)
    in
    
    (* affichage des fonctions du programme *)
    List.iter affiche_fun programme.functions