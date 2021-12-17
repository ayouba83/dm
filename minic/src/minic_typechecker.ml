open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty prog.globals
  in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    
    (* On devrait ici avoir également un environnement local.
       À COMPLÉTER
     *)
    let local_env =
      List.fold_left (fun env (x, ty) -> Env.add x ty env) Env.empty fdef.locals
    in
    
    let param_env = 
      List.fold_left (fun env (x, ty) -> Env.add x ty env) Env.empty fdef.params
    in
    
    (* Vérification du bon typage et calcul du type d'une expression.
       À nouveau, fonction locale avec accès à tout ce qui est au-dessus. *)
    let rec type_expr = function
      | Cst _ -> Int      (* 4 + 5*)
      | BCst _ -> Bool    (*True | False*)
      | Get x -> begin
          try Env.find x local_env   (*4 + x*)
          with Not_found ->
               try Env.find x param_env
               with Not_found -> 
               	try Env.find x global_env
               	with Not_found -> failwith "Variable not found" 
       end
      
      (*Opérateurs arithmétiques*)
      | Add(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Sub(e1, e2) ->
          let t1 = type_expr e1 in
          let t2 = type_expr e2 in
          if t1 = Int && t2 = Int
            then Int
            else failwith "type error"
      | Opp(e) ->
          if type_expr e = Int
            then Int
            else failwith "type error"
      
      (*Opérateurs de comparaison*)
      | Lt(e1, e2) | Le(e1, e2) | Gt(e1, e2) | Ge(e1, e2) ->
          let t1 = type_expr e1 in
          let t2 = type_expr e2 in
          if t1 = Int && t2 = Int
            then Bool
            else failwith "type error"
      | Eq(e1, e2) | Ne(e1, e2) ->
          let t1 = type_expr e1 in
          let t2 = type_expr e2 in
          if (t1 = Int && t2 = Int) || (t1 = Bool && t2 = Bool)
            then Bool
            else failwith "type error"
      
      (* opérateurs logiques (booléens) *)
      | Andl(e1, e2) | Orl(e1, e2) ->
          let t1 = type_expr e1 in
          let t2 = type_expr e2 in
          if t1 = Bool && t2 = Bool
            then Bool
            else failwith "type error"
      | Not(e) ->
          if type_expr e = Bool
            then Bool
            else failwith "type error"
            
      (* sucres sytaxiques *)
      | Incr(e) | Decr(e) ->
          let val_e = Get e in
          if type_expr val_e = Int
            then Int
            else failwith "type error"
      
    in

    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      (* Cas d'une instruction [return]. On vérifie que le type correspond au
         type de retour attendu par la fonction dans laquelle on se trouve. *)
      | Return(e) -> let t = type_expr e in
                     if t <> fdef.return then
                       failwith "type error"
      (* À COMPLÉTER *)
      (*****************************************************************************)
      | If(test, s1, s2) ->
          if type_expr test = Int
          then 
          
            let type_seq seq =
              match seq with
              | [] -> Void
              | _ -> begin 
                       let typecheck_seq = List.map typecheck_instr seq in
                       let len = List.length typecheck_seq in
                       let last = List.nth typecheck_seq (len-1) in
                       last
                     end
            in
          
            let t1 = type_seq s1 in
            let t2 = type_seq s2 in
            
            if t1 = t2
              then t1
              else failwith "type error"
          
          else
            failwith "type error"
       (******************************************************************************)
                   
    and typecheck_seq s =
      List.iter typecheck_instr s        
    in

    (* Code principal du typage d'une fonction : on type ses instructions. *)
    typecheck_seq (fdef.code);
  in

  (* Code principal du typage d'un programme : on type ses fonctions.
     Il faudrait aussi vérifier les valeurs initiales des variables globales.
     À COMPLÉTER
   *)
  List.iter typecheck_function (prog.functions);
