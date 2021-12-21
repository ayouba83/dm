open Minic_ast
(* Pour représenter les environnements associant chaque variable à son type. *)
module Env = Map.Make(String)

(* Vérification du bon typage d'un programme. *)
let typecheck_program (prog: prog) =
  (* L'environnement global mémorise le type de chaque variable globale. *)
  let global_env =
    List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty prog.globals
  in
  
  let line = ref 0 in

  let rec type_to_string = function
      | Int -> "Int"
      | Bool -> "Bool"
      | Void -> "Void"
      | Tab(t) -> (type_to_string t)^"[]"
      | Ptr(t) -> (type_to_string t)^"*"
  in
    
  let type_error styp1 styp2 desc =
    failwith (Printf.sprintf "Type error in %s : line %i; %s was given but %s was expected" desc !line styp1 styp2)
  in
    
  (* Vérification du bon typage et calcul du type d'une expression.
     À nouveau, fonction locale avec accès à tout ce qui est au-dessus. *)
  let rec type_expr fdef local_env param_env = function
    | Cst _ -> Int      (* 4 + 5*)
    | BCst _ -> Bool    (*True | False*)
    | Array (earr) -> 
      begin 
        match earr with
        | [||] -> Tab(Void)
        | _ -> let t = type_expr fdef local_env param_env earr.(0) in 
               if (Array.for_all (fun x -> (type_expr fdef local_env param_env x) = t) earr)
               then Tab(t) 
               else failwith (Printf.sprintf "at least one element of the array is not of type %s" (type_to_string t))
      end
    | Get x -> 
      begin  
        try Env.find x local_env   (*4 + x*)
        with Not_found ->
          try Env.find x param_env
          with Not_found -> 
            try Env.find x global_env
            with Not_found -> failwith (Printf.sprintf "in function %s, line %i; variable \"%s\" not found" fdef.name !line x) 
      end
    | Call(f, params) -> 
      begin 
        let params_eff_types = List.map (type_expr fdef local_env param_env) params in
        let prop (def: fun_def) =
          let params_types = snd (List.split def.params) in
          let b = 
            try List.for_all2 (fun x y -> x = y) params_types params_eff_types
            with Invalid_argument _ -> false 
          in
          (def.name = f) && b 
        in
        let val_f = 
          try List.find prop prog.functions
          with Not_found -> failwith (Printf.sprintf "call error on function %s" f) 
        in
        val_f.return
      end
    | Par(e) -> type_expr fdef local_env param_env e
    (*Opérateurs arithmétiques*)
    | Add(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Sub(e1, e2) ->
      let t1 = type_expr fdef local_env param_env e1 in
      let t2 = type_expr fdef local_env param_env e2 in
      if t1 = Int 
      then 
        if t2 = Int
        then Int
        else type_error (type_to_string t2) "Int" "arithmetic operation"
      else type_error (type_to_string t1) "Int" "arithmetic operation"
    | Opp(e) ->
      let t = type_expr fdef local_env param_env e in
      if t = Int
      then Int
      else type_error (type_to_string t) "Int" "negative"
      
    (*Opérateurs de comparaison*)
    | Lt(e1, e2) | Le(e1, e2) | Gt(e1, e2) | Ge(e1, e2) ->
      let t1 = type_expr fdef local_env param_env e1 in
      let t2 = type_expr fdef local_env param_env e2 in
      if t1 = Int 
      then 
        if t2 = Int
        then Bool
        else type_error (type_to_string t2) "Int" "comparison"
      else type_error (type_to_string t1) "Int" "comparison"
    | Eq(e1, e2) | Ne(e1, e2) ->
      let t1 = type_expr fdef local_env param_env e1 in
      let t2 = type_expr fdef local_env param_env e2 in
      if t1 = Int || t1 = Bool
      then 
        if t2 = t1
        then Bool
        else type_error (type_to_string t2) (type_to_string t1) "equality"
      else type_error (type_to_string t1) "{Bool or Int}" "equality"
      
    (* opérateurs logiques (booléens) *)
    | Andl(e1, e2) | Orl(e1, e2) ->
      let t1 = type_expr fdef local_env param_env e1 in
      let t2 = type_expr fdef local_env param_env e2 in
      if t1 = Bool 
      then 
        if t2 = Bool
        then Bool
        else type_error (type_to_string t2) "Bool" "comparison"
      else type_error (type_to_string t1) "Bool" "comparison"
    | Not(e) ->
      let t = type_expr fdef local_env param_env e in
      if t = Bool
      then Bool
      else type_error (type_to_string t) "Bool" "not"
            
    (* sucres sytaxiques *)
    | Incr(e) | Decr(e) ->
      let val_e = Get e in
      let t = type_expr fdef local_env param_env val_e in
      if t = Int
      then Int
      else type_error (type_to_string t) "Int" "Incrementation"
    in

  (* Vérification du bon typage d'une fonction.
     C'est une fonction locale : on a accès à [prog] et à [global_env]. *)
  let typecheck_function (fdef: fun_def) =
    (* On devrait ici avoir également un environnement local.
       À COMPLÉTER
    *)
    line := !line +1;
    let f = { name="P"; params=[]; return=Void; locals=[]; code=[] } in
    let e1 = Env.empty in
    let pred (_, t, v) =
      match v with
      | None -> ()
      | Some va ->
        let tva = type_expr f e1 e1 va in
        if (tva = t) || (tva = Void) 
        then ()
        else type_error (type_to_string tva) (type_to_string t) "local variable declaration"
    in
    
    List.iter (fun x -> line := !line +1; pred x ) fdef.locals;
    
    let local_env =
      List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty fdef.locals
    in
  
    let param_env = 
      List.fold_left (fun env (x, ty) -> Env.add x ty env) Env.empty fdef.params
    in
    
    (* Vérification du bon typage d'une instruction ou d'une séquence.
       Toujours local. *)
    let rec typecheck_instr = function
      (* Cas d'une instruction [return]. On vérifie que le type correspond au
         type de retour attendu par la fonction dans laquelle on se trouve. *)
      | Return(e) -> 
        begin 
          let t = type_expr fdef local_env param_env e in
          match t with
          | Tab(typ) -> 
            if Ptr(typ) <> fdef.return 
            then failwith (Printf.sprintf "The declared return value of the function is %s but an array was returned at line %i" (type_to_string fdef.return) !line)
          | _ -> 
            if t <> fdef.return 
            then type_error (type_to_string t) (type_to_string fdef.return) "return"
        end

      | If(test, s1, s2) ->
        begin 
          let t = type_expr fdef local_env param_env test in
          if t = Bool
          then 
            let () = typecheck_seq s1 in
            line := !line +1;
            let () = typecheck_seq s2 in
            let nth1 = List.nth s1 ((List.length s1)-1) in
            match nth1 with
            | Return(e1)-> 
              begin 
                let t1 = type_expr fdef local_env param_env e1 in
                let nth2 = List.nth s2 ((List.length s2)-1) in
                match nth2 with
                | Return(e2) -> 
                  begin
                    let t2 = type_expr fdef local_env param_env e2 in
                    if t1 <> t2
                    then type_error (type_to_string t2) (type_to_string t1) "\"then\" or \"else\" branch of \"if\" statement"
                    else line := !line +1;
                  end
                | _ -> failwith (Printf.sprintf "\"then\" branch has a return value but \"else\" branch doesn't in \"if\" statement at line %i" !line) 
              end
            | _ -> 
              begin
                let nth2 = List.nth s2 ((List.length s2)-1) in
                match nth2 with
                | Return(_) -> failwith (Printf.sprintf "\"then\" branch doesn't have a return value but \"else\" branch does in \"if\" statement at line %i" !line)
                | _ -> line := !line +1;
              end
          else type_error (type_to_string t) "Bool" "condition of \"if\" statement"
        end
            
      | While(test, s) ->
        begin 
          let t = type_expr fdef local_env param_env test in
          match t with
            | Bool -> typecheck_seq s; line := !line +1;
            | _ -> type_error (type_to_string t) "Bool" "condition of \"while\" statement" 
        end
            
      | Set(var, e) -> 
        begin 
          let t1 = type_expr fdef local_env param_env e in
          let t2 = type_expr fdef local_env param_env (Get var) in
          if t1 <> t2 
          then type_error (type_to_string t1) (type_to_string t2) (Printf.sprintf "%s = expression" var)
        end
         
      | Expr e -> 
        begin 
          match type_expr fdef local_env param_env e with
          | Bool | Int | Void | Ptr(_) | Tab(_) -> () 
        end
      
      | Putchar e -> 
        begin 
          let t = type_expr fdef local_env param_env e in
          match t with
          | Int -> ()
          | _ -> type_error (type_to_string t) "Int" "Putchar"
        end
      
      | For(init, test, iter, s) -> 
        typecheck_instr init;
        typecheck_instr iter;
        begin 
          let t = type_expr fdef local_env param_env test in
          match t with
          | Bool -> typecheck_seq s; line := !line +1
          | _ -> type_error (type_to_string t) "Bool" "condition of \"for\" statement"
        end
        
    and typecheck_seq s =
      List.iter (fun x ->  line := !line +1; typecheck_instr x) s
    in
    (* Code principal du typage d'une fonction : on type ses instructions. *)
    typecheck_seq (fdef.code);
    line := !line +1
  in
  let fdef = { name="P"; params=[]; return=Void; locals=[]; code=[] } in
  let e1 = Env.empty in
  let pred (_, t, v) =
    match v with
    | None -> ()
    | Some va ->
      let tva = type_expr fdef e1 e1 va in
      if (tva = t) || (tva = Void)
      then ()
      else type_error (type_to_string tva) (type_to_string t) "global variable declaration"
  in
  List.iter (fun x -> line := !line +1; pred x ) prog.globals;

  (* Code principal du typage d'un programme : on type ses fonctions.
     Il faudrait aussi vérifier les valeurs initiales des variables globales.
     À COMPLÉTER
   *)
  List.iter typecheck_function (prog.functions);
