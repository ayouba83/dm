(**
   Sémantique et interprétation de IMP
*)

(* open Minic_ast
module Env = Map.Make(String)


type value =      (*valeur de retour du programme*)
  | VCst of int
  | VBCst of bool
  | VArray of value array

type result =
  | REnd 
  | RContinue
  | RReturn of value

(* 
   On en profite pour placer les fonctions d'évaluation comme
   fonctions internes d'un fonction d'évaluation globale, dans
   laquelle les environnements sont définis une fois pour toute. 

   Ici, pour gérer l'environnement local lors de l'appel des
   fonctions on le fait en deux étages, avec une fonction
   [exec_with_locals] au niveau de laquelle on définit un nouvel
   environnement local qui est l'environnement des paramètres de la fonction.
*) 

let exec_program (prog : prog): string =  (*on execute un programme qui nous renvoie une valeur*)
  let global_env = (*on définit l'environnement global comme l'environnement des variables du programme*)
    List.fold_left (fun env (x, _, v) -> 
                        match v with
                        | None -> Env.add x (Cst 0) env
                        | Some va -> Env.add x va env) Env.empty prog.globals
  in
  
  let fun_env =  (*on définit l'environnement des fonctions pareillement*)
    List.fold_left (fun env fonc -> Env.add fonc.name fonc env) Env.empty prog.functions
  in

  let rec exec_with_locals (func : fun_def) (param_values : expr list) (glob_env : expr Env.t) : string = (*on effectue les fonction une à une qui nous retourne des valeurs *)
    
    let fun_par = List.map2 (fun x y -> (x, y)) func.params param_values in
    let local_env_params = 
      List.fold_left (fun env ((name, _), v) -> Env.add name v env) Env.empty fun_par
    in
    
    let local_env =
      List.fold_left (fun env (x, _, v) -> 
                        match v with
                        | None -> Env.add x (Cst 0) env
                        | Some va -> Env.add x va env) local_env_params func.locals 
    in
    
    let rec eval: expr -> value = function
      | Cst n -> VCst n
      | BCst b -> VBCst b
      | Array arr -> VArray (Array.map eval arr)
      | Get x ->
        begin 
          try
            eval (Env.find x local_env)
          with Not_found -> 
            eval (Env.find x glob_env) (*le typechecker est lancé avant et dit déjà si la var n'existe pas*)
        end
      | Par e -> eval e
      | Add(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst va, VCst vb -> VCst(va + vb)
        | _, _ -> failwith "Not good type" end
      | Sub(e1, e2) ->
            let v1 = eval e1 in
            let v2 = eval e2 in
            begin match v1, v2 with
            | VCst a, VCst b -> VCst(a-b)
            | _, _ -> failwith "not good type" end
      | Mul(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin  match v1, v2 with
        | VCst a, VCst b -> VCst(a*b)
        | _, _ -> failwith "not good type" end
      | Div(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst a, VCst b -> VCst(a/b)
          | _, _ -> failwith "not good type" end
      | Opp(e) -> 
        begin match eval e with
        | VCst a -> VCst (-a) end
      | Lt(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst a, VCst b -> VBCst(a<b)
      | _, _ -> failwith "not good type" end
      | Gt(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst a, VCst b -> VBCst(a>b)
        | _, _ -> failwith "not good type" end
      | Le(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst a, VCst b -> VBCst(a<=b)
        | _, _ -> failwith "not good type" end
      | Ge(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
       begin  match v1, v2 with
        | VCst a, VCst b -> VBCst(a>=b)
        | _, _ -> failwith "not good type" end
      | Eq(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
       begin  match v1, v2 with
        | VCst a, VCst b -> VBCst(a=b)
        | VBCst a, VBCst b -> VBCst(a=b)
        | _, _ -> failwith "not good type" end
      | Ne(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        begin match v1, v2 with
        | VCst a, VCst b -> VBCst(a!=b)
        | VBCst a, VBCst b -> VBCst(a!=b)
        | _, _ -> failwith "not good type" end
      | Not(e) -> 
       begin  match eval e with
        | VBCst a -> VBCst(not a)
        | _ -> failwith "not good type" end
      | Andl(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
       begin match v1, v2 with
        |VBCst a, VBCst b -> VBCst (a && b)
        |_ -> failwith "not good type" end
        
      | Orl(e1, e2) ->
          let v1 = eval e1 in
          let v2 = eval e2 in
         begin match v1, v2 with
          |VBCst a, VBCst b -> VBCst (a || b)
          |_ -> failwith "not good type" end
     | And(e1, e2) ->
          let v1 = eval e1 in
          let v2 = eval e2 in
          begin match v1, v2 with
          |VCst a, VCst b -> VCst (a && b)
          |_ -> failwith "not good type" end
      | Or(e1, e2) ->
          let v1 = eval e1 in
          let v2 = eval e2 in
          begin match v1, v2 with
          |VCst a, VCst b -> VCst ((a||b))
          |_ -> failwith "not good type" end
      | Incr(x) -> 
        let get = Get(x) in
        begin  match eval get with
        |VCst(value) -> let _ = execi Set(x, VCst(value+1)) in VCst(value+1);  
        |_-> failwith "not good type" end
      | Decr(x) -> 
        let get = Get(x) in
        begin match eval get with
        |VCst(value) -> let _ = execi Set(x, VCst(value-1)) in VCst(value-1);  
        |_-> failwith "not good type" end
      | Elm(e, tab) ->
        let get = Get(tab) in
        let value = eval get in
        begin match eval value with
        |VArray arr -> begin match eval e with
                |VCst va -> arr.(va)
                |_-> failwith "not good type" end
        |_-> failwith "not good type" end
      | Len(tab) -> 
        let get = Get(tab) in
        let value = eval get in
        begin match value with
        |VArray(arr) -> VCst(Array.length arr)
        |_ -> failwith "not an array" end
      
      | Call(fname, args) ->
        let f = Env.find fname fun_env in
        let new_env = List.fold_left (fun g_env (x, e) -> Env.add x e g_env) glob_env local_env in
        exec_with_locals f args new_env
   and execi: instr -> result = function
      | Set(x, e) ->
            let v = eval e in
            let _ = Env.add x e local_env in
            RContinue
      | Expr e -> let _ = eval e in RContinue
      | Putchar e -> let _ = Printf.sprintf "%s\n" (value_to_string (eval e)) in RContinue
      | If(e, b1, b2) ->
        let v = eval e in
        match v with
        |VBCst(b) -> if b
                    then execb b1
                    else execb b2
        
      | While(e, b) as i -> match eval e with
              |VBcst(c) -> if c
                    then begin match execb b with
                      | REnd | RContinue -> execi i
                      | RReturn _ as r -> r
                    end
                    else REnd
        
      | For(init, test, iter, s) as i ->
        let ini = execi init in
       match eval test with
      |VBCst(tes) -> if tes
                    then 
                      match execb s with
                      | REnd | RContinue -> 
                        let _ = execi iter in
                        execi i
                      | RReturn _ as r -> r
                    else
                      REnd
      | Return(e) -> RReturn(eval e)
      | Insert(pos, e, tab) -> 
        let v = eval e in
        let i = eval pos in
        let get = Get(tab) in
        begin match eval get with
        | VArray(arr) -> begin match i with
                    |VCst j -> Array.set arr j v
                  end
        end 
    and execb: seq -> result = function
      | [] -> REnd
      | i :: b' ->
        begin match execi i with
          | REnd -> execb b'
          | r -> r
        end
    in
    
    let ret : result -> value = function
      | RReturn value -> value
      | REnd | RContinue -> VCst 0
    in
    
    ret (execb (func.code))
  in
  
  let ret_val = exec_with_locals (Env.find "main" fun_env) [] global_env in
  let rec value_to_string v = function
    | VCst a -> Printf.sprintf "%i" a
    | VBCst b -> match b with | true -> "true" | false -> "false"
    | VArray arr -> "["^(Array.fold_left (fun s v -> s^(value_to_string v)^", ") "" arr)^"]"
  in
  value_to_string ret_val
*)