(**
   Sémantique et interprétation de IMP
*)

open Minic_ast
module Env = Map.Make(String)


type value =      (*valeur de retour du programme*)
  | Cst of int
  | BCst of bool
  | Array of expr array

type Result =
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

let exec_program (programme : prog): string =  (*on execute un programme qui nous renvoie une valeur*)
  let global_env = (*on définit l'environnement global comme l'environnement des variables du programme*)
    List.fold_left (fun env (x, _, v) -> 
                        match v with
                        | None -> Env.add x 0 env
                        | Some va -> Env.add x va env) Env.empty prog.globals
  in
  
  let fun_env =  (*on définit l'environnement des fonctions pareillement*)
    List.fold_left (fun env fonc -> Env.add fonc.name fonc env) Env.empty prog.functions
  in

  let rec exec_with_locals (func : fun_def) (param_values : value list) (glob_env : String*value) : string = (*on effectue les fonction une à une qui nous retourne des valeurs *)
    
    let local_env_params = 
      List.fold_left (fun env ((name, _), v) -> Env.add name v env) Env.empty (func.params, param_values)
    in
    
    let local_env =
      List.fold_left (fun env (x, _, v) -> 
                        match v with
                        | None -> Env.add x 0 env
                        | Some va -> Env.add x va env) local_env_params func.locals 
    in
    
    let rec eval: expr -> value = function
      | Cst n -> n
      | BCst b -> b
      | Array arr -> arr
      | Get x -> 
        try Env.find local_env x 
        with Not_Found -> Env.find glob_env x (*le typechecker est lancé avant et dit déjà si la var n'existe pas*)
      | Par e -> eval e
      | Add(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 + v2
      | Sub(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 - v2
      | Mul(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 * v2
      | Div(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 / v2
      | Opp(e) -> -(eval e)
      | Lt(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 < v2
      | Gt(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 > v2
      | Le(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 <= v2
      | Ge(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 >= v2
      | Eq(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 = v2
      | Ne(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 != v2
      | Not(e) -> !(eval e)
      | Andl(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 && v2
      | Orl(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 || v2
      | And(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 & v2
      | Xor(e1, e2) ->
        let v1 = eval e1 in
        let v2 = eval e2 in
        v1 ^ v2
      | Incr(x) -> 
        let get = Get(x) in
        let value = eval get in
        value+1
      | Incr(x) -> 
        let get = Get(x) in
        let value = eval get in
        value-1
      | Elm(e, tab) ->
        let get = Get(tab) in
        let value = eval get in
        value.(eval e)
      | Len(tab) -> 
        let get = Get(tab) in
        let value = eval get in
        Array.fold_left (fun cpt _ -> cpt+1) 0 value
      | Call(fname, args) ->
        let f = Env.find fname fun_env in
        let new_env = List.fold_left (fun g_env (x, e) -> Env.add x e g_env) glob_env local_env
        exec_with_locals f args new_env
    in
        
    let rec execi: instr -> Result = function
      | Set(x, e) ->
        let v = eval e in
        let _ = Env.add x e local_env in
        RContinue
      | Expr e -> RContinue
      | Putchar e -> Printf.sprintf "%s\n" (value_to_string (eval e))
      | If(e, b1, b2) ->
        let v = eval e in
        if v
        then execb b1
        else execb b2
      | While(e, b) as i -> 
        let v = eval e in
        if v
        then begin match execb b with
          | REnd | RContinue -> execi i
          | RReturn _ as r -> r
        end
        else REnd
      | For(init, test, iter, s) as i ->
        let ini = execi init in
        let tes = eval test in
        if tes
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
        let get = Get(tab) in
        let t = eval get in
        let new_t = Set(t.(eval pos), eval e) in
        let _ = execi new_t in
        let _ = Env.add tab t local_env in
        RContinue
    and execb: seq -> Result = function
      | [] -> REnd
      | i :: b' ->
        begin match execi i with
          | REnd -> execb b'
          | r -> r
        end
    in
    
    let ret : Result -> Value = function
      | RReturn value -> value
      | REnd | RContinue -> Cst 0
    in
    
    ret (execb (func.code))
  in
  
  let ret_val = exec_with_locals (Env.find "main" fun_env) [] global_env in
  let rec value_to_string v = function
    | Cst a -> Printf.sprintf "%i" a
    | BCst b -> match b with | true -> "true" | false -> "false"
    | Array arr -> "["^(Array.fold_left (fun s v -> s^(value_to_string v)^", ") "" arr)^"]"
  in
  value_to_string ret_val
in
