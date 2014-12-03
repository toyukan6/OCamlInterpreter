open Syntax 
open List

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be integer: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be integer: ||")

let uncurry f = fun (x, y) -> f x y

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (list, exp2) ->
    let elist = map (fun (id, exp) -> (id, eval_exp env exp)) list in
      eval_exp (fold_right (uncurry Environment.extend) elist env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
        (match funval with
	    ProcV (id, body, env') ->
	      let newenv = Environment.extend id arg env' in
                eval_exp newenv body
	  | _ -> err ("Non-function value is applied"))
    

let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ([("-", v)], env)
  | Decl (list, opt) ->
      let elist = map (fun (id, exp) -> (id, eval_exp env exp)) list in
      let newenv = fold_right (uncurry Environment.extend) elist env in
        match opt with
            None -> (elist, newenv)
	  | Some e2 ->
	      let (list', env') = eval_decl newenv e2 in
	      (elist @ list', env')