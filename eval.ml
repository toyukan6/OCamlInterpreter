open Syntax 
open List

type exval = 
    IntV of int
  | BoolV of bool
  | ListV of exval list
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ListV l ->
    (match l with
	[] -> "[]"
      | [h] -> "[" ^ string_of_exval h ^ "]"
      | h :: t -> "[" ^ string_of_exval h ^ fold_right (fun x y -> ";" ^ (string_of_exval x) ^ y) t "]")
  | ProcV (_,_,_) -> "<fun>"
  | DProcV (_,_) -> "<fun>"

let pp_val v =
  print_string (string_of_exval v);
  print_newline ()

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be bool: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be bool: ||")
  | Cons, IntV i, ListV l -> ListV ((IntV i) :: l)
  | Cons, BoolV b, ListV l -> ListV ((BoolV b) :: l)
  | Cons, ListV l1, ListV l2 -> ListV ((ListV l1) :: l2)
  | Cons, _, _ -> err ("Both arguments must be equal: ::") 

let uncurry f = fun (x, y) -> f x y

let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | LLit l -> ListV (map (eval_exp env) l)
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
  | MatchExp (exp, cond) ->
    (let test = eval_exp env exp in
     let rec condEqual t c e =
       match (t, c) with
	   (IntV var1, IntCond var2) -> (var1 = var2, e)
	 | (BoolV var1, BoolCond var2) -> (var1 = var2, e)
	 | (var1, VarCond id) -> (true, Environment.extend id var1 e)
	 | (ListV [], NullListCond)
	 | (ListV _, ListCond [Underbar]) -> (true, e)
	 | (ListV _ as l, ListCond [VarCond id]) -> (true, Environment.extend id l e)
	 | (ListV (head :: tail), ListCond (hc :: tc)) ->
	   let (b, newenv) = condEqual head hc e in
	   if b then condEqual (ListV tail) (ListCond tc) newenv else (false, e)
	 | (ListV l, SemiListCond [VarCond id]) ->
	   (match l with
	       [x] -> (true, Environment.extend id x e)
	     | _ -> (false, e))
	 | (ListV l, SemiListCond cond) ->
	   (try
	      let clist = combine l cond in
	      let (b, newenv) =
		fold_right (fun (ex, co) (bo, en) -> let (boo, nen) = condEqual ex co en in (boo && bo, nen)) clist (true, e) in
	      if b then (true, newenv) else (false, e)
	    with Invalid_argument _ -> (false, e))
	 | (_, Underbar) -> (true, e)
	 | _ -> (false, e) in
     let rec searchcond list =
       match list with
	 [] -> err "Match pattern does not exist"
       | (c, e) :: t ->
	 let (b, newenv) = condEqual test c env in
	 if b then eval_exp newenv e else searchcond t in
     searchcond cond)
  | LetExp (list, exp2) ->
    let eval_list = function
        ([], exp) -> err ("give me id : LetExp")
      | (i :: rest, exp) -> (i, eval_exp env (FunExp (rest, exp))) in
    let elist = map eval_list list in
      eval_exp (fold_right (uncurry Environment.extend) elist env) exp2
  | LetRecExp (list, exp) ->
    (let dummyenv = ref Environment.empty in
     let eval_list = function
         ([], exp) -> err ("give me id : LetRecExp")
       | ([i], FunExp (h :: t, exp))
       | (i :: h :: t, exp) -> (i, ProcV (h, FunExp (t, exp), dummyenv))
       | ([i], exp) -> (i, eval_exp env exp) in
     let elist = map eval_list list in
     let newenv = fold_right (uncurry Environment.extend) elist env in
     dummyenv := newenv;
     eval_exp newenv exp)
  | FunExp ([], exp) -> eval_exp env exp
  | FunExp ([id], exp) -> ProcV (id, exp, ref env)
  | FunExp (id :: rest, exp) -> ProcV (id, FunExp (rest, exp), ref env)
  | DFunExp ([], exp) -> eval_exp env exp
  | DFunExp ([id], exp) -> DProcV (id, exp)
  | DFunExp (id :: rest, exp) -> DProcV (id, FunExp (rest, exp))
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1
      and arg = eval_exp env exp2 in
        (match funval with
	    ProcV (id, body, env') ->
	      let newenv = Environment.extend id arg !env' in
                eval_exp newenv body
	  | DProcV (id, body) ->
	      let newenv = Environment.extend id arg env in
                eval_exp newenv body
	  | _ -> err ("Non-function value is applied"))
    

let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ([("-", v)], env)
  | Decl (list, opt) ->
    (let eval_list = function
        ([], exp) -> ("-", eval_exp env exp)
      | ([i], exp) -> (i, eval_exp env exp)
      | (i :: rest, exp) -> (i, eval_exp env (FunExp (rest, exp))) in
     let elist = map eval_list list in
     let newenv = fold_right (uncurry Environment.extend) elist env in
     match opt with
         None -> (elist, newenv)
       | Some e2 ->
	 let (list', env') = eval_decl newenv e2 in
	 (elist @ list', env'))
  | RecDecl list ->
    (let dummyenv = ref Environment.empty in
     let eval_list = function
         ([], exp) -> err ("give me id : LetRecExp")
       | ([i], FunExp (h :: t, exp))
       | (i :: h :: t, exp) -> (i, ProcV (h, FunExp (t, exp), dummyenv))
       | ([i], exp) -> (i, eval_exp env exp) in
     let elist = map eval_list list in
     let newenv = fold_right (uncurry Environment.extend) elist env in
     dummyenv := newenv;
     (elist, newenv))
