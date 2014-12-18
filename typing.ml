open Syntax
open List

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list
let show_eqs l = print_string "show\n";
  map (fun (ty1, ty2) -> pp_ty ty1; print_string "="; pp_ty ty2; print_newline() ) l; print_newline ()
(* eqs_of_subst : subst -> (ty * ty) list *)
let eqs_of_subst s = map (fun (v, t) -> (TyVar v, t)) s
(* subst_type : subst -> ty -> ty *)
let rec subst_type sub ty1 =
  let rec replace ty (tv, t) =
    match ty, t with
	TyVar v, TyVar v2 ->
	  if tv = v then t
	  else if v = v2 then (TyVar tv)
	  else ty
      | TyVar v, _ -> if tv = v then t else ty
      | TyList t1, _ -> TyList (replace t1 (tv, t))
      | TyFun (t1, t2), _ -> TyFun (replace t1 (tv, t), replace t2 (tv, t))
      | _, _ -> ty in
  let repl (tv1, t1) s =
    match t1 with
	TyVar v -> [(tv1, replace (TyVar tv1) s); (v, replace t1 s)]
      | _ -> [(tv1, replace t1 s)] in
  let newsub = fold_left (fun l s -> s :: concat (map (fun t -> repl t s) l)) [] (rev sub) in
  fold_left replace ty1 newsub
(* subst_eqs : subst -> (ty * ty) list -> (ty * ty) list *)
let subst_eqs s eqs =
  let replace (leq, req) =
    match leq, req with
	TyVar v, t
      | t, TyVar v -> (try (assoc v s, t) with Not_found -> (leq, req))
      | _, _ -> (leq, req) in
  map replace eqs
let assoc_all key list =
    fold_right (fun (a, b) l -> if key = a then b :: l else l) list []
(* unify : (ty * ty) list -> subst *)
let unify l = 
  let rec u r l =
    match l with
      [] -> r
    | (TyFun (t1, t2), TyFun (t3, t4)) :: rest ->
      if t1 = t3 && t2 = t4
      then u r rest
      else u r ((t1, t3) :: (t2, t4) :: rest)
    | (TyInt, TyInt) :: rest
    | (TyBool, TyBool) :: rest -> u r rest
    | (TyList t1, TyList t2) :: rest -> u r ((t1, t2) :: rest)
    | (TyVar v, t) :: rest
    | (t, TyVar v) :: rest ->
      if MySet.member v (freevar_ty t)
      then err "unify error: member"
      else u ((v, t) :: r) rest
    | (t1, t2) :: rest ->
      pp_ty t1; print_newline ();
      pp_ty t2; print_newline ();
      err "unify error: unknown pattern" in
  let uni = u [] l in
  let var_equal = function
      (_, TyVar _) -> true
    | _ -> false in
  let ves = filter var_equal uni and nves = filter (fun x -> not (var_equal x)) uni in
  let equals v1 v2 = exists (fun v -> v1 == v || v2 == v)
  and insert v l = if mem v l then l else v :: l in
  let rec classify ((v1, (TyVar v2)) as tl) = function
      [] -> [[v1;v2]]
    | h :: t -> if equals v1 v2 h then insert v2 (insert v1 h) :: t else h :: classify tl t in
  let classified = fold_right classify ves [] in
  let values = map (fun l -> fold_right (fun v vs -> assoc_all v nves @ vs) l []) classified in
  let allpattern l1 l2 = concat (map (fun e1 -> fold_right (fun e2 l -> (e1, e2) :: l) l2 []) l1) in
  let eqs = concat (map (fun (l, v) -> allpattern (map (fun tv -> TyVar tv) l) v @ allpattern v v) (combine classified values)) in
  (*show_eqs eqs;*)
  u [] (eqs @ eqs_of_subst nves)

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Cons -> ([(ty2, TyList ty1)], TyList ty1)

let uncurry f = fun (x, y) -> f x y

let rec init = function
    [] -> raise (Invalid_argument "null list : init")
  | [h] -> []
  | h :: t -> h :: init t

let rec ty_exp tyenv = function
    Var x ->
      (try ([],Environment.lookup x tyenv) with
	  Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> ([],TyInt)
  | BLit _ -> ([], TyBool)
  | LLit [] -> ([], TyList (TyVar (fresh_tyvar ())))
  | LLit (h :: t) ->
    let (s1, ty1) = ty_exp tyenv h
    and (s2, ty2) = ty_exp tyenv (LLit t) in
    let eqs = (ty2, TyList ty1) :: eqs_of_subst (s1 @ s2) in
    let s3 = unify eqs in (s3, subst_type s3 ty2)
  | BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1
    and (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = eqs_of_subst (s1 @ s2) @ eqs3 in
    let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
    let (s1, ty1) = ty_exp tyenv exp1
    and (s2, ty2) = ty_exp tyenv exp2
    and (s3, ty3) = ty_exp tyenv exp3 in
    let eqs = (ty1, TyBool) :: (ty2, ty3) :: eqs_of_subst (s1 @ s2 @ s3) in
    let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (list, exp) ->
    let eval_list = function
        ([], exp2) -> err "give me id : LetExp"
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, t)
      | (i :: rest, exp2) -> let (s, t) = ty_exp tyenv (FunExp (rest, exp2))
			    in (i, t) in
    let elist = map eval_list list in
      ty_exp (fold_right (uncurry Environment.extend) elist tyenv) exp
  | LetRecExp (list, exp) ->
    (let eval_list = function
        ([], exp2) -> err "give me id : LetRecExp"
      | ([i], FunExp (h :: t, exp2))
      | (i :: h :: t, exp2) ->
        let alpha = TyVar (fresh_tyvar ())
        and beta = TyVar (fresh_tyvar ()) in
        let tyenv2 = Environment.extend i (TyFun (alpha, beta)) tyenv in
        let tyenv3 = Environment.extend h alpha tyenv2 in
        let (s, ty) = ty_exp tyenv3 (FunExp (t, exp2)) in
	(i, TyFun (subst_type s alpha, ty))
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, t) in
     let elist = map eval_list list in
     ty_exp (fold_right (uncurry Environment.extend) elist tyenv) exp)
  | FunExp ([], exp) -> ty_exp tyenv exp
  | FunExp ([id], exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty =
      ty_exp (Environment.extend id domty tyenv) exp in
    (s, TyFun (subst_type s domty, ranty))
  | FunExp (id :: rest, exp) ->
    ty_exp tyenv (FunExp ([id], FunExp (rest, exp)))
  | AppExp (exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1
    and (s2, ty2) = ty_exp tyenv exp2
    and alpha = TyVar (fresh_tyvar ()) in
    let eqs = (TyFun (ty2, alpha), ty1) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in (s3, subst_type s3 alpha)
  | MatchExp (test, cond) ->
    (let rec ty_cond env = function
         IntCond _ -> ([], TyInt, env)
       | BoolCond _ -> ([], TyBool, env)
       | NullListCond 
       | ListCond []
       | SemiListCond [] -> ([], TyList (TyVar (fresh_tyvar ())), env)
       | VarCond v -> 
 	 let alpha = TyVar (fresh_tyvar ()) in
	 ([], alpha, Environment.extend v alpha env)
       | ListCond [VarCond v] ->
 	 let alpha = TyVar (fresh_tyvar ()) in
	 ([], TyList alpha, Environment.extend v (TyList alpha) env)
       | ListCond (h :: t) ->
	 let (s1, t1, e1) = ty_cond env h in
	 let (s2, t2, e2) = ty_cond e1 (ListCond t) in
	 let eqs = (t2, TyList t1) :: eqs_of_subst (s1 @ s2) in
	 let s3 = unify eqs in (s3, subst_type s3 t2, e2)
       | SemiListCond l ->
	 let (hs, ht, he) = ty_cond env (hd l) in
	 let (s1, tyl, newenv) =
	   fold_right (fun c (s, t, e) -> let (ss, tt, ee) = ty_cond e c in (ss @ s, tt :: t, ee)) (tl l) (hs, [ht], he) in 
	 let eqs = combine (init tyl) (tl tyl) @ eqs_of_subst s1 in
	 let s2 = unify eqs in (s2, TyList (subst_type s2 (hd tyl)), newenv) in
     let (s1, ty1) = ty_exp tyenv test
     and tlist =
       map (fun (c, e) -> let (s, t, ne) = ty_cond tyenv c in let (s2, t2) = ty_exp ne e in (s @ s2, t, t2)) cond in
     let sl = concat (map (fun (x, y, z) -> x) tlist)
     and tyl = map (fun (x, y, z) -> y) tlist
     and tyl2 = map (fun (x, y, z) -> z) tlist in
     let eqs = (ty1, hd tyl) :: combine (init tyl) (tl tyl) @ combine (init tyl2) (tl tyl2) @ eqs_of_subst sl in (*show_eqs eqs;*)
     let s2 = unify eqs in (*show_eqs (eqs_of_subst s2);*) (s2, subst_type s2 (hd tyl2)))
  | _ -> err "Not Implemented!"

let rec ty_decl tyenv = function
    Exp e -> let (_, t) = ty_exp tyenv e in ([t], tyenv)
  | Decl (list, opt) ->
    (let ty_list = function
        ([], exp2) -> err "give me id : LetDecl"
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, t)
      | (i :: rest, exp2) -> let (s, t) = ty_exp tyenv (FunExp (rest, exp2))
			    in (i, t) in
     let tlist = map ty_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     match opt with
	 None -> (map snd tlist, newtyenv)
       | Some e2 ->
	 let (list', env') = ty_decl newtyenv e2 in
	 (map snd tlist @ list', env'))
  | RecDecl list ->
    (let eval_list = function
        ([], exp2) -> err "give me id : LetRecDecl"
      | ([i], FunExp (h :: t, exp2))
      | (i :: h :: t, exp2) ->
        let alpha = TyVar (fresh_tyvar ())
        and beta = TyVar (fresh_tyvar ()) in
        let tyenv2 = Environment.extend i (TyFun (alpha, beta)) tyenv in
        let tyenv3 = Environment.extend h alpha tyenv2 in
        let (s, ty) = ty_exp tyenv3 (FunExp (t, exp2)) in
	(i, TyFun (subst_type s alpha, ty))
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, t) in
     let tlist = map eval_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     (map snd tlist, newtyenv))
