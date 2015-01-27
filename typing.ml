open Syntax
open List

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = tysc Environment.t

type subst = (tyvar * ty) list
let show_eqs eqs =
  print_string "show\n";
  fold_left (fun () (ty1, ty2) -> print_string (string_of_ty ty1 ^ "=" ^ string_of_ty ty2 ^ "\n")) () eqs;
  print_newline ()

let insert v l = if mem v l then l else v :: l
let unique list = fold_right insert list []
let rec init = function
    [] -> raise (Invalid_argument "null list : init")
  | [h] -> []
  | h :: t -> h :: init t
let last l = hd (rev l)

(* unify : (ty * ty) list -> subst *)
let unify l =
  let rec u = function 
      [] -> []
    | (TyFun (t1, t2), TyFun (t3, t4)) :: rest ->
      if t1 = t3 && t2 = t4
      then u rest
      else u  ((t1, t3) :: (t2, t4) :: rest)
    | (TyInt, TyInt) :: rest
    | (TyBool, TyBool) :: rest -> u rest
    | (TyList t1, TyList t2) :: rest ->
      if t1 = t2
      then u rest
      else u ((t1, t2) :: rest)
    | (TyVar v1, TyVar v2) :: rest ->
      if v1 = v2 then u rest else (v1, TyVar v2) :: u rest
    | (TyVar v, t) :: rest
    | (t, TyVar v) :: rest ->
      if MySet.member v (freevar_ty t)
      then err "unify error: member"
      else (v, t) :: u rest
    | (t1, t2) :: rest ->
      err (string_of_ty t1 ^ " cannot cast to " ^ string_of_ty t2) in
  unique (u l)

(* eqs_of_subst : subst -> (ty * ty) list *)
let eqs_of_subst s = map (fun (v, t) -> (TyVar v, t)) s
(* subst_type : subst -> ty -> ty *)
let rec subst_type sub ty1 =
  let rec replace (tv, t) ty =
    match ty with
	TyVar v ->
	  if tv = v then t
	  else
	    (match t with
	      TyVar v2 -> if tv = v2 then TyVar v else ty
	    | _ -> ty)
      | TyList t1 -> TyList (replace (tv, t) t1)
      | TyFun (t1, t2) -> TyFun (replace (tv, t) t1, replace (tv, t) t2)
      | _ -> ty in
  print_string "subst_type:"; show_eqs (eqs_of_subst sub);
  print_string "type:"; pp_ty ty1; print_newline ();
  if length sub = 0 then ty1
  else 
    let s = last sub and rest = init sub in
    let replace_subst (stv, st) (tv, t) =
      let t' = replace (stv, st) t in
      match t' with
	   TyVar tv2 ->
	    if tv = stv then (st, t')
	    else if stv = tv2 then (TyVar tv, st)
	    else (TyVar tv, t')
	| _ -> if tv = stv then (t', st) else (TyVar tv, t') in
    subst_type (unify (unique (map (replace_subst s) rest))) (replace s ty1)

let rec freevar_tyenv tyenv =
  Environment.fold_right (fun ts l -> MySet.union (freevar_tysc ts) l) tyenv MySet.empty
(* closure : ty -> tyenv -> subst -> tysc *)
let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map
	 (fun id -> freevar_ty (subst_type subst (TyVar id)))
	 fv_tyenv') in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)
    
let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Cons -> ([(ty2, TyList ty1)], TyList ty1)

let uncurry f = fun (x, y) -> f x y

let rec ty_exp tyenv = function
    Var x ->
      (try
	 let TyScheme (vars, ty) = Environment.lookup x tyenv in
	 let s = map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
	 print_string "var:"; show_eqs (eqs_of_subst s);
	 ([], subst_type s ty)
       with
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
    let s4 = unify eqs in
    print_string "ifexp:"; show_eqs (eqs_of_subst s4);
    (s4, subst_type s4 ty2)
  | LetExp (list, exp) ->
    let eval_list = function
        ([], exp2) -> err "give me id : LetExp"
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in
		       (i, closure t tyenv s, s)
      | (i :: rest, exp2) -> let (s, t) = ty_exp tyenv (FunExp (rest, exp2)) in
			     (i, closure t tyenv s, s) in
    let evlist = map eval_list list in
    let elist = map (fun (x, y, z) -> (x, y)) evlist 
    and slist = map (fun (x, y, z) -> z) evlist in
    let (s1, t1) = ty_exp (fold_right (uncurry Environment.extend) elist tyenv) exp in
    (s1 @ concat slist, t1)
  | LetRecExp (list, exp) ->
    (let eval_list = function
        ([], exp2) -> err "give me id : LetRecExp"
      | ([i], FunExp (h :: t, exp2))
      | (i :: h :: t, exp2) ->
        let alpha = TyVar (fresh_tyvar ())
        and beta = TyVar (fresh_tyvar ()) in
	let funt = TyFun (alpha, beta) in
        let tyenv2 = Environment.extend i (tysc_of_ty funt) tyenv in
        let tyenv3 = Environment.extend h (tysc_of_ty alpha) tyenv2 in
        let s, ty = ty_exp tyenv3 (FunExp (t, exp2)) in
	let eqs = (beta, ty) :: eqs_of_subst s in
	let s2 = unify eqs in
	let t = subst_type s2 funt in
	(i, closure t tyenv3 s2, s2)
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, closure t tyenv s, s) in
    let evlist = map eval_list list in
    let elist = map (fun (x, y, z) -> (x, y)) evlist 
    and slist = map (fun (x, y, z) -> z) evlist in
    let (s1, t1) = ty_exp (fold_right (uncurry Environment.extend) elist tyenv) exp in
    (s1 @ concat slist, t1))
  | FunExp ([], exp) -> ty_exp tyenv exp
  | FunExp ([id], exp) ->
    let domty' = fresh_tyvar () in
    let domty = TyVar domty' in
    let newenv = Environment.extend id (tysc_of_ty domty) tyenv in
    let s, ranty = ty_exp newenv exp in
    print_string "funexp:"; show_eqs (eqs_of_subst s);
    (s, TyFun (subst_type s domty, subst_type s ranty)) 
  | FunExp (idl, exp) ->
    let fexp = fold_right (fun i e -> FunExp ([i], e)) idl exp in
    ty_exp tyenv fexp
  | AppExp (exp1, exp2) ->
    let s1, ty1 = ty_exp tyenv exp1
    and s2, ty2 = ty_exp tyenv exp2
    and alpha = TyVar (fresh_tyvar ()) in
    let eqs = (ty1, TyFun (ty2, alpha)) :: eqs_of_subst (s1 @ s2) in
    let s3 = unify eqs in
    print_string "app:"; show_eqs (eqs_of_subst s3);
    (s3, subst_type s3 alpha)
  | MatchExp (test, cond) ->
    (let rec ty_cond env = function
         IntCond _ -> ([], TyInt, env)
       | BoolCond _ -> ([], TyBool, env)
       | NullListCond 
       | ListCond []
       | SemiListCond [] -> ([], TyList (TyVar (fresh_tyvar ())), env)
       | VarCond v ->
	 (try
	    let _ = Environment.lookup v env in
	    err (v ^ " is already exists.")
	  with Environment.Not_bound -> 
 	    let alpha = TyVar (fresh_tyvar ()) in
	    ([], alpha, Environment.extend v (tysc_of_ty alpha) env))
       | ListCond [VarCond v] ->
	 (try
	    let _ = Environment.lookup v env in
	    err (v ^ " is already exists.")
	  with Environment.Not_bound -> 
 	    let alpha = TyVar (fresh_tyvar ()) in
	    ([], TyList alpha, Environment.extend v (tysc_of_ty (TyList alpha)) env))
       | ListCond (h :: t) ->
	 let s1, t1, e1 = ty_cond env h in
	 let s2, t2, e2 = ty_cond e1 (ListCond t) in
	 let eqs = (t2, TyList t1) :: eqs_of_subst (s1 @ s2) in
	 let s3 = unify eqs in
	 (s3, subst_type s3 t2, e2)
       | SemiListCond l ->
	 let (s1, tyl, newenv) =
	   fold_right (fun c (s, t, e) -> let ss, tt, ee = ty_cond e c in (ss @ s, tt :: t, ee)) l ([], [], env) in 
	 let eqs = combine (init tyl) (tl tyl) @ eqs_of_subst s1 in
	 let s2 = unify eqs in (s2, TyList (subst_type s2 (hd tyl)), newenv)
       | Underbar ->
	 let alpha = TyVar (fresh_tyvar ()) in
	 ([], alpha, env) in
     let (s1, ty1) = ty_exp tyenv test
     and tlist =
       map (fun (c, e) -> let s, t, ne = ty_cond tyenv c in let s2, t2 = ty_exp ne e in (s @ s2, t, t2)) cond in
     let sl = concat (map (fun (x, y, z) -> x) tlist)
     and tyl = map (fun (x, y, z) -> y) tlist
     and tyl2 = map (fun (x, y, z) -> z) tlist in
     let eqs =
       (ty1, hd tyl) :: combine (init tyl) (tl tyl) @ combine (init tyl2) (tl tyl2) @ eqs_of_subst (sl @ s1) in
     print_string "match exp:"; show_eqs eqs;
     let s2 = unify eqs in (s2, subst_type s2 (hd tyl2)))
  | _ -> err "Not Implemented!"

let rec ty_decl tyenv = function
    Exp e -> let (_, t) = ty_exp tyenv e in (tyenv, [TyScheme ([], t)])
  | Decl (list, opt) ->
    (let ty_list = function
        ([], exp2) -> err "give me id : LetDecl"
      | ([i], exp2) ->
	let (s, t) = ty_exp tyenv exp2 in
	(i, closure t tyenv s)
      | (i :: rest, exp2) ->
	let (s, t) = ty_exp tyenv (FunExp (rest, exp2)) in
	(i, closure t tyenv s) in
     let tlist = map ty_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     let scl = map snd tlist in
     match opt with
	 None -> (newtyenv, scl)
       | Some e2 ->
	 let (env', list') = ty_decl newtyenv e2 in
	 (env', scl @ list'))
  | RecDecl list ->
    (let eval_list = function
        ([], exp2) -> err "give me id : LetRecDecl"
      | ([i], FunExp (h :: t, exp2))
      | (i :: h :: t, exp2) ->
        let alpha = TyVar (fresh_tyvar ())
        and beta = TyVar (fresh_tyvar ()) in
	let funt = tysc_of_ty (TyFun (alpha, beta)) in
        let tyenv2 = Environment.extend i funt tyenv in
        let tyenv3 = Environment.extend h (tysc_of_ty alpha) tyenv2 in
        let (s, ty) = ty_exp tyenv3 (FunExp (t, exp2)) in
	let eqs = (beta, ty) :: eqs_of_subst s in
	let s2 = unify eqs in
	let tf = TyFun (subst_type s2 alpha, subst_type s2 ty) in
	(i, closure tf tyenv3 s2)
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, closure t tyenv s) in
     let tlist = map eval_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     let scl = map snd tlist in
     (newtyenv, scl))
