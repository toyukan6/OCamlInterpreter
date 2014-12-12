open Syntax
open List

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

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
  let newsub = fold_left (fun l s -> map (fun (tv, t) -> (tv, replace t s)) l) sub sub in
  fold_left replace ty1 newsub
(* eqs_of_subst : subst -> (ty * ty) list *)
let eqs_of_subst s = map (fun (v, t) -> (TyVar v, t)) s
(* subst_eqs : subst -> (ty * ty) list -> (ty * ty) list *)
let subst_eqs s eqs =
  let replace (leq, req) =
    match leq, req with
	TyVar v, t
      | t, TyVar v -> (try (assoc v s, t) with Not_found -> (leq, req))
      | _, _ -> (leq, req) in
  map replace eqs
(* unify : (ty * ty) list -> subst *)
let rec unify = function
    [] -> []
  | (TyFun (t1, t2), TyFun (t3, t4)) :: rest ->
    if t1 = t3 && t2 = t4
    then unify rest
    else unify ((t1, t3) :: (t2, t4) :: rest)
  | (TyInt, TyInt) :: rest
  | (TyBool, TyBool) :: rest -> unify rest
  | (TyList t1, TyList t2) :: rest -> unify ((t1, t2) :: rest)
  | (TyVar v, t) :: rest
  | (t, TyVar v) :: rest ->
    if MySet.member v (freevar_ty t)
    then err "unify error: member"
    else (v, t) :: unify rest
  | (t1, t2) :: rest ->
    pp_ty t1; print_newline ();
    pp_ty t2; print_newline ();
    err "unify error: unknown pattern"

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Cons -> ([(ty2, TyList ty1)], TyList ty1)

let uncurry f = fun (x, y) -> f x y

let init =
  let rec i r = function
      [_] -> []
    | h :: t -> i (h :: r) t in
  i []

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
    (let (s1, ty1) = ty_exp tyenv test in
     let rec ty_cond env = function
         IntCond _, exp -> (ty_exp env exp, TyInt)
       | BoolCond _, exp -> (ty_exp env exp, TyBool)
       | NullListCond, exp 
       | ListCond [], exp 
       | SemiListCond [], exp -> (ty_exp env exp, TyList (TyVar (fresh_tyvar ())))
       | VarCond v, exp ->
	 let alpha = TyVar (fresh_tyvar ()) in
	 (ty_exp (Environment.extend v alpha env) exp, alpha)
       | ListCond [VarCond v], exp ->
 	 let alpha = TyVar (fresh_tyvar ()) in
	 (ty_exp (Environment.extend v alpha env) exp, TyList alpha)
       | ListCond [c], exp ->
	 let ((s, t) ct) = ty_cond env (c, exp) in
	 ((s, t), TyList ct)
       | SemiListCond [x], exp ->
	 let tyl = map ty_cond l in
	 let tys = map snd tyl in
	 let eqs = combine (init tys) (tl tys) @ concat (map (fun x -> eqs_of_subst (fst x)) tyl) in
	 let s = unify eqs in (s, TyList (subst_type s (hd tys))) in
     let tlist = map (tycond tyenv) cond in

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
