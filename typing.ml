open Syntax
open List

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = tysc Environment.t

type subst = (tyvar * ty) list
let show_eqs eqs =
  print_string "show\n";
  map (fun (ty1, ty2) -> print_string (string_of_ty ty1 ^ "=" ^ string_of_ty ty2 ^ "\n")) eqs;
  print_newline ()

let assoc_all key list =
    fold_right (fun (a, b) l -> if key = a then b :: l else l) list []
let equals v1 v2 = exists (fun v -> v1 == v || v2 == v)
let insert v l = if mem v l then l else v :: l
let unique list = fold_right (fun x l -> insert x l) list []
let allpattern l1 l2 = concat (map (fun e1 -> fold_right (fun e2 l -> (e1, e2) :: l) l2 []) l1)
let rec classify ((v1, (TyVar v2)) as tl) = function
    [] -> [[v1;v2]]
  | h :: t ->
    if equals v1 v2 h
    then insert v2 (insert v1 h) :: (classify tl t)
    else h :: classify tl t
let rec classconcat c = function
    [] -> [c]
  | h :: t ->
    if exists (fun c' -> mem c' h) c
    then fold_right (fun c' l -> insert c' l) c h :: t
    else h :: classconcat c t

(* subst_type : subst -> ty -> ty *)
let subst_type sub ty1 =
  let rec replace (tv, t) ty =
    match ty with
      | TyVar v -> if tv = v then t else ty
      | TyList t1 -> TyList (replace (tv, t) t1)
      | TyFun (t1, t2) -> TyFun (replace (tv, t) t1, replace (tv, t) t2)
      | _ -> ty in
  let subst ty cs vs =
    let sub = concat (map (fun c -> allpattern (tl c) [TyVar (hd c)]) cs) in
    let ty2 = fold_right replace sub ty in
    let eqs = concat (map (fun (c, v) -> try [(hd c, hd v)] with _ -> []) (combine cs vs)) in
    fold_right replace eqs ty2 in
  let var_equal = function
      (_, TyVar _) -> true
    | _ -> false in
  let ves = filter var_equal sub and nves = filter (fun x -> not (var_equal x)) sub in
  let fuga l = fold_left (fun s x -> s ^ "=" ^ string_of_ty (TyVar x)) (string_of_ty (TyVar (hd l))) (tl l) in
  let classified = fold_right classconcat (fold_right classify ves []) [] in
  map (fun l -> print_string (fuga l ^ "\n")) classified;
  let values = map (fun l -> try [assoc (hd l) nves] with Not_found -> []) classified in
  subst ty1 classified values
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
    | (TyList t1, TyList t2) :: rest ->
      if t1 = t2
      then u r rest
      else u r ((t1, t2) :: rest)
    | (TyVar v1, TyVar v2) :: rest ->
      if v1 = v2 then u r rest else u ((v1, TyVar v2) :: r) rest
    | (TyVar v, t) :: rest
    | (t, TyVar v) :: rest ->
      if MySet.member v (freevar_ty t)
      then err "unify error: member"
      else u ((v, t) :: r) rest
    | (t1, t2) :: rest ->
      err (string_of_ty t1 ^ " cannot cast to " ^ string_of_ty t2) in
  let uni = u [] l in print_string "unify:"; show_eqs (eqs_of_subst uni);
  let var_equal = function
      (_, TyVar _) -> true
    | _ -> false in
  let ves = filter var_equal uni and nves = filter (fun x -> not (var_equal x)) uni in
  let classified = fold_right classconcat (fold_right classify ves []) [] in
  let values = map (fun l -> fold_right (fun v vs -> assoc_all v nves @ vs) l []) classified in
  let eqs = concat (map (fun (l, v) -> allpattern (map (fun tv -> TyVar tv) l) v @ allpattern v v) (combine classified values)) in
  let eqs2 = map (fun (ty1, ty2) -> (subst_type nves ty1, subst_type nves ty2)) eqs in
  let rec remove_eqs eqs =
    match eqs with
	[] -> []
      | (t1, t2) :: t ->
	if mem (t2, t1) t then remove_eqs t else (t1, t2) :: remove_eqs t in
  let u' = u [] (remove_eqs (unique (eqs2 @ l))) in print_string "unify2:"; show_eqs (eqs_of_subst u'); u'
  (*u [] (eqs2 @ l)*)

let rec freevar_tyenv tyenv =
  Environment.fold_right (fun ts l -> MySet.union (freevar_tysc ts) l) tyenv MySet.empty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map
	 (fun id -> freevar_ty (subst_type subst (TyVar id)))
	 fv_tyenv') in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  print_string "closure:";
  show_eqs (eqs_of_subst subst);
  print_string "type:";
  pp_ty ty;
  print_newline ();
  TyScheme (MySet.to_list ids, subst_type subst ty)
    
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
      (try
	 let TyScheme (vars, ty) = Environment.lookup x tyenv in
	 let s = map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
	 print_string "var:";
	 show_eqs (eqs_of_subst s);
	 (s, subst_type s ty)
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
    let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (list, exp) ->
    let eval_list = function
        ([], exp2) -> err "give me id : LetExp"
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, closure t tyenv s, s)
      | (i :: rest, exp2) -> let (s, t) = ty_exp tyenv (FunExp (rest, exp2))
			     in (i, closure t tyenv s, s) in
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
        let alpha' = fresh_tyvar ()
        and beta' = fresh_tyvar () in
        let alpha = TyVar alpha'
        and beta = TyVar beta' in
	let funt = TyScheme ([alpha';beta'], TyFun (alpha, beta)) in
        let tyenv2 = Environment.extend i funt tyenv in
        let tyenv3 = Environment.extend h (TyScheme ([alpha'], alpha)) tyenv2 in
        let (s, ty) = ty_exp tyenv3 (FunExp (t, exp2)) in
	let t = TyFun (subst_type s alpha, ty) in
	(i, closure t tyenv3 s, s)
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
    let newenv = Environment.extend id (TyScheme ([domty'], domty)) tyenv in
    let s, ranty = ty_exp newenv exp in
    print_string "funexp:";
    show_eqs (eqs_of_subst s);
    (s, TyFun (subst_type s domty, ranty)) 
  | FunExp (id :: rest, exp) ->
    ty_exp tyenv (FunExp ([id], FunExp (rest, exp)))
  | AppExp (exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1
    and (s2, ty2) = ty_exp tyenv exp2
    and alpha = TyVar (fresh_tyvar ()) in
    let eqs = (TyFun (ty2, alpha), ty1) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in
    print_string "app:";
    show_eqs (eqs_of_subst s3);
    (s3, subst_type s3 alpha)
  | MatchExp (test, cond) ->
    (let rec ty_cond env = function
         IntCond _ -> ([], TyInt, env)
       | BoolCond _ -> ([], TyBool, env)
       | NullListCond 
       | ListCond []
       | SemiListCond [] -> ([], TyList (TyVar (fresh_tyvar ())), env)
       | VarCond v -> 
 	 let alpha' = fresh_tyvar () in
 	 let alpha = TyVar alpha' in
	 ([], alpha, Environment.extend v (TyScheme ([alpha'], alpha)) env)
       | ListCond [VarCond v] ->
 	 let alpha' = fresh_tyvar () in
 	 let alpha = TyVar alpha' in
	 ([], TyList alpha, Environment.extend v (TyScheme ([alpha'], (TyList alpha))) env)
       | ListCond (h :: t) ->
	 let (s1, t1, e1) = ty_cond env h in
	 let (s2, t2, e2) = ty_cond e1 (ListCond t) in
	 let eqs = (t2, TyList t1) :: eqs_of_subst (s1 @ s2) in
	 let s3 = unify eqs in
	 (s3, subst_type s3 t2, e2)
       | SemiListCond l ->
	 let (hs, ht, he) = ty_cond env (hd l) in
	 let (s1, tyl, newenv) =
	   fold_right (fun c (s, t, e) -> let (ss, tt, ee) = ty_cond e c in (ss @ s, tt :: t, ee)) (tl l) (hs, [ht], he) in 
	 let eqs = combine (init tyl) (tl tyl) @ eqs_of_subst s1 in
	 let s2 = unify eqs in (s2, TyList (subst_type s2 (hd tyl)), newenv)
       | Underbar ->
	 let alpha' = fresh_tyvar () in
	 let alpha = TyVar alpha' in
	 ([], alpha, env) in
     let (s1, ty1) = ty_exp tyenv test
     and tlist =
       map (fun (c, e) -> let (s, t, ne) = ty_cond tyenv c in let (s2, t2) = ty_exp ne e in (s @ s2, t, t2)) cond in
     let sl = concat (map (fun (x, y, z) -> x) tlist)
     and tyl = map (fun (x, y, z) -> y) tlist
     and tyl2 = map (fun (x, y, z) -> z) tlist in
     let eqs =
       (ty1, hd tyl) :: combine (init tyl) (tl tyl) @ combine (init tyl2) (tl tyl2) @ eqs_of_subst (sl @ s1) in print_string "match exp:"; show_eqs eqs;
     let s2 = unify eqs in (s2, subst_type s2 (hd tyl2)))
  | _ -> err "Not Implemented!"

let rec ty_decl tyenv = function
    Exp e -> let (_, t) = ty_exp tyenv e in ([TyScheme ([], t)], tyenv)
  | Decl (list, opt) ->
    (let ty_list = function
        ([], exp2) -> err "give me id : LetDecl"
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, closure t tyenv s)
      | (i :: rest, exp2) -> let (s, t) = ty_exp tyenv (FunExp (rest, exp2))
			     in (i, closure t tyenv s) in
     let tlist = map ty_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     let scl = map snd tlist in
     match opt with
	 None -> (scl, newtyenv)
       | Some e2 ->
	 let (list', env') = ty_decl newtyenv e2 in
	 (scl @ list', env'))
  | RecDecl list ->
    (let eval_list = function
        ([], exp2) -> err "give me id : LetRecDecl"
      | ([i], FunExp (h :: t, exp2))
      | (i :: h :: t, exp2) ->
        let alpha' = fresh_tyvar ()
        and beta' = fresh_tyvar () in
        let alpha = TyVar alpha'
        and beta = TyVar beta' in
	let funt = TyScheme ([alpha';beta'], TyFun (alpha, beta)) in
        let tyenv2 = Environment.extend i funt tyenv in
        let tyenv3 = Environment.extend h (TyScheme ([alpha'], alpha)) tyenv2 in
        let (s, ty) = ty_exp tyenv3 (FunExp (t, exp2)) in
	let t = TyFun (subst_type s alpha, ty) in
	(i, closure t tyenv3 s)
      | ([i], exp2) -> let (s, t) = ty_exp tyenv exp2 in (i, closure t tyenv s) in
     let tlist = map eval_list list in
     let newtyenv = fold_right (uncurry Environment.extend) tlist tyenv in
     let scl = map snd tlist in
     (scl, newtyenv))
