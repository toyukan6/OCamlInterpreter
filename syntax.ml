(* ML interpreter / type reconstruction *)
type id = string

type comment = string

type binOp = Plus | Mult | Lt | And | Or | Cons

type cond =
    IntCond of int
  | BoolCond of bool
  | VarCond of string
  | NullListCond
  | ListCond of cond list
  | SemiListCond of cond list
  | Underbar

let rec string_of_cond = function
    IntCond i -> string_of_int i
  | BoolCond b -> string_of_bool b
  | VarCond v -> v
  | NullListCond -> "[]"
  | ListCond l
  | SemiListCond l ->
    (match l with
	[] -> "[]"
      | h :: t -> "[" ^ string_of_cond h ^ List.fold_right (fun x y -> ";" ^ (string_of_cond x) ^ y) t "]")
  | Underbar -> "_"

let pp_cond c =
  print_string (string_of_cond c);
  print_newline ()

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | LLit of exp list
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id list * exp) list * exp
  | LetRecExp of (id list * exp) list * exp
  | FunExp of id list * exp
  | DFunExp of id list * exp
  | AppExp of exp * exp
  | MatchExp of exp * (cond * exp) list

let rec make_app v = function
    AppExp (e1, e2) -> AppExp (make_app v e1, e2)
  | e -> AppExp (v, e)

type program = 
    Exp of exp
  | Decl of (id list * exp) list * program option
  | RecDecl of (id list * exp) list

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyList of ty
  | TyVar of tyvar
  | TyFun of ty * ty

let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyList l -> string_of_ty l ^ " list"
  | TyVar v -> "'" ^ (Char.escaped (char_of_int (v + 97)))
  | TyFun (ty1, ty2) ->
    (let s =
       match ty1 with
	   TyFun _ -> "(" ^ string_of_ty ty1 ^ ")"
	 | _ -> string_of_ty ty1 in
     s ^ " -> " ^ string_of_ty ty2)

(* pretty printing *)
let pp_ty ty = print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty ty =
  match ty with
      TyVar v -> MySet.singleton v
    | TyFun (t1, t2) -> MySet.union (freevar_ty t1) (freevar_ty t2)
    | TyList t -> freevar_ty t
    | _ -> MySet.empty

(* type scheme *)
type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let freevar_tysc tysc =
  match tysc with
      TyScheme (l, ty) ->
	MySet.from_list (List.fold_right (fun t tl -> if List.mem t l then tl else t :: tl) (MySet.to_list (freevar_ty ty)) [])
