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

let pp_cond c =
  print_string (string_of_cond c);
  print_newline ();

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

type program = 
    Exp of exp
  | Decl of (id list * exp) list * program option
  | RecDecl of (id list * exp) list;;

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyList of ty
  | TyVar of tyvar
  | TyFun of ty * ty

(* pretty printing *)
let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyList l -> pp_ty l; print_string " list"
  | TyVar v -> print_string ("'" ^ (Char.escaped (char_of_int (v + 97))))
  | TyFun (ty1, ty2) ->
    (match ty1 with
	TyFun _ ->
	  print_string "(";
	  pp_ty ty1;
	  print_string ")"
      | _ -> pp_ty ty1);
    print_string "->";
    pp_ty ty2

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
    | _ -> MySet.empty
