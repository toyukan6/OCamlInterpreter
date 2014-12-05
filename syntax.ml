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
  | LetRecExp of (id * id list * exp) list * exp
  | FunExp of id list * exp
  | DFunExp of id list * exp
  | AppExp of exp * exp
  | MatchExp of exp * (cond * exp) list

type program = 
    Exp of exp
  | Decl of (id list * exp) list * program option
  | RecDecl of (id * id list * exp) list;;
