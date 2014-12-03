(* ML interpreter / type reconstruction *)
type id = string

type comment = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp
  | FunExp of id * exp
  | AppExp of exp * exp

type program = 
    Exp of exp
  | Decl of (id * exp) list * program option
