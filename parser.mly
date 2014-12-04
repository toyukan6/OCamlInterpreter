%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI LBRACKET RBRACKET SEMI
%token PLUS MULT LT ANDB ORB CONS
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND REC
%token RARROW FUN DFUN
%token MATCH WITH GUARD

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LetDecl SEMISEMI { $1 }

LetDecl :
    LET Decl LetDecl { Decl ($2, Some $3) }
  | LET Decl { Decl ($2, None) }
  | LET REC RecDecl { RecDecl ($3) }

RecDecl :
    ID IDList EQ Expr AND RecDecl { ($1, $2, $4) :: $6 }
  | ID IDList EQ Expr { [($1, $2, $4)] }

Decl :
    IDList EQ Expr AND Decl { ($1, $3) :: $5 }
  | IDList EQ Expr { [($1, $3)] }

Expr :
    ORExpr { $1 }

FunExpr :
    FUN IDList RARROW Expr { FunExp ($2, $4) }
  | DFUN IDList RARROW Expr { DFunExp ($2, $4) }

IDList :
    ID IDList { $1 :: $2 }
  | ID { [$1] }

LetExpr :
    LET Decl IN Expr { LetExp ($2, $4) }
  | LET REC RecDecl IN Expr { LetRecExp ($3, $5) }
      
ORExpr :
    ANDExpr ORB ANDExpr { BinOp (Or, $1, $3) }
  | ANDExpr { $1 }

ANDExpr :
    LTExpr ANDB LTExpr { BinOp (And, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    CExpr LT PExpr { BinOp (Lt, $1, $3) }
  | CExpr { $1 }

CExpr :
    PExpr CONS CExpr { BinOp (Cons, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LBRACKET RBRACKET { LLit [] }
  | LBRACKET List RBRACKET { LLit $2 }
  | LPAREN Operator RPAREN { Var ("(" ^ $2 ^ ")") }
  | LPAREN Expr RPAREN { $2 }
  | IfExpr { $1 }
  | LetExpr { $1 }
  | FunExpr { $1 }
  | MatchExpr { $1 }

List :
    Expr SEMI List { $1 :: $3 }
  | Expr { [$1] }

Operator :
    PLUS { "+" }
  | MULT { "*" }
  | LT   { "<" }
  | ANDB { "&&" }
  | ORB  { "||" }
  | CONS { "::" }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

MatchExpr :
    MATCH Expr WITH Pattern { MatchExp ($2, $4) }

Pattern :
    Condition RARROW Expr GUARD Pattern { ($1, $3) :: $5 }
  | Condition RARROW Expr { [($1, $3)] }

Condition :
    INTV { IntCond $1 }
  | TRUE { BoolCond true }
  | FALSE { BoolCond false }
  | ID { VarCond $1 }
  | ListCondition { ListCond $1 }
  | LPAREN ListCondition RPAREN { ListCond $2 }
  | LBRACKET RBRACKET { NullListCond }

ListCondition :
    Condition CONS ListCond { $1 :: $3 }
  | Condition { [$1] }
