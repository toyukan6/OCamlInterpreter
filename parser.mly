%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT ANDB ORB
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND
%token RARROW FUN

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

Decl :
    ID EQ Expr AND Decl { ($1, $3) :: $5 }
  | ID EQ Expr { [($1, $3)] }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | ORExpr { $1 }
  | FunExpr { $1 }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) }

LetExpr :
    LET Decl IN Expr { LetExp ($2, $4) }
      
ORExpr :
    ANDExpr ORB ANDExpr { BinOp (Or, $1, $3) }
  | ANDExpr { $1 }

ANDExpr :
    LTExpr ANDB LTExpr { BinOp (And, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
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
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

   
