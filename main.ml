open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (tylist, newtyenv) = ty_decl tyenv decl in
    let (list, newenv) = eval_decl env decl in
    let rec print_value = function
        ([], []) -> ()
      | ((id, v) :: rest, (TyScheme (_, ty)) :: trest) ->
	Printf.printf "val %s : " id;
	pp_ty ty;
	print_string " = ";
	pp_val v;
	print_value (rest, trest)
      | _ -> print_string "something wrong"
    in print_value (list, tylist);
       read_eval_print newenv newtyenv
  with Parsing.Parse_error ->
         print_string "parse error.";
         print_newline();
         read_eval_print env tyenv
    |  Eval.Error s ->
         print_string s;
         print_newline();
         read_eval_print env tyenv
    |  Typing.Error s ->
         print_string s;
         print_newline ();
	 read_eval_print env tyenv

let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
       (Environment.extend "x" (IntV 10)
           (Environment.extend "ii" (IntV 2)
               (Environment.extend "iii" (IntV 3)
		  (Environment.extend "iv" (IntV 4)
		     (Environment.extend "(+)" (ProcV ("a", FunExp (["b"], BinOp (Plus, (Var "a"), (Var "b"))), ref Environment.empty))
			(Environment.extend "(<)" (ProcV ("a", FunExp (["b"], BinOp (Lt, (Var "a"), (Var "b"))), ref Environment.empty))
			   (Environment.extend "(&&)" (ProcV ("a", FunExp (["b"], BinOp (And, (Var "a"), (Var "b"))), ref Environment.empty))
			      (Environment.extend "(||)" (ProcV ("a", FunExp (["b"], BinOp (Or, (Var "a"), (Var "b"))), ref Environment.empty))
				 (Environment.extend "(::)" (ProcV ("a", FunExp (["b"], BinOp (Cons, (Var "a"), (Var "b"))), ref Environment.empty))
				    (Environment.extend "(*)" (ProcV ("a", FunExp (["b"], BinOp (Mult, (Var "a"), (Var "b"))), ref Environment.empty)) Environment.empty)))))))))))

let initial_tyvar' = fresh_tyvar ()
let initial_tyvar = TyVar initial_tyvar'

let initial_tyenv = 
  Environment.extend "i" (TyScheme ([], TyInt))
    (Environment.extend "v"  (TyScheme ([], TyInt))
       (Environment.extend "x"  (TyScheme ([], TyInt))
           (Environment.extend "ii" (TyScheme ([], TyInt))
               (Environment.extend "iii" (TyScheme ([], TyInt))
		  (Environment.extend "iv" (TyScheme ([], TyInt))
		     (Environment.extend "(+)" (TyScheme ([], TyFun (TyInt, TyFun (TyInt, TyInt))))
			(Environment.extend "(<)" (TyScheme ([], TyFun (TyInt, TyFun (TyInt, TyBool))))
			   (Environment.extend "(&&)" (TyScheme ([], TyFun (TyBool, TyFun (TyBool, TyBool))))
			      (Environment.extend "(||)" (TyScheme ([], TyFun (TyBool, TyFun (TyBool, TyBool))))
				 (Environment.extend "(::)" (TyScheme ([initial_tyvar'], TyFun (initial_tyvar, TyFun (TyList initial_tyvar, TyList initial_tyvar))))
				    (Environment.extend "(*)" (TyScheme ([], TyFun (TyInt, TyFun (TyInt, TyInt)))) Environment.empty)))))))))))

let _ = read_eval_print initial_env initial_tyenv
