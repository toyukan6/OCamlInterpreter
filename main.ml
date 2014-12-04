open Syntax
open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (list, newenv) = eval_decl env decl in
    let rec print_value = function
        [] -> ()
      | (id, v) :: rest ->
	Printf.printf "val %s = " id;
	pp_val v;
	print_newline();
	print_value rest
    in print_value list;
       read_eval_print newenv
  with Parsing.Parse_error ->
         print_string "parse error.";
         print_newline();
         read_eval_print env
    |  Eval.Error s ->
         print_string s;
         print_newline();
         read_eval_print env

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
				 (Environment.extend "(*)" (ProcV ("a", FunExp (["b"], BinOp (Mult, (Var "a"), (Var "b"))), ref Environment.empty)) Environment.empty))))))))))

let _ = read_eval_print initial_env
