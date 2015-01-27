open Syntax
open Eval
open Typing

let unlines strlist = List.fold_right (fun str1 str2 -> str1 ^ "\n" ^ str2) strlist ""

let rpl_comm str =
  let rec search_comm_end str start =
    let ast = String.index_from str (start + 1) '*'
    and r_bra = String.index_from str (start + 1) ')' in
    if r_bra = ast + 1
    then r_bra
    else search_comm_end str (ast + 1) in
  let rec rpl s i =
    if i < String.length s
    then
      try
	if s.[i] = '(' && s.[i + 1] = '*'
	then
	  let ireko = rpl s (i + 2) in
	  let end_comm = search_comm_end ireko (i + 2) in
	  String.fill ireko i (end_comm - i + 1) ' ';
	  rpl ireko (end_comm + 1)
	else rpl s (i + 1)
      with Invalid_argument msg -> s
    else s in
  rpl str 0

let rec read_eval_print env tyenv file =
  print_string "# ";
  flush stdout;
  try
    let buf =
      if file = ""
      then Lexing.from_channel stdin
      else
	let rec read f =
	  try
	    let line = input_line f in
	    line :: read f
          with
	      End_of_file ->
		if file <> ""
		then close_in f; [] in
	Lexing.from_string (rpl_comm (unlines (read (open_in file)))) in
    let decl = Parser.toplevel Lexer.main buf in
    let (newtyenv, tylist) = ty_decl tyenv decl in
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
    read_eval_print newenv newtyenv ""
  with Parsing.Parse_error ->
         print_string "parse error.";
         print_newline();
         read_eval_print env tyenv ""
    |  Eval.Error s ->
         print_string s;
         print_newline();
         read_eval_print env tyenv ""
    |  Typing.Error s ->
         print_string s;
         print_newline ();
	 read_eval_print env tyenv ""

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

let _ =
  let input = try Sys.argv.(1) with Invalid_argument _ -> "" in
  read_eval_print initial_env initial_tyenv input

(*
  let makefact = fun maker -> fun x -> if x < 1 then 1 else x * maker maker (x + -1) in
  let fact = fun x -> makefact makefact x in
  fact 5
*)
