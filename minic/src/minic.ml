let () =
  let file = Sys.argv.(1) in
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  (* Affichage de l'Ast *)
  Printf.printf "Successfully parsed program %s\n" file;
  Printf.printf 
  "===============================================================================\n";
  Printm.print ast;
  Printf.printf
  "===============================================================================\n";
  (* Type checker*)
  Minic_typechecker.typecheck_program ast;
  Printf.printf "Successfully checked program %s\n" file;
  
  (*return value*)
  let return_value = Minic_interpreteur_imp.exec.program ast in
  Printf.sprintf "Return value of program is %s" return_value;
  exit 0
