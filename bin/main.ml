open Minirust

let usage_msg = "Usage: minirust <file>"
let input_file = ref ""

let set_input_file filename = input_file := filename

let spec_list = []

let compile_file filename = 
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    (* Parse the program *)
    let ast = Parser.program Lexer.token lexbuf in
    close_in ic;
    
    Printf.printf "Parsed successfully!\n";
    Printf.printf "AST: %s\n" (Ast.show_program ast);
    
    (* TODO: Add semantic analysis *)
    (* let typed_ast = Semantic.analyze ast in *)
    
    (* TODO: Add code generation *)
    (* let code = Codegen.generate typed_ast in *)
  with
  | Sys_error msg -> 
      Printf.eprintf "Error: %s\n" msg; exit 1
  | Lexer.LexError msg -> 
      Printf.eprintf "Lexer error: %s\n" msg; exit 1
  | Parser.Error -> 
      Printf.eprintf "Parser error\n"; exit 1
  | _ -> 
      Printf.eprintf "Syntax error\n"; exit 1

let () = 
  Arg.parse spec_list set_input_file usage_msg;
  if !input_file = "" then begin
    Printf.eprintf "%s\n" usage_msg;
    exit 1
  end;
  compile_file !input_file