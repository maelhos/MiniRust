open Minirust
open Lexing

let usage_msg = "Usage: minirust <file>"
let input_file = ref ""

let set_input_file filename = input_file := filename

let spec_list = []

let syntax_error lexbuf mess =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  let filename = pos.pos_fname in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" 
    filename line col col;
  Printf.eprintf "Error: %s\n" mess;
  flush stderr


let compile_file filename = 
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    begin try

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
    | Lexer.LexError msg -> 
        Printf.eprintf "Lexer error: %s\n" msg; exit 1
    | Parser.Error -> 
      syntax_error lexbuf "Parser error\n"; exit 1
    | _ -> 
        Printf.eprintf "Syntax error\n"; exit 1
    end
  with
  | Sys_error msg -> 
    Printf.eprintf "Sys error: %s\n" msg; exit 1
  | _ -> 
      Printf.eprintf "Unexpected error\n"; exit 1


let () = 
  Arg.parse spec_list set_input_file usage_msg;
  if !input_file = "" then begin
    Printf.eprintf "%s\n" usage_msg;
    exit 1
  end;
  compile_file !input_file