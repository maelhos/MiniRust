open Minirust
open Lexing

let usage_msg = "Usage: minirust <file>"
let input_files = ref []
let parse_only = ref false
let type_only = ref false
let no_asm = ref false


let set_input_file filename = input_files := filename::!input_files

let speclist =
  [("--parse-only", Arg.Set parse_only, "Only go as far as parsing");
  ("--type-only", Arg.Set type_only, "Only go as far as typing");
  ("--no-asm", Arg.Set no_asm, "Do not generated assembly")]

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
    let lexbuf = Lexing.from_channel ic ~with_positions:true in
    Lexing.set_filename lexbuf filename;
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

    Arg.parse speclist set_input_file usage_msg;
  if List .is_empty !input_files then begin
    Printf.eprintf "%s\n" usage_msg;
    exit 1
  end;
  List.iter compile_file (!input_files)