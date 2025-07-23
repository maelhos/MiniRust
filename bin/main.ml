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

let error err_type loc mess =
  let pos1 = fst loc in
  let line1 = pos1.pos_lnum in
  let col1 = pos1.pos_cnum - pos1.pos_bol + 1 in
  let pos2 = snd loc in
  let col2 = pos2.pos_cnum - pos1.pos_bol + 1 in
  let filename = pos1.pos_fname in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" 
    filename line1 col1 col2;
  Printf.eprintf "%d | %s error: %s\n" line1 err_type mess;
  flush stderr

let syntax_error = error "Syntax"
let type_error = error "Type"


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

      if not !parse_only then begin
        let typed_ast = Semantic.analyze ast in

        Printf.printf "Typed successfully!\n";
        Printf.printf "AST: %s\n" (Ast.show_program typed_ast)

        (* TODO: Add code generation *)
        (* let code = Codegen.generate typed_ast in *)
    end
    with
    | Semantic.SemanticError (msg, loc) ->
      type_error loc msg; exit 1
    | Lexer.LexError msg -> 
      syntax_error (lexbuf.lex_curr_p, lexbuf.lex_curr_p) msg; exit 1
    | Parser.Error -> 
      syntax_error (lexbuf.lex_curr_p, lexbuf.lex_curr_p) "Unexpected token"; exit 1
    | _ -> 
        Printf.eprintf "Unknown error\n"; exit 1
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