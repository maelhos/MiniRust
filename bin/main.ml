open Minirust
open Lexing
open Anot

let usage_msg = "Usage: minirust <file>"
let input_files = ref []
let parse_only = ref false
let type_only = ref false
let no_asm = ref false
let set_input_file filename = input_files := filename :: !input_files

let speclist =
  [
    ("--parse-only", Arg.Set parse_only, "Only go as far as parsing");
    ("--type-only", Arg.Set type_only, "Only go as far as typing");
    ("--no-asm", Arg.Set no_asm, "Do not generated assembly");
  ]

let error err_type loc ic mess =
  let pos1, pos2 = loc in
  let line1 = pos1.pos_lnum in
  let col1 = pos1.pos_cnum - pos1.pos_bol + 1 in
  let col2 = pos2.pos_cnum - pos1.pos_bol + 1 in
  let filename = pos1.pos_fname in
  seek_in ic 0;
  for i = 1 to line1 - 1 do
    ignore (input_line ic)
  done;
  Printf.eprintf "\x1B[1;97mFile \"%s\", line %d, characters %d-%d:\x1B[0m\n"
    filename line1 col1 col2;
  let prolog = Printf.sprintf "%d | " line1 in
  Printf.eprintf "%s%s\n" prolog (input_line ic);
  let spaces = String.make (col1 + String.length prolog - 1) ' ' in
  let errs = String.make (col2 - col1 + 1) '^' in
  Printf.eprintf "%s\x1B[1;31m%s\x1B[0m\n" spaces errs;
  Printf.eprintf "\x1B[1;31mError\x1B[0m: %s %s\n" err_type mess;
  flush stderr

let syntax_error = error "Syntax"
let type_error = error "Type"

let compile_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic ~with_positions:true in
    Lexing.set_filename lexbuf filename;
    (try
       (* Parse the program *)
       let ast = Parser.program Lexer.token lexbuf in
       Printf.printf "Parsed successfully!\n";
       Printf.printf "AST: %s\n" (Ast.show_program Ast.pp_loc ast);
       if not !parse_only then (
         let typed_ast = Semantic.analyze_program ast in
         Printf.printf "Typed successfully!\n";
         Printf.printf "AST: %s\n" (Ast.show_program Anot.pp_anot typed_ast)
         (* TODO: Add code generation *)
         (* let code = Codegen.generate typed_ast in *))
     with
    | Semantic.SemanticError (msg, loc) ->
        type_error loc ic msg;
        exit 1
    | Lexer.LexError msg ->
        syntax_error (lexbuf.lex_start_p, lexbuf.lex_curr_p) ic msg;
        exit 1
    | Ast.SpecialError msg ->
        syntax_error (lexbuf.lex_start_p, lexbuf.lex_curr_p) ic msg;
        exit 1
    | Parser.Error ->
        syntax_error
          (lexbuf.lex_start_p, lexbuf.lex_curr_p)
          ic "Unexpected token";
        exit 1
    | _ ->
        Printf.eprintf "Unknown error\n";
        exit 1);
    close_in ic
  with
  | Sys_error msg ->
      Printf.eprintf "Sys error: %s\n" msg;
      exit 1
  | ex ->
      Printf.eprintf "Unexpected error : %s\n" (Printexc.to_string ex);
      exit 1

let () =
  Arg.parse speclist set_input_file usage_msg;
  if List.is_empty !input_files then (
    Printf.eprintf "%s\n" usage_msg;
    exit 1);
  List.iter compile_file !input_files
