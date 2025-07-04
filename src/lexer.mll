{
  open Parser
  exception LexError of string
 
  let pos_lnum = ref 0;;

  let next_line () = 
    pos_lnum := !pos_lnum + 1; 
		()
  
	let parse_string_literal s =
		let len = String.length s in
		if len < 2 || s.[0] <> '"' || s.[len-1] <> '"' then
			failwith "Invalid string literal: missing quotes"
		else
			let content = String.sub s 1 (len - 2) in
			let buf = Buffer.create (String.length content) in
			let rec loop i =
				if i >= String.length content then
					Buffer.contents buf
				else
					match content.[i] with
					| '\\' when i + 1 < String.length content ->
							(match content.[i + 1] with
							| '\\' -> Buffer.add_char buf '\\'; loop (i + 2)
							| '"' -> Buffer.add_char buf '"'; loop (i + 2)
							| 'n' -> Buffer.add_char buf '\n'; loop (i + 2)
							| _   -> failwith "Invalid escape sequence: backslash at end of string")
					| '\\' ->
							failwith "Invalid escape sequence: backslash at end of string"
					| c ->
							Buffer.add_char buf c; loop (i + 1)
			in
			loop 0
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit | '_'
let whitespace = [' ' '\t']
let newline = "\r\n" | '\r' | '\n'
let integer = digit+
let identifier = alpha alnum*
let string_literal = '"' ([^ '"' '\\'] | '\\' ['"' '\\' 'n'])* '"'

rule token = parse
  | whitespace+ { token lexbuf }
  | newline { next_line (); token lexbuf }
  | "//" [^ '\n']* { token lexbuf }  (* Single line comments *)
  | "/*" { comment lexbuf }          (* Multi-line comments *)
  
  (* Keywords *)
  | "fn" { FN }
  | "let" { LET }
  | "mut" { MUT }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "return" { RETURN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "struct" { STRUCT }
  
  (* Operators *)
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { STAR }
  | "/"    { SLASH }
  | "%"    { PERCENT }
  | "=="   { EQ }
  | "!="   { NE }
  | "<"    { LT }
  | "<="   { LE }
  | ">"    { GT }
  | ">="   { GE }
  | "&&"   { AND }
  | "||"   { OR }
  | "!"    { NOT }
  | "="    { ASSIGN }
  | "."    { DOT }
  | "&mut" { REFMUT }
  | "&"    { REF }
  
  (* Delimiters *)
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "{"  { LBRACE }
  | "}"  { RBRACE }
  | "["  { LBRACK }
  | "]"  { RBRACK }
  | ";"  { SEMICOLON }
  | ","  { COMMA }
  | ":"  { COLON }
  | "->" { ARROW }
  
  (* Literals *)
  | integer as i { INTEGER (int_of_string i) }
  | identifier as id { IDENTIFIER id }
  | string_literal as str { STRING (parse_string_literal str) }
  | eof { EOF }
  | _ as c { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | "*/" { token lexbuf }
  | newline { next_line (); comment lexbuf }
  | _ { comment lexbuf }
  | eof { raise (LexError "Unterminated comment") }