{
  open Parser
  exception LexError of string
 (*  
  let next_line lexbuf = 
    lexbuf

    let pos = lexbuf.Lexing.lex_curr_pos in
    lexbuf.Lexing.lex_curr_pos <- 
      { pos with pos_lnum = pos.pos_lnum + 1; 
                 pos_bol = pos.pos_cnum }
    *)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = alpha | digit | '_'
let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let integer = digit+
let identifier = alpha alnum*
let string_literal = '"' [^ '"']* '"'

rule token = parse
  | whitespace+ { token lexbuf }
  | newline { (* next_line lexbuf;*) token lexbuf }
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
  
  | eof { EOF }
  | _ as c { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | "*/" { token lexbuf }
  | newline { (* next_line lexbuf; *) comment lexbuf }
  | _ { comment lexbuf }
  | eof { raise (LexError "Unterminated comment") }