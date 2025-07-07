%{
  open Ast

  let detailed_syntax_error (pos: Lexing.position) msg =
    let line = pos.pos_lnum in
    let col = pos.pos_cnum - pos.pos_bol + 1 in
    let filename = pos.pos_fname in
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" 
        filename line col col;
    Printf.eprintf "Error: %s\n" msg;
    flush stderr

%}

(* Token declarations *)
%token <int> INTEGER
%token <string> IDENTIFIER
%token <string> STRING
%token TRUE FALSE
%token FN LET MUT IF ELSE WHILE RETURN STRUCT
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NE LT LE GT GE
%token AND OR NOT
%token ASSIGN DOT REFMUT REF
%token LPAREN RPAREN LBRACE RBRACE RBRACK LBRACK
%token SEMICOLON COMMA COLON ARROW
%token EOF

(* Precedence and associativity *)
%right ASSIGN
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT USTAR UMINUS REF REFMUT
%nonassoc LBRACK RBRACK
%nonassoc DOT

(* Start symbol *)
%start program
%type <Ast.program> program

%%

program:
  | functions=list(decl) EOF { functions }
  | error {
    detailed_syntax_error $startpos($1) "Expected expression";
    failwith "Parse error"
  }

%inline
struct_field:
  | id=IDENTIFIER COLON typ=rtype { (id, typ) }

%inline 
decl_struct:
  | STRUCT id=IDENTIFIER LBRACE flist=separated_list(COMMA, struct_field) RBRACE
    { DeclStruct {name=id; fields=flist} }

%inline 
decl_fun:
  | FN id=IDENTIFIER LPAREN params=separated_list(COMMA, param) RPAREN 
    return_typ = option(preceded(ARROW, rtype))
    body=block
    { DeclFun {name=id; params=params; rtype=return_typ; body=body} }

decl:
  | decl_fun | decl_struct { $1 }

%inline 
param:
  | is_mut=option(MUT) name=IDENTIFIER COLON typ=rtype { (Option.is_some is_mut, name, typ) }

rtype:
  | id=IDENTIFIER {TyBasic id}
  | id=IDENTIFIER LT typ=rtype GT { TyTemplate (id, typ) }
  | REFMUT typ=rtype { TyRefMut typ }
  | REF typ=rtype { TyRef typ }

%inline
block:
  | LBRACE instrs=list(instr) exp=option(expr) RBRACE { (List.filter_map  (fun a -> a) instrs, exp) }

%inline
let_field:
  | id=IDENTIFIER COLON exp=expr { (id, exp) }


instr:
  | SEMICOLON { None }
  | exp=expr SEMICOLON { Some (InstrExpr exp) }
  | LET is_mut=option(MUT) IDENTIFIER ASSIGN exp=expr SEMICOLON { Some (InstrLetExpr (Option.is_some is_mut, exp)) }
  | LET is_mut=option(MUT) IDENTIFIER ASSIGN 
    id=IDENTIFIER LBRACE fields=separated_list(COMMA, let_field) RBRACE SEMICOLON
    { Some (InstrLetStruct (Option.is_some is_mut, id, fields)) }
  | WHILE exp=expr body=block { Some (InstrWhile (exp, body)) }
  | RETURN exp=option(expr) SEMICOLON { Some (InstrReturn exp) }
  | nif=rif { Some (InstrIf nif) }

rif:
  | IF exp=expr then_body=block { IfElse (exp, then_body, None) }
  | IF exp=expr then_body=block ELSE else_body=block { IfElse (exp, then_body, Some else_body) }
  | IF exp=expr then_body=block ELSE nif=rif { IfElif (exp, then_body, nif) }

%inline
binop: 
  | EQ      { OpEQ      }
  | NE      { OpNE      }
  | LT      { OpLT      }
  | GT      { OpGT      }
  | LE      { OpLE      }
  | GE      { OpGE      }
  | AND     { OpAND     }
  | OR      { OpOR      }
  | PLUS    { OpPLUS    }
  | MINUS   { OpMINUS   }
  | STAR    { OpSTAR    }
  | SLASH   { OpSLASH   }
  | PERCENT { OpPERCENT }

%inline
unop: 
  | NOT    { OpNOT }
  | REF    { OpREF }
  | REFMUT { OpREFMUT }

expr:
  | i=INTEGER { ExprInt i}
  | TRUE { ExprBool true }
  | FALSE { ExprBool false }
  | id=IDENTIFIER { ExprIdent id }
  | exp_l=expr op=binop exp_r=expr { ExprBinop (exp_l, op, exp_r) }
  | op=unop exp=expr { ExprUnop (op, exp) }
  | MINUS exp=expr %prec UMINUS { ExprUnop (OpMINUS, exp) }
  | STAR exp=expr %prec USTAR { ExprUnop (OpSTAR, exp) }
  | exp=expr DOT id=IDENTIFIER { ExprField (exp, id) }
  | exp=expr DOT len_id=IDENTIFIER LPAREN RPAREN { if len_id = "len" then 
    ExprLen exp else failwith("TODO: Only .len() method is implemented") }
  | exp1=expr LBRACK exp2=expr RBRACK { ExprBrack (exp1, exp2) }
  | id=IDENTIFIER LPAREN args=separated_list(COMMA, expr) RPAREN { ExprCall (id, args)}
  | id=IDENTIFIER NOT LBRACK args=separated_list(COMMA, expr) RBRACK 
    { if id = "vec" then ExprVec args else failwith("TODO: Only vec! bracket macro is implemented") }
  | id=IDENTIFIER NOT LPAREN str=STRING RPAREN 
    { if id = "print" then ExprPrint str else failwith("TODO: Only print! call macro is implemented") }
  | body=block { ExprBlock body }
  | LPAREN exp=expr RPAREN { exp }
