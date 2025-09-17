%{
  open Ast
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
%nonassoc LBRACK (*RBRACK*)
%nonassoc DOT

(* Start symbol *)
%start program
%type <loc Ast.program> program

%%

program:
  | functions=list(decl) EOF { (functions, $loc) }

%inline
struct_field:
  | id=IDENTIFIER COLON typ=rtype { (id, typ, $loc) }

%inline 
decl_struct:
  | STRUCT id=IDENTIFIER LBRACE flist=separated_list(COMMA, struct_field) RBRACE
    { DeclStruct ({name=id; fields=flist}, $loc) }

%inline 
decl_fun:
  | FN id=IDENTIFIER LPAREN params=separated_list(COMMA, param) RPAREN 
    return_typ = option(preceded(ARROW, rtype))
    body=block
    { DeclFun ({name=id; params=params; rtype=return_typ; body=body}, $loc) }

decl:
  | decl_fun | decl_struct { $1 }

%inline 
param:
  | is_mut=option(MUT) name=IDENTIFIER COLON typ=rtype { (Option.is_some is_mut, name, typ, $loc) }

rtype:
  | id=IDENTIFIER {TyBasic (id, $loc)}
  | id=IDENTIFIER LT typ=rtype GT { TyTemplate (id, typ, $loc) }
  | REFMUT typ=rtype { TyRefMut (typ, $loc) }
  | REF typ=rtype { TyRef (typ, $loc) }

block_content:
  | first=instr rest=block_ending {
      let (instrs, expr_opt, _) = rest in
      (first :: instrs, expr_opt, $loc)
    }
  | last=expr { ([], Some last, $loc) }
  | SEMICOLON bc=block_ending { bc }

block_ending:
  | { ([], None, $loc) }
  | next=block_content { next }

block:
  | LBRACE RBRACE { ([], None, $loc) }
  | LBRACE bc=block_content RBRACE { bc }
  
%inline
let_field:
  | id=IDENTIFIER COLON exp=expr { (id, exp, $loc) }

stmt:
  | exp=expr SEMICOLON { InstrExpr (exp, $loc) }
  | LET is_mut=option(MUT) id=IDENTIFIER ASSIGN exp=expr SEMICOLON { InstrLetExpr (Option.is_some is_mut, id, exp, $loc) }
  | LET is_mut=option(MUT) id=IDENTIFIER ASSIGN
    id_struct=IDENTIFIER LBRACE fields=separated_list(COMMA, let_field) RBRACE SEMICOLON
    { InstrLetStruct (Option.is_some is_mut, id, id_struct, fields, $loc) }
  | RETURN exp=option(expr) SEMICOLON { InstrReturn (exp, $loc) }

instr:
  | s=stmt { s }
  | WHILE exp=expr body=block { InstrWhile (exp, body, $loc) }
  | nif=rif { InstrIf (nif, $loc) }

rif:
  | IF exp=expr then_body=block { IfElse (exp, then_body, None, $loc) }
  | IF exp=expr then_body=block ELSE else_body=block { IfElse (exp, then_body, Some else_body, $loc) }
  | IF exp=expr then_body=block ELSE nif=rif { IfElif (exp, then_body, nif, $loc) }

%inline
binop: 
  | EQ      { OpEQ      }
  | ASSIGN  { OpASSIGN  }
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
  | i=INTEGER { ExprInt (i, $loc)}
  | TRUE { ExprBool (true, $loc) }
  | FALSE { ExprBool (false, $loc) }
  | id=IDENTIFIER { ExprIdent (id, $loc) }
  | exp_l=expr op=binop exp_r=expr { ExprBinop (exp_l, op, exp_r, $loc) }
  | op=unop exp=expr { ExprUnop (op, exp, $loc) }
  | MINUS exp=expr %prec UMINUS { ExprUnop (OpMINUS, exp, $loc) }
  | STAR exp=expr %prec USTAR { ExprUnop (OpSTAR, exp, $loc) }
  | exp=expr DOT id=IDENTIFIER { ExprField (exp, id, $loc) }
  | exp=expr DOT len_id=IDENTIFIER LPAREN RPAREN { if len_id = "len" then 
    ExprLen (exp, $loc) else raise (SpecialError "Only .len() method is implemented") }
  | exp1=expr LBRACK exp2=expr RBRACK { ExprBrack (exp1, exp2, $loc) }
  | id=IDENTIFIER LPAREN args=separated_list(COMMA, expr) RPAREN { ExprCall (id, args, $loc)}
  | id=IDENTIFIER NOT LBRACK args=separated_list(COMMA, expr) RBRACK 
    { if id = "vec" then ExprVec (args, $loc) else raise (SpecialError "Only vec! bracket macro is implemented") }
  | id=IDENTIFIER NOT LPAREN str=STRING RPAREN 
    { if id = "print" then ExprPrint (str, $loc) else raise (SpecialError "Only print! macro is implemented") }
  | body=block { ExprBlock (body, $loc) }
  | LPAREN exp=expr RPAREN { exp }
