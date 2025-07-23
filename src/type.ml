
open Ast

type typ = 
  | I32 | Bool | Unit | Str
  | Struct of string * (string * typ) list
  | Vec of typ
  | Ref of typ
  | MutRef of typ
  [@@deriving show]

(* Typed AST *)
type full_typ = typ * bool * bool [@@deriving show] (* type, left value, mut *)
let extract_typ (t, _, _: full_typ) = t

type styp_expr =
  | TypExprInt   of int 
  | TypExprBool  of bool 
  | TypExprIdent of string 
  | TypExprBinop of typ_expr * op * typ_expr 
  | TypExprUnop  of op * typ_expr 
  | TypExprField of typ_expr * string 
  | TypExprLen   of typ_expr 
  | TypExprBrack of typ_expr * typ_expr 
  | TypExprCall  of string * typ_expr list 
  | TypExprVec   of typ_expr list 
  | TypExprPrint of string 
  | TypExprBlock of typ_block 
  [@@deriving show]
and typ_expr = full_typ * styp_instr [@@deriving show]

and typ_let_field = full_typ * string * expr * loc [@@deriving show]

and styp_instr = 
  | TypInstrExpr      of typ_expr 
  | TypInstrLetExpr   of bool * typ_expr 
  | TypInstrLetStruct of bool * string * typ_let_field list 
  | TypInstrWhile     of typ_expr * typ_block 
  | TypInstrReturn    of typ_expr option 
  | TypInstrIf        of typ_instr_if 
  [@@deriving show]
and typ_instr = full_typ * styp_instr [@@deriving show]

and styp_instr_if = 
  | TypIfElse of typ_expr * typ_block * typ_block option 
  | TypIfElif of typ_expr * typ_block * typ_instr_if 
  [@@deriving show]
and typ_instr_if = full_typ * styp_instr_if [@@deriving show]

and styp_ty = 
  | TypTyBasic    of string 
  | TypTyTemplate of string * typ_ty 
  | TypTyRefMut   of typ_ty 
  | TypTyRef      of typ_ty 
  [@@deriving show]
and typ_ty = full_typ * styp_ty [@@deriving show]


and typ_field = full_typ * string * typ_ty  [@@deriving show]
and typ_param = full_typ * bool * string * typ_ty  [@@deriving show]
and typ_block = full_typ * typ_instr list * typ_expr option  [@@deriving show]

type typ_struct_decl = {
  name: string;
  fields: typ_field list;
} [@@deriving show]

type typ_fun_decl = {
  name: string;
  params: param list;
  rtype: ty option;
  body: block;
} [@@deriving show]

type styp_decl = 
  | TypDeclStruct of typ_struct_decl 
  | TypDeclFun    of typ_fun_decl 
  [@@deriving show]
type typ_decl = full_typ * styp_decl [@@deriving show]

type typ_program = typ_decl list [@@deriving show];;