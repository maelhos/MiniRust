module Lexing = struct
  include Lexing

  let pp_position fmt pos =
    Format.fprintf fmt
      "{ pos_fname = %S; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }"
      pos.pos_fname pos.pos_lnum pos.pos_bol pos.pos_cnum

  let show_position pos = Format.asprintf "%a" pp_position pos
end

type loc = (Lexing.position * Lexing.position[@opaque]) [@@deriving show]

exception SpecialError of string

type op =
  | OpEQ
  | OpNE
  | OpLT
  | OpGT
  | OpLE
  | OpGE
  | OpAND
  | OpOR
  | OpPLUS
  | OpMINUS
  | OpSTAR
  | OpSLASH
  | OpPERCENT
  | OpNOT
  | OpREF
  | OpREFMUT
  | OpASSIGN
[@@deriving show]

let string_of_op (o : op) : string =
  match o with
  | OpEQ -> "=="
  | OpNE -> "!="
  | OpLT -> "<"
  | OpGT -> ">"
  | OpLE -> "<="
  | OpGE -> ">="
  | OpAND -> "&&"
  | OpOR -> "||"
  | OpPLUS -> "+"
  | OpMINUS -> "-"
  | OpSTAR -> "*"
  | OpSLASH -> "/"
  | OpPERCENT -> "%"
  | OpNOT -> "!"
  | OpREF -> "&"
  | OpREFMUT -> "&mut"
  | OpASSIGN -> "="

type 'a expr =
  | ExprInt of int * 'a
  | ExprBool of bool * 'a
  | ExprIdent of string * 'a
  | ExprBinop of 'a expr * op * 'a expr * 'a
  | ExprUnop of op * 'a expr * 'a
  | ExprField of 'a expr * string * 'a
  | ExprLen of 'a expr * 'a
  | ExprBrack of 'a expr * 'a expr * 'a
  | ExprCall of string * 'a expr list * 'a
  | ExprVec of 'a expr list * 'a
  | ExprPrint of string * 'a
  | ExprBlock of 'a block * 'a
[@@deriving show]

and 'a let_field = string * 'a expr * 'a [@@deriving show]

and 'a instr =
  | InstrExpr of 'a expr * 'a
  | InstrLetExpr of bool * string * 'a expr * 'a
  | InstrLetStruct of bool * string * string * 'a let_field list * 'a
  | InstrWhile of 'a expr * 'a block * 'a
  | InstrReturn of 'a expr option * 'a
  | InstrIf of 'a instr_if * 'a
[@@deriving show]

and 'a instr_if =
  | IfElse of 'a expr * 'a block * 'a block option * 'a
  | IfElif of 'a expr * 'a block * 'a instr_if * 'a
[@@deriving show]

and 'a ty =
  | TyBasic of string * 'a
  | TyTemplate of string * 'a ty * 'a
  | TyRefMut of 'a ty * 'a
  | TyRef of 'a ty * 'a
[@@deriving show]

and 'a field = string * 'a ty * 'a [@@deriving show]
and 'a param = bool * string * 'a ty * 'a [@@deriving show]
and 'a block = 'a instr list * 'a expr option * 'a [@@deriving show]

type 'a struct_decl = {name: string; fields: 'a field list} [@@deriving show]

type 'a fun_decl = {
  name: string;
  params: 'a param list;
  rtype: 'a ty option;
  body: 'a block;
}
[@@deriving show]

type 'a decl = DeclStruct of 'a struct_decl * 'a | DeclFun of 'a fun_decl * 'a
[@@deriving show]

type 'a program = 'a decl list * 'a [@@deriving show]
