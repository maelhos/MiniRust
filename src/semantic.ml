open Ast
open Type
open Anot

exception SemanticError of string * loc

module Smap = Map.Make (String)

type env = {
  vars: anot Smap.t;
  structs: typ Smap.t Smap.t;
  funs: (typ list * typ) Smap.t;
  curr_fun_rt: typ option;
}

let name_taken (name : string) (ev : env) : bool =
  Smap.mem name ev.vars || Smap.mem name ev.structs || Smap.mem name ev.funs

let rec analyse_ty (p : loc ty) (ev : env) : anot ty * typ =
  match p with
  | TyBasic (tname, l) -> (
    match tname with
    | "i32" -> (TyBasic (tname, typ_anot l TypI32), TypI32)
    | "bool" -> (TyBasic (tname, typ_anot l TypBool), TypBool)
    | _ when Smap.mem tname ev.structs ->
      let rt = TypStruct tname in
      (TyBasic (tname, typ_anot l rt), rt)
    | _ -> raise (SemanticError (Printf.sprintf "Unknown type \"%s\"" tname, l))
    )
  | TyRef (t, l) ->
    let at, st = analyse_ty t ev in
    let rt = TypRef st in
    (TyRef (at, typ_anot l rt), rt)
  | TyRefMut (t, l) ->
    let at, st = analyse_ty t ev in
    let rt = TypMutRef st in
    (TyRefMut (at, typ_anot l rt), rt)
  | TyTemplate (tname, t, l) ->
    if tname <> "Vec" then
      raise
        (SemanticError
           ( Printf.sprintf
               "Unknown template type \"%s\" (only Vec is implemented)" tname,
             l ));
    let at, st = analyse_ty t ev in
    let rt = TypVec st in
    (TyTemplate (tname, at, typ_anot l rt), rt)

let rec analyse_expr (p : loc expr) (ev : env) : anot expr * typ =
  match p with
  | ExprInt (v, l) -> (ExprInt (v, typ_anot l TypI32), TypI32)
  | ExprBool (v, l) -> (ExprBool (v, typ_anot l TypBool), TypBool)
  | ExprIdent (name, l) ->
    if Smap.mem name ev.vars then
      raise
        (SemanticError
           (Printf.sprintf "Unknown variable identifier \"%s\"" name, l));
    let an = Smap.find name ev.vars in
    (ExprIdent (name, an), an.ty)
  | ExprBinop (e1, op, e2, l) -> (
    let ae1, t1 = analyse_expr e1 ev in
    let ae2, t2 = analyse_expr e2 ev in
    match op with
    | OpEQ | OpNE | OpLT | OpLE | OpGT | OpGE ->
      if t1 <> TypI32 || t2 <> TypI32 then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                 (string_of_op op),
               l ));
      (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
    | OpPLUS | OpMINUS | OpSTAR | OpSLASH | OpPERCENT ->
      if t1 <> TypI32 || t2 <> TypI32 then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                 (string_of_op op),
               l ));
      (ExprBinop (ae1, op, ae2, typ_anot l TypI32), TypI32)
    | OpAND | OpOR ->
      if t1 <> TypBool || t2 <> TypBool then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect variables of type bool"
                 (string_of_op op),
               l ));
      (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
    | _ ->
      raise
        (SemanticError
           ( Printf.sprintf "Operator \"%s\" is not a binary operator"
               (string_of_op op),
             l )))
  | ExprUnop (op, e, l) -> (
    let ae, t = analyse_expr e ev in
    match op with
    | OpMINUS ->
      if t <> TypI32 then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect variables of type i32"
                 (string_of_op op),
               l ));
      (ExprUnop (op, ae, typ_anot l TypI32), TypI32)
    | OpNOT ->
      if t <> TypBool then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect variables of type bool"
                 (string_of_op op),
               l ));
      (ExprUnop (op, ae, typ_anot l TypBool), TypBool)
    | OpREF ->
      if not (extract_anot_expr ae).lval then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect a l-value"
                 (string_of_op op),
               l ));
      let rt = TypRef t in
      (ExprUnop (op, ae, typ_anot l rt), rt)
    | OpREFMUT ->
      if not (extract_anot_expr ae).lval then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect a l-value"
                 (string_of_op op),
               l ));
      if not (extract_anot_expr ae).mut then
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expect a mutable value"
                 (string_of_op op),
               l ));
      let rt = TypMutRef t in
      (ExprUnop (op, ae, typ_anot l rt), rt)
    | OpSTAR -> (
      match t with
      | TypRef tr ->
        let at = var_anot l tr false in
        (ExprUnop (op, ae, at), tr)
      | TypMutRef tr ->
        let at = var_anot l tr true in
        (ExprUnop (op, ae, at), tr)
      | _ ->
        raise
          (SemanticError
             ( Printf.sprintf "Operator \"%s\" expects a reference type"
                 (string_of_op op),
               l )))
    | _ ->
      raise
        (SemanticError
           ( Printf.sprintf "Operator \"%s\" is not a unary operator"
               (string_of_op op),
             l )))
  | ExprField (e, fname, l) -> (
    let ae, t = analyse_expr e ev in
    match t with
    | TypStruct sname ->
      let struct_mems = Smap.find sname ev.structs in
      if not (Smap.mem sname struct_mems) then
        raise
          (SemanticError
             ( Printf.sprintf "Struct \"%s\" does not have a field named \"%s\""
                 sname fname,
               l ));
      if not (extract_anot_expr ae).mut then
        raise
          (SemanticError
             ( "Operator \".\" expects a l-valued struct or a struct reference",
               l ));
      let rt = Smap.find sname struct_mems in
      (ExprField (ae, fname, var_anot l rt (extract_anot_expr ae).mut), rt)
    | TypRef (TypStruct sname) ->
      let struct_mems = Smap.find sname ev.structs in
      if not (Smap.mem sname struct_mems) then
        raise
          (SemanticError
             ( Printf.sprintf "Struct \"%s\" does not have a field named \"%s\""
                 sname fname,
               l ));
      let rt = Smap.find sname struct_mems in
      (ExprField (ae, fname, var_anot l rt false), rt)
    | TypMutRef (TypStruct sname) ->
      let struct_mems = Smap.find sname ev.structs in
      if not (Smap.mem sname struct_mems) then
        raise
          (SemanticError
             ( Printf.sprintf "Struct \"%s\" does not have a field named \"%s\""
                 sname fname,
               l ));
      let rt = Smap.find sname struct_mems in
      (ExprField (ae, fname, var_anot l rt true), rt)
    | _ ->
      raise
        (SemanticError
           ( "Operator \".\" expects either a struct or a reference to a struct",
             l )))
  | ExprLen (e, l) -> (
    let ae, t = analyse_expr e ev in
    match t with
    | TypVec _ | TypRef (TypVec _) | TypMutRef (TypVec _) ->
      (ExprLen (ae, typ_anot l TypI32), TypI32)
    | _ ->
      raise
        (SemanticError
           ( "Method \".len()\" expects either a Vec<t> or a reference to Vec<t>",
             l )))
  | ExprBrack (e1, e2, l) -> (
    let ae1, t1 = analyse_expr e1 ev in
    let ae2, t2 = analyse_expr e2 ev in
    if t2 <> TypI32 then
      raise
        (SemanticError ("Array-like type can only be indexed by type i32", l));
    match t1 with
    | TypVec ti ->
      if not (extract_anot_expr ae1).lval then
        raise
          (SemanticError
             ( "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>",
               l ));
      (ExprBrack (ae1, ae2, var_anot l ti (extract_anot_expr ae1).mut), ti)
    | TypRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti false), ti)
    | TypMutRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti true), ti)
    | _ ->
      raise
        (SemanticError
           ( "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>",
             l )))
  | ExprCall (fname, el, l) ->
    if not (Smap.mem fname ev.funs) then
      raise
        (SemanticError (Printf.sprintf "Undefined function \"%s\"" fname, l));
    let fun_ts, fun_ret = Smap.find fname ev.funs in
    let aels = List.map (fun e -> analyse_expr e ev) el in
    let arg_ts = List.map snd aels in
    if List.length arg_ts <> List.length el then
      raise
        (SemanticError
           ( Printf.sprintf
               "Function \"%s\" expects %d argument while only %d were provided"
               fname (List.length arg_ts) (List.length el),
             l ));
    List.iter2
      (fun t1 t2 ->
        if not (t1 <= t2) then
          raise
            (SemanticError
               ( Printf.sprintf
                   "Function call for \"%s\" expected argument type %s but got %s"
                   fname (string_of_typ t1) (string_of_typ t2),
                 l )))
      arg_ts fun_ts;
    (ExprCall (fname, List.map fst aels, typ_anot l fun_ret), fun_ret)
  | ExprVec (el, l) ->
    let aels = List.map (fun e -> analyse_expr e ev) el in
    let arg_ts = List.map snd aels in
    if List.is_empty arg_ts then
      raise
        (SemanticError
           ("Macro call for vec! expects a non empty list. TODO: inference", l));
    let ti = List.hd arg_ts in
    if not (List.for_all (fun te -> te = ti) arg_ts) then
      raise
        (SemanticError
           ("Macro call for vec! expects all elements to be of the same type", l));
    let rt = TypVec ti in
    (ExprVec (List.map fst aels, typ_anot l rt), rt)
  | ExprPrint (s, l) -> (ExprPrint (s, default_anot l), TypUnit)
  | ExprBlock (b, l) ->
    let ab, t = analyse_block b ev in
    (ExprBlock (ab, typ_anot l t), t)

and analyse_field_list
    (id_struct : string)
    (ls : loc let_field list)
    (st : typ Smap.t)
    (ev : env)
    (l : loc) : anot let_field list =
  if Smap.cardinal st <> List.length ls then
    raise
      (SemanticError
         ( Printf.sprintf
             "Struct type \"%s\" expects %d fields while only %d were provided"
             id_struct (Smap.cardinal st) (List.length ls),
           l ));
  List.map
    (fun field ->
      let fname, e, l = field in
      if not (Smap.mem fname st) then
        raise
          (SemanticError
             ( Printf.sprintf
                 "Struct type \"%s\" does not expect a field named \"%s\""
                 id_struct fname,
               l ));
      let ae, et = analyse_expr e ev in
      let vt = Smap.find fname st in
      if vt <> et then
        raise
          (SemanticError
             ( Printf.sprintf
                 "Struct type \"%s\" expects field named \"%s\" to be of type %s while %s was provided"
                 id_struct fname (string_of_typ vt) (string_of_typ et),
               l ));
      (fname, ae, typ_anot l vt))
    ls

and analyse_instr_if (p : loc instr_if) (ev : env) : anot instr_if * typ =
  match p with
  | IfElse (ec, bt, None, l) ->
    let aec, tc = analyse_expr ec ev in
    if tc <> TypBool then
      raise
        (SemanticError
           ( Printf.sprintf
               "If confition expects type bool while %s was provided."
               (string_of_typ tc),
             l ));
    let abt, tbt = analyse_block bt ev in
    if tbt <> TypUnit then
      raise
        (SemanticError
           ( Printf.sprintf
               "Then block exepts type unit if no else block, %s was provided."
               (string_of_typ tbt),
             l ));
    (IfElse (aec, abt, None, typ_anot l TypUnit), TypUnit)
  | IfElse (ec, bt, Some be, l) ->
    let aec, tc = analyse_expr ec ev in
    if tc <> TypBool then
      raise
        (SemanticError
           ( Printf.sprintf
               "If confition expects type bool while %s was provided."
               (string_of_typ tc),
             l ));
    let abt, tbt = analyse_block bt ev in
    let abe, tbe = analyse_block be ev in
    if tbt <> tbe then
      raise
        (SemanticError
           ( Printf.sprintf
               "Then and else block exepts the sametype while %s and %s were provided."
               (string_of_typ tbt) (string_of_typ tbe),
             l ));
    (IfElse (aec, abt, Some abe, typ_anot l tbe), tbe)
  | IfElif (ec, bt, ie, l) ->
    let aec, tc = analyse_expr ec ev in
    if tc <> TypBool then
      raise
        (SemanticError
           ( Printf.sprintf
               "If confition expects type bool while %s was provided."
               (string_of_typ tc),
             l ));
    let abt, tbt = analyse_block bt ev in
    let abe, tbe = analyse_instr_if ie ev in
    if tbt <> tbe then
      raise
        (SemanticError
           ( Printf.sprintf
               "Then and else block exepts the sametype while %s and %s were provided."
               (string_of_typ tbt) (string_of_typ tbe),
             l ));
    (IfElif (aec, abt, abe, typ_anot l tbe), tbe)

and analyse_block (p : loc block) (ev : env) : anot block * typ =
  let rec analyse_instr_list (ls : loc instr list) (ev : env) :
      anot instr list * env =
    match ls with
    | InstrExpr (e, l) :: tll ->
      let ae, t = analyse_expr e ev in
      let ail, rev = analyse_instr_list tll ev in
      (InstrExpr (ae, typ_anot l t) :: ail, rev)
    | InstrLetExpr (m, id, e, l) :: tll ->
      let ae, t = analyse_expr e ev in
      let an = var_anot l t m in
      let nev =
        {
          vars= Smap.add id an ev.vars;
          structs= ev.structs;
          funs= ev.funs;
          curr_fun_rt= ev.curr_fun_rt;
        }
      in
      let ail, rev = analyse_instr_list tll nev in
      (InstrLetExpr (m, id, ae, default_anot l) :: ail, rev)
    | InstrLetStruct (m, id, id_struct, fl, l) :: tll ->
      if not (Smap.mem id_struct ev.structs) then
        raise
          (SemanticError
             (Printf.sprintf "No such struct type \"%s\"" id_struct, l));
      let st = Smap.find id_struct ev.structs in
      let ae = analyse_field_list id_struct fl st ev l in
      let ail, rev = analyse_instr_list tll ev in
      (InstrLetStruct (m, id, id_struct, ae, default_anot l) :: ail, rev)
    | InstrWhile (eb, e, l) :: tll ->
      let aeb, tb = analyse_expr eb ev in
      let ae, t = analyse_block e ev in
      if tb <> TypBool then
        raise
          (SemanticError
             ( Printf.sprintf
                 "While condition excepts type bool while %s was provided."
                 (string_of_typ tb),
               l ));
      if t <> TypUnit then
        raise
          (SemanticError
             ( Printf.sprintf
                 "While block excepts type unit while %s was provided."
                 (string_of_typ t),
               l ));
      let ail, rev = analyse_instr_list tll ev in
      (InstrWhile (aeb, ae, default_anot l) :: ail, rev)
    | InstrReturn (eo, l) :: tll -> (
      let rt = Option.get ev.curr_fun_rt in
      match eo with
      | Some e ->
        let ae, t = analyse_expr e ev in
        if t <> rt then
          raise
            (SemanticError
               ( Printf.sprintf
                   "A return in the current function excepts type %s while %s was provided."
                   (string_of_typ rt) (string_of_typ t),
                 l ));
        let ail, rev = analyse_instr_list tll ev in
        (InstrReturn (Some ae, default_anot l) :: ail, rev)
      | None ->
        if rt <> TypUnit then
          raise
            (SemanticError
               ( Printf.sprintf
                   "A return in the current function excepts type %s unit while unit was provided."
                   (string_of_typ rt),
                 l ));

        let ail, rev = analyse_instr_list tll ev in
        (InstrReturn (None, default_anot l) :: ail, rev))
    | InstrIf (ii, l) :: tll ->
      let aii, t = analyse_instr_if ii ev in
      let ail, rev = analyse_instr_list tll ev in
      (InstrIf (aii, typ_anot l t) :: ail, rev)
    | [] -> ([], ev)
  in
  let il, eo, l = p in
  let ail, nev = analyse_instr_list il ev in
  match eo with
  | Some e ->
    let ae, t = analyse_expr e nev in
    ((ail, Some ae, typ_anot l t), t)
  | None -> ((ail, None, typ_anot l TypUnit), TypUnit)

let analyse_decl (p : loc decl) (ev : env) : anot decl * env =
  match p with
  | DeclStruct (sdecl, l) ->
    if name_taken sdecl.name ev then
      raise
        (SemanticError
           (Printf.sprintf "Struct name \"%s\" is already taken" sdecl.name, l));
    let ret_fields =
      List.map (fun (name, ty, l) -> (name, ty, default_anot l)) sdecl.fields
    in
    failwith "todo"
  | DeclFun (fdecl, l) -> failwith "todo"

let analyze_program (p : loc program) : anot program =
  let prog, l = p in
  let rec aux (p : loc decl list) (ev : env) : anot decl list =
    match p with
    | [] -> []
    | h :: t ->
      let nh, nev = analyse_decl h ev in
      nh :: aux t nev
  in
  ( aux prog
      {
        vars= Smap.empty;
        structs= Smap.empty;
        funs= Smap.empty;
        curr_fun_rt= None;
      },
    default_anot l )
