open Ast
open Type
open Anot

exception SemanticError of string * loc

module Smap = Map.Make (String)

type env = {
  vars: anot Smap.t;
  structs: typ Smap.t Smap.t;
  funs: (anot list * typ) Smap.t;
  curr_fun_rt: typ option;
}

let print_smap (pp : 'a -> unit) (l : 'a Smap.t) : unit =
  print_string "[ ";
  List.iter
    (fun (k, v) ->
      print_string k;
      print_string ": ";
      pp v;
      print_char ' ')
    (Smap.to_list l);
  print_char ']'

let print_env (ev : env) : unit =
  print_string "{\n  vars:  ";
  print_smap (fun a -> print_string (show_anot a)) ev.vars;
  print_string "\n  structs:  ";
  print_smap
    (fun ap -> print_smap (fun bp -> print_string (show_typ bp)) ap)
    ev.structs;
  print_string "\n  funs:  ";
  print_smap
    (fun (a, t) ->
      print_char '(';
      List.iter (fun b -> print_string (show_anot b)) a;
      print_string " : ";
      print_string (show_typ t);
      print_char ')')
    ev.funs;
  print_string "\n  curr_fun_rt:  ";
  (match ev.curr_fun_rt with
  | Some t -> print_string (string_of_typ t)
  | None -> print_string "()");
  print_string "\n}\n";
  flush stdout

let err (l : loc) fmt : 'a =
  Printf.ksprintf (fun s -> raise (SemanticError (s, l))) fmt

let check (b : bool) (l : loc) fmt : 'a =
  Printf.ksprintf (fun s -> if not b then raise (SemanticError (s, l))) fmt

let rec analyse_ty (p : loc ty) (ev : env) : anot ty * typ =
  match p with
  | TyBasic (tname, l) -> (
    match tname with
    | "i32" -> (TyBasic (tname, typ_anot l TypI32), TypI32)
    | "bool" -> (TyBasic (tname, typ_anot l TypBool), TypBool)
    | _ when Smap.mem tname ev.structs ->
      let rt = TypStruct tname in
      (TyBasic (tname, typ_anot l rt), rt)
    | _ -> err l "Unknown type \"%s\"" tname)
  | TyRef (t, l) ->
    let at, st = analyse_ty t ev in
    let rt = TypRef st in
    (TyRef (at, typ_anot l rt), rt)
  | TyRefMut (t, l) ->
    let at, st = analyse_ty t ev in
    let rt = TypMutRef st in
    (TyRefMut (at, typ_anot l rt), rt)
  | TyTemplate (tname, t, l) ->
    check (tname = "Vec") l
      "Unknown template type \"%s\" (only Vec is implemented)" tname;
    let at, st = analyse_ty t ev in
    let rt = TypVec st in
    (TyTemplate (tname, at, typ_anot l rt), rt)

let rec analyse_expr (p : loc expr) (ev : env) : anot expr * typ =
  match p with
  | ExprInt (v, l) -> (ExprInt (v, typ_anot l TypI32), TypI32)
  | ExprBool (v, l) -> (ExprBool (v, typ_anot l TypBool), TypBool)
  | ExprIdent (name, l) ->
    check (Smap.mem name ev.vars) l "Unknown variable identifier \"%s\"" name;
    let an = Smap.find name ev.vars in
    (ExprIdent (name, an), an.ty)
  | ExprBinop (e1, op, e2, l) -> (
    let ae1, t1 = analyse_expr e1 ev in
    let ae2, t2 = analyse_expr e2 ev in
    match op with
    | OpEQ | OpNE | OpLT | OpLE | OpGT | OpGE ->
      check
        (t1 = TypI32 && t2 = TypI32)
        l "Operator \"%s\" expects variables of type i32" (string_of_op op);
      (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
    | OpPLUS | OpMINUS | OpSTAR | OpSLASH | OpPERCENT ->
      check
        (t1 = TypI32 && t2 = TypI32)
        l "Operator \"%s\" expects variables of type i32" (string_of_op op);
      (ExprBinop (ae1, op, ae2, typ_anot l TypI32), TypI32)
    | OpAND | OpOR ->
      check
        (t1 = TypBool && t2 = TypBool)
        l "Operator \"%s\" expects variables of type bool" (string_of_op op);
      (ExprBinop (ae1, op, ae2, typ_anot l TypBool), TypBool)
    | OpASSIGN ->
      check (t2 <= t1) l
        "Operator \"%s\" expects both expression to be of same type while %s and %s were provided."
        (string_of_op op) (string_of_typ t1) (string_of_typ t2);
      check
        ((extract_anot_expr ae1).mut && (extract_anot_expr ae1).lval)
        l "Operator \"%s\" expects left expression to be a mutable l-value."
        (string_of_op op);
      (ExprBinop (ae1, op, ae2, default_anot l), TypUnit)
    | _ -> err l "Operator \"%s\" is not a binary operator" (string_of_op op))
  | ExprUnop (op, e, l) -> (
    let ae, t = analyse_expr e ev in
    match op with
    | OpMINUS ->
      check (t = TypI32) l "Operator \"%s\" expect variables of type i32"
        (string_of_op op);
      (ExprUnop (op, ae, typ_anot l TypI32), TypI32)
    | OpNOT ->
      check (t = TypBool) l "Operator \"%s\" expect variables of type bool"
        (string_of_op op);
      (ExprUnop (op, ae, typ_anot l TypBool), TypBool)
    | OpREF ->
      check (extract_anot_expr ae).lval l "Operator \"%s\" expect a l-value"
        (string_of_op op);
      let rt = TypRef t in
      (ExprUnop (op, ae, typ_anot l rt), rt)
    | OpREFMUT ->
      check (extract_anot_expr ae).lval l "Operator \"%s\" expect a l-value"
        (string_of_op op);
      check (extract_anot_expr ae).mut l
        "Operator \"%s\" expect a mutable value" (string_of_op op);
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
      | _ -> err l "Operator \"%s\" expects a reference type" (string_of_op op))
    | _ -> err l "Operator \"%s\" is not a unary operator" (string_of_op op))
  | ExprField (e, fname, l) -> (
    let ae, t = analyse_expr e ev in
    match t with
    | TypStruct sname ->
      let struct_mems = Smap.find sname ev.structs in
      check
        (Smap.mem fname struct_mems)
        l "Struct \"%s\" does not have a field named \"%s\"" sname fname;
      check (extract_anot_expr ae).lval l
        "Operator \".\" expects a l-valued struct or a struct reference";
      let rt = Smap.find fname struct_mems in
      (ExprField (ae, fname, var_anot l rt (extract_anot_expr ae).mut), rt)
    | TypRef (TypStruct sname) ->
      let struct_mems = Smap.find sname ev.structs in
      print_env ev;
      flush stdout;
      check
        (Smap.mem fname struct_mems)
        l "Struct \"%s\" does not have a field named \"%s\"" sname fname;
      let rt = Smap.find fname struct_mems in
      (ExprField (ae, fname, var_anot l rt false), rt)
    | TypMutRef (TypStruct sname) ->
      let struct_mems = Smap.find sname ev.structs in
      check
        (Smap.mem fname struct_mems)
        l "Struct \"%s\" does not have a field named \"%s\"" sname fname;
      let rt = Smap.find fname struct_mems in
      (ExprField (ae, fname, var_anot l rt true), rt)
    | _ ->
      err l "Operator \".\" expects either a struct or a reference to a struct")
  | ExprLen (e, l) -> (
    let ae, t = analyse_expr e ev in
    match t with
    | TypVec _ | TypRef (TypVec _) | TypMutRef (TypVec _) ->
      (ExprLen (ae, typ_anot l TypI32), TypI32)
    | _ ->
      err l "Method \".len()\" expects either a Vec<t> or a reference to Vec<t>"
    )
  | ExprBrack (e1, e2, l) -> (
    let ae1, t1 = analyse_expr e1 ev in
    let ae2, t2 = analyse_expr e2 ev in
    check (t2 = TypI32) l "Array-like type can only be indexed by type i32";
    match t1 with
    | TypVec ti ->
      check (extract_anot_expr ae1).lval l
        "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>";
      (ExprBrack (ae1, ae2, var_anot l ti (extract_anot_expr ae1).mut), ti)
    | TypRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti false), ti)
    | TypMutRef (TypVec ti) -> (ExprBrack (ae1, ae2, var_anot l ti true), ti)
    | _ ->
      err l
        "Indexing an array-like requires an l-valued Vec<t> or a reference to Vec<t>"
    )
  | ExprCall (fname, el, l) ->
    check (Smap.mem fname ev.funs) l "Undefined function \"%s\"" fname;
    let fun_ts, fun_ret = Smap.find fname ev.funs in
    let aels = List.map (fun e -> analyse_expr e ev) el in
    let arg_ts = List.map snd aels in
    check
      (List.length arg_ts = List.length el)
      l "Function \"%s\" expects %d argument while only %d were provided" fname
      (List.length arg_ts) (List.length el);
    List.iter2
      (fun t1 a2 ->
        let t2 = a2.ty in
        check (t1 <= t2) l
          "Function call for \"%s\" expected argument type %s but got %s" fname
          (string_of_typ t1) (string_of_typ t2))
      arg_ts fun_ts;
    (ExprCall (fname, List.map fst aels, typ_anot l fun_ret), fun_ret)
  | ExprVec (el, l) -> (
    let aels = List.map (fun e -> analyse_expr e ev) el in
    let arg_ts = List.map snd aels in
    if List.is_empty arg_ts then
      (ExprVec (List.map fst aels, typ_anot l TypEmptyVec), TypEmptyVec)
    else
      match list_cast arg_ts with
      | None ->
        err l "Macro call for vec! expects all elements to be of the same type"
      | Some ti ->
        let rt = TypVec ti in
        (ExprVec (List.map fst aels, typ_anot l rt), rt))
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
  check
    (Smap.cardinal st = List.length ls)
    l "Struct type \"%s\" expects %d fields while only %d were provided"
    id_struct (Smap.cardinal st) (List.length ls);
  List.map
    (fun field ->
      let fname, e, l = field in
      check (Smap.mem fname st) l
        "Struct type \"%s\" does not expect a field named \"%s\"" id_struct
        fname;
      let ae, et = analyse_expr e ev in
      let vt = Smap.find fname st in
      check (et <= vt) l
        "Struct type \"%s\" expects field named \"%s\" to be of type %s while %s was provided"
        id_struct fname (string_of_typ vt) (string_of_typ et);
      (fname, ae, typ_anot l vt))
    ls

and analyse_instr_if (p : loc instr_if) (ev : env) : anot instr_if * typ =
  match p with
  | IfElse (ec, bt, None, l) ->
    let aec, tc = analyse_expr ec ev in
    check (tc = TypBool) l
      "If confition expects type bool while %s was provided." (string_of_typ tc);
    let abt, tbt = analyse_block bt ev in
    check (tbt = TypUnit) l
      "Then block exepts type unit if no else block, %s was provided."
      (string_of_typ tbt);
    (IfElse (aec, abt, None, typ_anot l TypUnit), TypUnit)
  | IfElse (ec, bt, Some be, l) ->
    let aec, tc = analyse_expr ec ev in
    check (tc = TypBool) l
      "If confition expects type bool while %s was provided." (string_of_typ tc);
    let abt, tbt = analyse_block bt ev in
    let abe, tbe = analyse_block be ev in
    check (tbt = tbe) l
      "Then and else block exepts the sametype while %s and %s were provided."
      (string_of_typ tbt) (string_of_typ tbe);
    (IfElse (aec, abt, Some abe, typ_anot l tbe), tbe)
  | IfElif (ec, bt, ie, l) ->
    let aec, tc = analyse_expr ec ev in
    check (tc = TypBool) l
      "If confition expects type bool while %s was provided." (string_of_typ tc);
    let abt, tbt = analyse_block bt ev in
    let abe, tbe = analyse_instr_if ie ev in
    check (tbt = tbe) l
      "Then and else block exepts the sametype while %s and %s were provided."
      (string_of_typ tbt) (string_of_typ tbe);
    (IfElif (aec, abt, abe, typ_anot l tbe), tbe)

and analyse_block (p : loc block) (ev : env) : anot block * typ =
  let rec analyse_instr_list (ls : loc instr list) (ev : env) :
      anot instr list * env =
    match ls with
    | InstrExpr (e, l) :: tll ->
      let ae, t = analyse_expr e ev in
      let ail, rev = analyse_instr_list tll ev in
      (* check
        (t = TypUnit)
        l "Instruction in blocks expects type unit while %s was provided." (string_of_typ t); *)
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
      check
        (Smap.mem id_struct ev.structs)
        l "No such struct type \"%s\"" id_struct;
      let st = Smap.find id_struct ev.structs in
      let ae = analyse_field_list id_struct fl st ev l in
      let nev =
        {
          vars= Smap.add id (var_anot l (TypStruct id_struct) m) ev.vars;
          structs= ev.structs;
          funs= ev.funs;
          curr_fun_rt= ev.curr_fun_rt;
        }
      in
      let ail, rev = analyse_instr_list tll nev in
      (InstrLetStruct (m, id, id_struct, ae, default_anot l) :: ail, rev)
    | InstrWhile (eb, e, l) :: tll ->
      let aeb, tb = analyse_expr eb ev in
      let ae, t = analyse_block e ev in
      check (tb = TypBool) l
        "While condition excepts type bool while %s was provided."
        (string_of_typ tb);
      check (t = TypUnit) l
        "While block excepts type unit while %s was provided." (string_of_typ t);
      let ail, rev = analyse_instr_list tll ev in
      (InstrWhile (aeb, ae, default_anot l) :: ail, rev)
    | InstrReturn (eo, l) :: tll -> (
      let rt = Option.get ev.curr_fun_rt in
      match eo with
      | Some e ->
        let ae, t = analyse_expr e ev in
        check (t = rt) l
          "A return in the current function excepts type %s while %s was provided."
          (string_of_typ rt) (string_of_typ t);
        let ail, rev = analyse_instr_list tll ev in
        (InstrReturn (Some ae, default_anot l) :: ail, rev)
      | None ->
        check (rt = TypUnit) l
          "A return in the current function excepts type %s." (string_of_typ rt);
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

let analyse_fun_decl (p : loc fun_decl) (ev : env) (l : loc) :
    anot fun_decl * env =
  let rec aux (ls : loc param list) (m : anot Smap.t) :
      anot param list * anot Smap.t * anot list =
    match ls with
    | (mu, fname, ty, l) :: tll ->
      let aty, t = analyse_ty ty ev in
      check
        (not (Smap.mem fname m))
        l "Function declaration already has an argument named %s." fname;
      let aa = var_anot l t mu in
      let al, rm, kl = aux tll (Smap.add fname aa m) in
      ((mu, fname, aty, typ_anot l t) :: al, rm, aa :: kl)
    | [] -> ([], m, [])
  in
  check
    (not (Smap.mem p.name ev.funs))
    l "Function named %s already defined." p.name;
  let at, rt =
    match p.rtype with
    | Some ty ->
      let a, t = analyse_ty ty ev in
      (Some a, t)
    | None -> (None, TypUnit)
  in
  let npl, m, anl = aux p.params Smap.empty in
  let nev =
    {
      vars= Smap.union (fun _ _ a -> Some a) ev.vars m;
      structs= ev.structs;
      funs= Smap.add p.name (anl, rt) ev.funs;
      curr_fun_rt= Some rt;
    }
  in
  check
    (not (is_ref_typ rt))
    l "Function declaration expects return type not to be a reference";
  let bo, bt = analyse_block p.body nev in
  let isl, le, _ = bo in
  let gbt =
    match (List.rev isl, le) with
    | InstrReturn (Some ire, _) :: _, None -> (extract_anot_expr ire).ty
    | InstrIf (ire, _) :: _, None -> (extract_anot_instr_if ire).ty
    | _ -> bt
  in
  check (rt = gbt) l
    "Function declaration's return type does not match its definition, expected %s while %s was provided"
    (string_of_typ rt) (string_of_typ gbt);
  ({name= p.name; params= npl; rtype= at; body= bo}, nev)

let analyse_struct_decl (p : loc struct_decl) (ev : env) (l : loc) :
    anot struct_decl * env =
  let rec aux (ls : loc field list) (m : typ Smap.t) (ev : env) :
      anot field list * typ Smap.t =
    match ls with
    | (fname, ty, l) :: tll ->
      let aty, t = analyse_ty ty ev in
      check
        (not (Smap.mem fname m))
        l
        "Struct declaration already has a field named %s unit while unit was provided."
        fname;
      check
        (not (is_ref_typ t))
        l "Struct declaration expects fields types not to be references.";
      check
        (not (t = TypStruct p.name))
        l "Struct declaration forbids fields types to be recursive.";
      let al, rm = aux tll (Smap.add fname t m) ev in
      ((fname, aty, typ_anot l t) :: al, rm)
    | [] -> ([], m)
  in
  check
    (not (Smap.mem p.name ev.structs))
    l "Struct name \"%s\" is already taken" p.name;
  let fake_typ_ev =
    {
      vars= ev.vars;
      structs= Smap.add p.name Smap.empty ev.structs;
      funs= ev.funs;
      curr_fun_rt= ev.curr_fun_rt;
    }
  in
  let nf, st = aux p.fields Smap.empty fake_typ_ev in
  ( {name= p.name; fields= nf},
    {
      vars= ev.vars;
      structs= Smap.add p.name st ev.structs;
      funs= ev.funs;
      curr_fun_rt= ev.curr_fun_rt;
    } )

let analyse_decl (p : loc decl) (ev : env) : anot decl * env =
  match p with
  | DeclStruct (sdecl, l) ->
    let ad, nev = analyse_struct_decl sdecl ev l in
    (DeclStruct (ad, default_anot l), nev)
  | DeclFun (fdecl, l) ->
    let ad, nev = analyse_fun_decl fdecl ev l in
    (DeclFun (ad, default_anot l), nev)

let analyze_program (p : loc program) : anot program =
  let prog, l = p in
  let rec aux (p : loc decl list) (ev : env) : anot decl list =
    match p with
    | [] -> []
    | h :: t ->
      let nh, nev = analyse_decl h ev in
      nh
      :: aux t
           {
             vars= Smap.empty;
             structs= nev.structs;
             funs= nev.funs;
             curr_fun_rt= None;
           }
  in
  ( aux prog
      {
        vars= Smap.empty;
        structs= Smap.empty;
        funs= Smap.empty;
        curr_fun_rt= None;
      },
    default_anot l )
