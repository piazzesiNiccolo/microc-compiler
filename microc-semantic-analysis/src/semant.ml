open Ast
open Symbol_table

type var_info = position * typ

type fun_info = position * fun_decl

type symbols = {
  fun_symbols : fun_info Symbol_table.t;
  var_symbols : var_info Symbol_table.t;
}

let rec defined_type_size t =
  match t with
  | TypA (t, Some _) -> defined_type_size t
  | TypA (t, None) -> false
  | _ -> true

let check_type loc t =
  match t with
  | TypA (TypV, _) ->
      Util.raise_semantic_error loc "Trying to define a void array"
  | TypA (t, Some i) when i < 1 ->
      Util.raise_semantic_error loc "Array must have size > 0"
  | TypA (t, _) when not (defined_type_size t) ->
      Util.raise_semantic_error loc "Array size undefined"
  | TypP TypV -> Util.raise_semantic_error loc "Trying to define a void pointer"
  | _ -> ()

let check_var_type loc t =
  match t with
  | TypV -> Util.raise_semantic_error loc "Trying to define a void variable"
  | TypA (t, None) ->
      Util.raise_semantic_error loc
        "Array must be declared with an initial size"
  | _ -> check_type loc t

let check_var_decl scope loc (t, i) =
  check_var_type loc t;
  try Symbol_table.add_entry i (loc, t) scope.var_symbols |> ignore
  with DuplicateEntry ->
    Util.raise_semantic_error loc
    @@ "Variable " ^ i ^ " already defined in current scope"

let check_fun_type loc t =
  match t with
  | TypA (_, _) | TypP _ | TypNull ->
      Util.raise_semantic_error loc
      @@ "cannot define function of type " ^ show_typ t
  | _ -> ()

let rec match_types loc t1 t2 =
  match (t1, t2) with
  | TypA (t1, Some v), TypA (t2, Some v2) when v = v2 -> match_types loc t1 t2
  | TypA (t1, Some v), TypA (t2, Some v2) when v <> v2 ->
      Util.raise_semantic_error loc "Array size must be the same"
  | TypA (t1, None), TypA (t2, _) -> match_types loc t1 t2
  | TypP _, TypNull -> true
  | TypNull, TypP _ -> true
  | TypP t1, TypP t2 -> match_types loc t1 t2
  | t1, t2 -> t1 = t2

let binaryexp_type loc op et1 et2 =
  match (op, et1, et2) with
  | (Add | Sub | Mult | Div | Mod | Comma), TypI, TypI -> TypI
  | (Add | Sub | Mult | Div | Mod | Comma), TypF, TypF -> TypF
  | (Equal | Neq | Less | Leq | Greater | Geq), TypI, TypI -> TypB
  | (Equal | Neq | Less | Leq | Greater | Geq), TypF, TypF -> TypB
  | (Equal | Neq), TypC, TypC -> TypB
  | (Equal | Neq), TypA (t1, _), TypA (t2, _) when match_types loc t1 t2 -> TypB
  | (Equal | Neq), TypP _, TypNull -> TypB
  | (Equal | Neq), TypNull, TypP _ -> TypB
  | (Equal | Neq), TypP t1, TypP t2 when match_types loc t1 t2 -> TypB
  | (And | Or), TypB, TypB -> TypB
  | _ -> Util.raise_semantic_error loc "Type mismatch on expression"

let unaryexp_type loc u et =
  match (u, et) with
  | Neg, TypI -> TypI
  | Neg, TypF -> TypF
  | Not, TypB -> TypB
  | (PreInc | PreDec | PostInc | PostDec), (TypI | TypF) -> et
  | (PreInc | PreDec | PostInc | PostDec), _ ->
      Util.raise_semantic_error loc
        "Cannot use operator pre/post increment/decrement with non numeric \
         value"
  | Neg, _ ->
      Util.raise_semantic_error loc
        "Cannot apply minus operator to non numeric value"
  | Not, _ ->
      Util.raise_semantic_error loc
        "Cannot apply not operator to non boolean value"

let rec expr_type scope e =
  match e.node with
  | Access a -> access_type scope a
  | Assign (a, e) -> (
      let at = access_type scope a in
      match at with
      | TypA (_, _) ->
          Util.raise_semantic_error e.loc "trying to reassign array"
      | _ ->
          let et = expr_type scope e in
          if match_types e.loc at et then at
          else
            Util.raise_semantic_error e.loc
              "Cannot assign a value of different type")
  | Addr a ->
      let at = access_type scope a in
      TypP at
  | ILiteral _ -> TypI
  | CLiteral _ -> TypC
  | FLiteral _ -> TypF
  | BLiteral _ -> TypB
  | String s -> TypA (TypC, Some (String.length s + 1))
  | Null -> TypNull
  | UnaryOp (u, e1) ->
      let et = expr_type scope e1 in
      unaryexp_type e.loc u et
  | BinaryOp (op, e1, e2) ->
      let et1 = expr_type scope e1 in
      let et2 = expr_type scope e2 in
      binaryexp_type e.loc op et1 et2
  | Call (id, params) -> (
      let params_types = List.map (expr_type scope) params in
      match Symbol_table.lookup id scope.fun_symbols with
      | Some (_, f) ->
          let formals_types = List.map (fun (t, i) -> t) f.formals in
          if List.length params_types = List.length formals_types then
            if List.for_all2 (match_types e.loc) formals_types params_types then
              f.typ
            else Util.raise_semantic_error e.loc "wrong parameter type"
          else
            Util.raise_semantic_error e.loc
              "Missing one or more arguments in function call"
      | None ->
          Util.raise_semantic_error e.loc @@ "Function " ^ id ^ "not defined")

and access_type scope a =
  match a.node with
  | AccVar v -> (
      match Symbol_table.lookup v scope.var_symbols with
      | Some (_, t) -> t
      | None ->
          Util.raise_semantic_error a.loc
          @@ "Variable " ^ v ^ " not defined in current scope")
  | AccDeref e -> (
      match expr_type scope e with
      | TypP t -> t
      | _ ->
          Util.raise_semantic_error a.loc "Trying to dereference a non pointer")
  | AccIndex (a, e) -> (
      match expr_type scope e with
      | TypI -> (
          match access_type scope a with
          | TypA (t, _) -> t
          | _ ->
              Util.raise_semantic_error a.loc "Cannot access index of non-array"
          )
      | _ -> Util.raise_semantic_error a.loc "Index of array must be an integer"
      )

let rec check_stmt scope ftype s =
  match s.node with
  | If (e, s1, s2) ->
      if expr_type scope e <> TypB then
        Util.raise_semantic_error s.loc "If condition is not boolean"
      else check_stmt scope ftype s1;
      check_stmt scope ftype s2
  | DoWhile (e, s) ->
      if expr_type scope e <> TypB then
        Util.raise_semantic_error s.loc "Condition is not boolean"
      else check_stmt scope ftype s
  | While (e, s) ->
      if expr_type scope e <> TypB then
        Util.raise_semantic_error s.loc "Condition is not boolean"
      else check_stmt scope ftype s
  | Expr e -> expr_type scope e |> ignore
  | Return (Some e) ->
      if expr_type scope e <> ftype then
        Util.raise_semantic_error s.loc
          "Return type does not match function signature"
      else ()
  | Return None ->
      if ftype <> TypV then
        Util.raise_semantic_error s.loc "missing return value"
      else ()
  | Block stmts ->
      let new_scope =
        { scope with var_symbols = Symbol_table.begin_block scope.var_symbols }
      in
      List.iter (check_stmtordec new_scope ftype) stmts

and check_stmtordec scope ftype s =
  match s.node with
  | Dec (t, i, None) -> check_var_decl scope s.loc (t, i)
  | Dec (t, i, Some e) -> (
      check_var_decl scope s.loc (t, i);
      match (t, e.node) with
      | TypA (TypC, _), String _ -> ()
      | _ ->
          let et = expr_type scope e in
          match et with
          | TypA(_,_) -> Util.raise_semantic_error s.loc "Array is not a valid value initializer"
          | _ -> 
            if match_types s.loc t et then ()
            else Util.raise_semantic_error s.loc "Value of different type")
  | Stmt s -> check_stmt scope ftype s

let check_parameter scope loc (t, i) =
  check_type loc t;
  try Symbol_table.add_entry i (loc, t) scope.var_symbols |> ignore
  with DuplicateEntry ->
    Util.raise_semantic_error loc
    @@ "Parameter " ^ i ^ " already defined in current scope"

let check_func f scope loc =
  check_fun_type loc f.typ;
  let rec_scope =
    try Symbol_table.add_entry f.fname (loc, f) scope.fun_symbols
    with Symbol_table.DuplicateEntry ->
      Util.raise_semantic_error loc
      @@ "function " ^ f.fname ^ " already defined"
  in
  let new_scope =
    {
      fun_symbols = rec_scope;
      var_symbols = Symbol_table.begin_block scope.var_symbols;
    }
  in
  List.iter (check_parameter new_scope loc) f.formals;
  check_stmt new_scope f.typ f.body

let rec global_expr_type scope loc e =
  match e.node with
  | ILiteral _ | CLiteral _ | BLiteral _ | FLiteral _ | String _ | Null ->
      expr_type scope e
  | UnaryOp (u, e) ->
      let et = global_expr_type scope loc e in
      unaryexp_type loc u et
  | BinaryOp (o, e1, e2) ->
      let et1 = global_expr_type scope loc e1 in
      let et2 = global_expr_type scope loc e2 in
      binaryexp_type loc o et1 et2
  | _ ->
      Util.raise_semantic_error loc
        "Cannot assign non-constant value at compile time to a global variable"

let check_topdecl scope node =
  match node.node with
  | Fundecl f -> check_func f scope node.loc
  | Vardec (t, i, None) -> check_var_decl scope node.loc (t, i)
  | Vardec (t, i, Some e) -> (
      check_var_decl scope node.loc (t, i);
      match (t, e.node) with
      | TypA (TypC, _), String _ -> ()
      | _ ->
          let et = global_expr_type scope node.loc e in
          if match_types node.loc t et then ()
          else Util.raise_semantic_error node.loc "Value of different type")

let check_global_properties scope =
  let m = Symbol_table.lookup "main" scope.fun_symbols in
  match m with
  | Some (_, { typ = TypV; fname = "main"; formals = [] }) -> ()
  | Some (_, { typ = TypI; fname = "main"; formals = [] }) -> ()
  | Some _ -> Util.raise_semantic_error dummy_pos "Invalid signature of main"
  | None -> Util.raise_semantic_error dummy_pos " No main function defined"

let prelude_functions =
  let init_scope = Symbol_table.empty_table |> Symbol_table.begin_block in
  List.iter
    (fun (name, f) -> Symbol_table.add_entry name f init_scope |> ignore)
    Util.prelude;
  init_scope

let check (Prog topdecls) =
  let toplevel_scope =
    {
      fun_symbols = prelude_functions;
      var_symbols = Symbol_table.empty_table |> Symbol_table.begin_block;
    }
  in
  List.iter (check_topdecl toplevel_scope) topdecls;
  check_global_properties toplevel_scope |> ignore
