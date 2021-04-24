open Ast
open Symbol_table

type pos = Lexing.position * Lexing.position

type var_info = pos * typ

type fun_info = pos * fun_decl

type symbols = {
  fun_symbols : fun_info Symbol_table.t;
  var_symbols : var_info Symbol_table.t;
}

let check_type loc t =
  match t with
  | TypA (TypA (_, _), _) ->
      Util.raise_semantic_error loc "Cannot define multidimensional array"
  | TypA (TypV, _) ->
      Util.raise_semantic_error loc "Trying to define a void array"
  | TypA (t, Some i) when i < 1 ->
      Util.raise_semantic_error loc "Array must have size > 0"
  | TypA (t, None) ->
      Util.raise_semantic_error loc
        "Array must be declared with an initial size"
  | TypP TypV -> Util.raise_semantic_error loc "Trying to define a void pointer"
  | _ -> ()

let check_var_type loc t =
  match t with
  | TypV -> Util.raise_semantic_error loc "Trying to define a void variable"
  | _ -> check_type loc t

let check_var_decl scope loc (t, i) =
  check_var_type loc t;
  try Symbol_table.add_entry i (loc, t) scope.var_symbols |> ignore
  with DuplicateEntry ->
    Util.raise_semantic_error loc
    @@ "Variable " ^ i ^ " already defined in current scope"

let check_fun_type loc t =
  match t with
  | TypA (_, _) | TypP _ ->
      Util.raise_semantic_error loc
      @@ "cannot define function of type " ^ show_typ t
  | _ -> ()

let rec expr_type scope e = 
match e.node with
| Access a -> access_type scope a
| Assign (a,e) ->
  let at = access_type scope a in 
  let et = expr_type scope e in
  if at == et then at else Util.raise_semantic_error e.loc "Cannot assign value of different type"
| Addr a -> let at = access_type scope a in TypP(at)
| ILiteral(_) -> TypI
| CLiteral(_) -> TypC
| BLiteral(_) -> TypB
| _ -> TypB (*TODO *)

and  access_type scope a =
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
        |TypA(t,_) -> t
        |_ -> Util.raise_semantic_error a.loc "Cannot access index of non-array" 
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
  | While (e, s) ->
      if expr_type scope e  <> TypB then
        Util.raise_semantic_error s.loc "While condition is not boolean"
      else check_stmt scope ftype s
  | Expr e -> expr_type scope e |> ignore
  | Return (Some e) ->
      if expr_type scope e <> ftype then
        Util.raise_semantic_error s.loc
          "Return type does not match function signature"
      else ()
  | Return None ->
      if ftype <> TypV then
        Util.raise_semantic_error s.loc "missing return statement"
      else ()
  | Block stmts ->
      let new_scope =
        { scope with var_symbols = Symbol_table.begin_block scope.var_symbols }
      in
      List.iter (check_stmtordec new_scope ftype) stmts

and check_stmtordec scope ftype s =
  match s.node with
  | Dec (t, i) -> check_var_decl scope s.loc (t, i)
  | Stmt s -> check_stmt scope ftype s

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
  List.iter (check_var_decl new_scope loc) f.formals;
  check_stmt new_scope f.typ f.body

let check_topdecl scope node =
  match node.node with
  | Fundecl f -> check_func f scope node.loc
  | Vardec (t, i) -> check_var_decl scope node.loc (t, i)

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

let check_global_properties scope =
  let m = Symbol_table.lookup "main" scope.fun_symbols in
  match m with
  | Some (_, { typ = TypV; fname = "main"; formals = [] }) -> ignore
  | Some (_, { typ = TypI; fname = "main"; formals = [] }) -> ignore
  | Some _ -> Util.raise_semantic_error dummy "Invalid signature of main"
  | None -> Util.raise_semantic_error dummy " No main function defined"

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
