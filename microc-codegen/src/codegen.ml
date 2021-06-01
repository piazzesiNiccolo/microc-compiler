open Ast
module L = Llvm

let llcontext = L.global_context ()

let int_type = L.i32_type llcontext

let float_type = L.float_type llcontext

let bool_type = L.i1_type llcontext

let char_type = L.i8_type llcontext

let void_type = L.void_type llcontext

let llvm_one = L.const_int int_type 1

let llvm_zero = L.const_int int_type 0

let llvm_onef = L.const_float float_type 1.0

let llvm_true = L.const_int bool_type 1

let llvm_false = L.const_int bool_type 0

type struct_type = L.lltype * (L.lltype * string) list

type symbols = {
  fun_symbols : L.llvalue Symbol_table.t;
  var_symbols : L.llvalue Symbol_table.t;
  struct_symbols : struct_type Symbol_table.t;
}

let rec build_llvm_type structs = function
  | TypI -> int_type
  | TypF -> float_type
  | TypC -> char_type
  | TypB -> bool_type
  | TypA (t, Some v) -> L.array_type (build_llvm_type structs t) v
  | TypA (t, None) | TypP t -> L.pointer_type (build_llvm_type structs t)
  | TypV -> void_type
  | TypNull -> L.pointer_type void_type
  | TypS n -> (
      match Symbol_table.lookup n structs with
      | Some (t, _) -> t
      | None -> Util.raise_codegen_error @@ "Undefined structure " ^ n)

let unop = function
  | t, Neg when t = int_type -> L.build_neg
  | t, Neg when t = float_type -> L.build_fneg
  | t, Not when t = bool_type -> L.build_not
  | _ -> Util.raise_codegen_error "Invald unary operator for global variable"

let bin_op = function
  | t1, t2, Add when t1 = int_type && t2 = int_type -> L.build_add
  | t1, t2, Sub when t1 = int_type && t2 = int_type -> L.build_sub
  | t1, t2, Div when t1 = int_type && t2 = int_type -> L.build_sdiv
  | t1, t2, Mult when t1 = int_type && t2 = int_type -> L.build_mul
  | t1, t2, Mod when t1 = int_type && t2 = int_type -> L.build_srem
  | t1, t2, Less when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_type && t2 = int_type ->
      L.build_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_type && t2 = int_type -> L.build_icmp L.Icmp.Ne
  | t1, t2, Add when t1 = float_type && t2 = float_type -> L.build_fadd
  | t1, t2, Sub when t1 = float_type && t2 = float_type -> L.build_fsub
  | t1, t2, Div when t1 = float_type && t2 = float_type -> L.build_fdiv
  | t1, t2, Mult when t1 = float_type && t2 = float_type -> L.build_fmul
  | t1, t2, Less when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_type && t2 = float_type ->
      L.build_fcmp L.Fcmp.One
  | t1, t2, And when t1 = bool_type && t2 = bool_type -> L.build_and
  | t1, t2, Or when t1 = bool_type && t2 = bool_type -> L.build_or
  | _ ->
      Util.raise_codegen_error
        "Mismatch between type of global variable and initial value"

let const_op = function
  | t, Neg when t = int_type -> L.const_neg
  | t, Neg when t = float_type -> L.const_fneg
  | t, Not when t = bool_type -> L.const_not
  | _ -> Util.raise_codegen_error "Invald unary operator for global variable"

let const_bin_op = function
  | t1, t2, Add when t1 = int_type && t2 = int_type -> L.const_add
  | t1, t2, Sub when t1 = int_type && t2 = int_type -> L.const_sub
  | t1, t2, Div when t1 = int_type && t2 = int_type -> L.const_sdiv
  | t1, t2, Mult when t1 = int_type && t2 = int_type -> L.const_mul
  | t1, t2, Mod when t1 = int_type && t2 = int_type -> L.const_srem
  | t1, t2, Less when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Slt
  | t1, t2, Leq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Sle
  | t1, t2, Greater when t1 = int_type && t2 = int_type ->
      L.const_icmp L.Icmp.Sgt
  | t1, t2, Geq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Sge
  | t1, t2, Equal when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = int_type && t2 = int_type -> L.const_icmp L.Icmp.Ne
  | t1, t2, Add when t1 = float_type && t2 = float_type -> L.const_fadd
  | t1, t2, Sub when t1 = float_type && t2 = float_type -> L.const_fsub
  | t1, t2, Div when t1 = float_type && t2 = float_type -> L.const_fdiv
  | t1, t2, Mult when t1 = float_type && t2 = float_type -> L.const_fmul
  | t1, t2, Less when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Olt
  | t1, t2, Leq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Ole
  | t1, t2, Greater when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Ogt
  | t1, t2, Geq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Oge
  | t1, t2, Equal when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.Oeq
  | t1, t2, Neq when t1 = float_type && t2 = float_type ->
      L.const_fcmp L.Fcmp.One
  | t1, t2, And when t1 = bool_type && t2 = bool_type -> L.const_and
  | t1, t2, Or when t1 = bool_type && t2 = bool_type -> L.const_or
  | _ ->
      Util.raise_codegen_error
        "Mismatch between type of global variable and initial value"

let build_unary_incr_or_decr builder op value =
  let inc_op = function
    | (PreInc | PostInc), t when t = int_type -> L.build_add llvm_one
    | (PreInc | PostInc), t when t = float_type -> L.build_fadd llvm_one
    | (PreDec | PostDec), t when t = int_type -> Fun.flip L.build_sub llvm_one
    | (PreDec | PostDec), t when t = float_type -> Fun.flip L.build_sub llvm_one
    | _ ->
        Util.raise_codegen_error @@ "Invalid type for operator " ^ show_uop op
  in
  let apply_op = inc_op (op, L.type_of value) in
  let before = L.build_load value "" builder in
  let after = apply_op before "" builder in
  L.build_store after value builder |> ignore;
  if op = PreInc || op = PreDec then after else before

let rec codegen_expr bm scope builder e =
  match e.node with
  | ILiteral i -> L.const_int int_type i
  | FLiteral f -> L.const_float float_type f
  | CLiteral c -> L.const_int char_type (Char.code c)
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.const_stringz llcontext s
  | Null -> L.const_pointer_null void_type
  | Addr a -> codegen_access bm scope builder a
  | Access a -> codegen_access bm scope builder a
  | Assign (a, e) ->
      let acc_var = codegen_access bm scope builder a in
      let expr_val = codegen_expr bm scope builder e in
      L.build_store expr_val acc_var builder |> ignore;
      expr_val
  | UnaryOp (((PreInc | PostInc | PreDec | PostDec) as op), e) ->
      let e_val = codegen_expr bm scope builder e in
      build_unary_incr_or_decr builder op e_val
  | UnaryOp (u, e) ->
      let e_val = codegen_expr bm scope builder e in
      unop (L.type_of e_val, u) e_val "" builder
  | BinaryOp (b, e1, e2) ->
      let e1_val, e2_val =
        (codegen_expr bm scope builder e1, codegen_expr bm scope builder e2)
      in
      bin_op (L.type_of e1_val, L.type_of e2_val, b) e1_val e2_val "" builder
  | Call (f, params) ->
      let actual_f =
        match Symbol_table.lookup f scope.fun_symbols with
        | Some n -> n
        | None -> Util.raise_codegen_error @@ "Undefined  function  " ^ f
      in
      let llvm_params = List.map (codegen_expr bm scope builder) params in
      L.build_call actual_f (Array.of_list llvm_params) f builder

and codegen_access bm scope builder e = llvm_false

let codegen_stmt fdef scope builder stmt = ()

let codegen_func llmodule scope func =
  let ret_type = build_llvm_type scope.struct_symbols func.typ in
  let formals_types =
    func.formals |> List.map fst
    |> List.map (build_llvm_type scope.struct_symbols)
  in
  let f_type = L.function_type ret_type (Array.of_list formals_types) in
  let f = L.define_function func.fname f_type llmodule in
  let local_scope =
    {
      scope with
      fun_symbols = Symbol_table.add_entry func.fname f scope.fun_symbols;
      var_symbols = Symbol_table.begin_block scope.var_symbols;
    }
  in
  let f_builder = L.builder_at_end llcontext (L.entry_block f) in
  let build_param scope builder (t, i) p =
    let l = L.build_alloca (build_llvm_type scope.struct_symbols t) i builder in
    Symbol_table.add_entry i l scope.var_symbols |> ignore;
    L.build_store p l builder |> ignore
  in
  List.iter2
    (build_param local_scope f_builder)
    func.formals
    (Array.to_list (L.params f));
  codegen_stmt f local_scope f_builder func.body

let rec codegen_global_expr structs t e =
  match e.node with
  | ILiteral i -> L.const_int int_type i
  | FLiteral f -> L.const_float float_type f
  | CLiteral c -> Char.code c |> L.const_int char_type
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.const_stringz llcontext s
  | Null -> build_llvm_type structs t |> L.const_pointer_null
  | UnaryOp (uop, e1) ->
      let a = codegen_global_expr structs t e1 in
      let t1 = L.type_of a in
      const_op (t1, uop) a
  | BinaryOp (binop, e1, e2) ->
      let a = codegen_global_expr structs t e1 in
      let b = codegen_global_expr structs t e2 in
      let t1, t2 = (L.type_of a, L.type_of b) in
      const_bin_op (t1, t2, binop) a b
  | _ ->
      Util.raise_codegen_error "Invalid initial expression for global variable"

let codegen_global_variable llmodule scope (t, i) init =
  let var_init =
    Option.fold
      ~none:(L.undef (build_llvm_type scope.struct_symbols t))
      ~some:(codegen_global_expr scope.struct_symbols t)
      init
  in
  let var = L.define_global i var_init llmodule in
  Symbol_table.add_entry i var scope.var_symbols |> ignore

let codegen_topdecl llmodule scope n =
  match n.node with
  | Fundecl f -> codegen_func llmodule scope f |> ignore
  | Vardec (t, i, init) -> codegen_global_variable llmodule scope (t, i) init
  | Structdecl s ->
      let fields_t =
        s.fields
        |> List.map (fun (t, f) -> (build_llvm_type scope.struct_symbols t, f))
      in
      let struct_t =
        L.struct_type llcontext (Array.of_list (List.map fst fields_t))
      in
      Symbol_table.add_entry s.sname (struct_t, fields_t) scope.struct_symbols
      |> ignore

let add_rt_support llmodule scope =
  Util.rt_support
  |> List.map (fun (n, (_, f)) ->
         ( n,
           L.function_type
             (build_llvm_type scope.struct_symbols f.typ)
             (List.map
                (build_llvm_type scope.struct_symbols)
                (List.map fst f.formals)
             |> Array.of_list) ))
  |> List.iter (fun (n, t) ->
         Symbol_table.add_entry n
           (L.declare_function n t llmodule)
           scope.fun_symbols
         |> ignore)

let to_ir (Prog topdecls) =
  let module_name = "microc_module" in
  let llmodule = L.create_module llcontext module_name in
  let init_scope =
    {
      fun_symbols = Symbol_table.empty_table |> Symbol_table.begin_block;
      var_symbols = Symbol_table.empty_table |> Symbol_table.begin_block;
      struct_symbols = Symbol_table.empty_table |> Symbol_table.begin_block;
    }
  in
  add_rt_support llmodule init_scope;
  List.iter (codegen_topdecl llmodule init_scope) topdecls;
  llmodule
