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

type symbols = {
  fun_symbols : L.llvalue Symbol_table.t;
  var_symbols : L.llvalue Symbol_table.t;
  struct_symbols : (L.lltype * string list) Symbol_table.t;
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
  | t1, t2, Equal
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq
    when L.classify_type t1 = L.TypeKind.Pointer
         && L.classify_type t2 = L.TypeKind.Pointer ->
      L.build_icmp L.Icmp.Ne
  | t1, t2, Equal when t1 = char_type && t2 = char_type ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = char_type && t2 = char_type -> L.build_icmp L.Icmp.Ne
  | t1, t2, And when t1 = bool_type && t2 = bool_type -> L.build_and
  | t1, t2, Or when t1 = bool_type && t2 = bool_type -> L.build_or
  | t1, t2, Equal when t1 = bool_type && t2 = bool_type ->
      L.build_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_type && t2 = bool_type -> L.build_icmp L.Icmp.Ne
  | t1, t2, _ -> Util.raise_codegen_error "Type mismatch between operands"

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
  | t1, t2, Equal when t1 = bool_type && t2 = bool_type ->
      L.const_icmp L.Icmp.Eq
  | t1, t2, Neq when t1 = bool_type && t2 = bool_type -> L.const_icmp L.Icmp.Ne
  | _ ->
      Util.raise_codegen_error
        "Mismatch between type of global variable and initial value"

let build_unary_incr_or_decr builder op value =
  let inc_op = function
    | (PreInc | PostInc), t when t = int_type -> L.build_add llvm_one
    | (PreInc | PostInc), t when t = float_type -> L.build_fadd llvm_one
    | (PreDec | PostDec), t when t = int_type -> Fun.flip L.build_sub llvm_one
    | (PreDec | PostDec), t when t = float_type ->
        Fun.flip L.build_fsub llvm_onef
    | _ ->
        Util.raise_codegen_error @@ "Invalid type for operator " ^ show_uop op
  in
  let apply_op = inc_op (op, L.element_type (L.type_of value)) in
  let before = L.build_load value "" builder in
  let after = apply_op before "" builder in
  L.build_store after value builder |> ignore;
  if op = PreInc || op = PreDec then after else before

let rec codegen_expr scope builder e =
  match e.node with
  | ILiteral i -> L.const_int int_type i
  | FLiteral f -> L.const_float float_type f
  | CLiteral c -> L.const_int char_type (Char.code c)
  | BLiteral b -> if b then llvm_true else llvm_false
  | String s -> L.build_global_string (s ^ "\000") "" builder
  | Null -> L.undef (int_type |> L.pointer_type)
  | Addr a -> codegen_access scope builder a
  | Access a ->
      let a_val = codegen_access scope builder a in
      L.build_load a_val "" builder
  | Assign (a, e) ->
      let acc_var = codegen_access scope builder a in
      let expr_val = codegen_expr scope builder e in
      let expr_act_val =
        if L.is_undef expr_val then
          L.const_pointer_null (L.type_of acc_var |> L.element_type)
        else expr_val
      in
      L.build_store expr_act_val acc_var builder |> ignore;
      expr_act_val
  | ShortAssign (a, op, e) ->
      let acc = codegen_access scope builder a in
      let a_val = L.build_load acc "" builder in
      let e_val = codegen_expr scope builder e in
      let value =
        bin_op (L.type_of a_val, L.type_of e_val, op) a_val e_val "" builder
      in
      L.build_store value acc builder |> ignore;
      value
  | UnaryOp (((PreInc | PostInc | PreDec | PostDec) as op), e) ->
      let access_e =
        match e.node with
        | Access a -> a
        | _ -> failwith "Unreachable statement"
      in
      let e_val = codegen_access scope builder access_e in
      build_unary_incr_or_decr builder op e_val
  | UnaryOp (u, e) ->
      let e_val = codegen_expr scope builder e in
      unop (L.type_of e_val, u) e_val "" builder
  | BinaryOp (b, e1, e2) ->
      let e1_val, e2_val =
        let v1, v2 =
          (codegen_expr scope builder e1, codegen_expr scope builder e2)
        in
        let t1, t2 = (L.type_of v1, L.type_of v2) in
        match (v1, v2) with
        | e1v, e2v when L.is_undef e1v && not (L.is_undef e2v) ->
            (L.const_pointer_null (L.pointer_type (L.element_type t2)), v2)
        | e1v, e2v when (not (L.is_undef e1v)) && L.is_undef e2v ->
            (v1, L.const_pointer_null (L.pointer_type (L.element_type t1)))
        | e1v, e2v -> (e1v, e2v)
      in
      bin_op (L.type_of e1_val, L.type_of e2_val, b) e1_val e2_val "" builder
  | Call (f, params) ->
      let actual_f =
        match Symbol_table.lookup f scope.fun_symbols with
        | Some n -> n
        | None -> Util.raise_codegen_error @@ "Undefined  function  " ^ f
      in

      let codegen_call_expr scope builder p e =
        match e.node with
        | Access a -> (
            let a_val = codegen_access scope builder a in
            let pt = p |> L.type_of |> L.classify_type in
            match pt with
            | L.TypeKind.Pointer ->
                if
                  a_val |> L.type_of |> L.element_type |> L.classify_type
                  = L.TypeKind.Array
                then L.build_gep a_val [| llvm_zero; llvm_zero |] "" builder
                else if
                  a_val |> L.type_of |> L.element_type |> L.classify_type
                  = L.TypeKind.Pointer
                then L.build_load a_val "" builder
                else a_val
            | _ -> L.build_load a_val "" builder)
        | _ ->
            let e_val = codegen_expr scope builder e in
            if
              L.type_of e_val |> L.element_type |> L.classify_type
              = L.TypeKind.Array
            then
              (* string literal *)
              L.build_gep e_val [| llvm_zero; llvm_zero |] "" builder
            else e_val
      in
      let fparams = L.params actual_f |> Array.to_list in
      let llvm_params =
        params
        |> List.map2 (codegen_call_expr scope builder) fparams
        |> List.map2
             (fun p e ->
               if L.is_undef e then L.const_pointer_null (L.type_of p) else e)
             fparams
      in
      L.build_call actual_f (Array.of_list llvm_params) "" builder

and codegen_access scope builder a =
  match a.node with
  | AccVar i -> (
      match Symbol_table.lookup i scope.var_symbols with
      | Some v -> v
      | None -> Util.raise_codegen_error @@ "Variable " ^ i ^ " not defined")
  | AccDeref e -> codegen_expr scope builder e
  | AccIndex (a, i) -> (
      let a_val = codegen_access scope builder a in
      let ind = codegen_expr scope builder i in
      let at = a_val |> L.type_of in
      match at |> L.classify_type with
      | L.TypeKind.Pointer -> (
          match at |> L.element_type |> L.classify_type with
          | L.TypeKind.Array ->
              L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder
          | _ ->
              let load_val = Llvm.build_load a_val "" builder in
              Llvm.build_in_bounds_gep load_val [| ind |] "" builder)
      | _ -> L.build_in_bounds_gep a_val [| llvm_zero; ind |] "" builder)
  | AccField (a, f) ->
      let a_val = codegen_access scope builder a in
      let sname = L.type_of a_val |> L.element_type |> L.struct_name in
      if not (Option.is_none sname) then
        match Symbol_table.lookup (Option.get sname) scope.struct_symbols with
        | Some (t, fields) ->
            let to_index = List.mapi (fun i m -> (m, i)) fields in
            let field_pos =
              match List.assoc_opt f to_index with
              | Some index -> index
              | None ->
                  Util.raise_codegen_error @@ "Undefined struct member " ^ f
            in
            L.build_struct_gep a_val field_pos "" builder
        | None ->
            Util.raise_codegen_error @@ "Undefined struct " ^ Option.get sname
      else assert false

let add_terminator builder after =
  let terminator = L.block_terminator (L.insertion_block builder) in
  if Option.is_none terminator then after builder |> ignore else ()

let rec codegen_stmt fdef scope builder stmt =
  let build_while choose_block condition body =
    let cond_block = L.append_block llcontext "test" fdef in
    let body_block = L.append_block llcontext "while_body" fdef in
    let cont_block = L.append_block llcontext "cont" fdef in
    let cond_builder = L.builder_at_end llcontext cond_block in
    let body_builder = L.builder_at_end llcontext body_block in
    choose_block (cond_block, body_block)
    |> L.build_br |> add_terminator builder |> ignore;
    let e_val = codegen_expr scope cond_builder condition in
    L.build_cond_br e_val body_block cont_block cond_builder |> ignore;
    codegen_stmt fdef scope body_builder body |> ignore;
    L.build_br cond_block |> add_terminator body_builder;
    L.position_at_end cont_block builder
  in

  match stmt.node with
  | If (e, st1, st2) ->
      let blockt = L.append_block llcontext "then" fdef in
      let blockelse = L.append_block llcontext "else" fdef in
      let blockcont = L.append_block llcontext "cont" fdef in
      let then_builder = L.builder_at_end llcontext blockt in
      let else_builder = L.builder_at_end llcontext blockelse in
      let e1 = codegen_expr scope builder e in
      L.build_cond_br e1 blockt blockelse builder |> ignore;

      codegen_stmt fdef scope then_builder st1 |> ignore;
      add_terminator then_builder (L.build_br blockcont);

      codegen_stmt fdef scope else_builder st2 |> ignore;
      add_terminator else_builder (L.build_br blockcont);
      L.position_at_end blockcont builder;
      true
  | Expr e ->
      codegen_expr scope builder e |> ignore;
      true
  | Block b ->
      let new_scope =
        { scope with var_symbols = Symbol_table.begin_block scope.var_symbols }
      in
      List.fold_left
        (fun cont bl ->
          if cont then codegen_stmtordec fdef new_scope builder bl else false)
        true b
  | Return e ->
      (if Option.is_none e then L.build_ret_void |> add_terminator builder
      else
        let e_val = Option.get e |> codegen_expr scope builder in
        let v =
          if L.is_undef e_val then
            L.const_pointer_null (L.type_of fdef |> L.pointer_type)
          else e_val
        in
        v |> L.build_ret |> add_terminator builder);
      false
  | While (e, s) ->
      build_while fst e s;
      true
  | DoWhile (e, s) ->
      build_while snd e s;
      true

and codegen_stmtordec fdef scope builder st =
  match st.node with
  | Dec (t, i, init) ->
      let var_v =
        L.build_alloca (build_llvm_type scope.struct_symbols t) i builder
      in
      let get_init_val e =
        let e_val = codegen_expr scope builder e in
        if
          L.type_of e_val |> L.element_type |> L.classify_type
          = L.TypeKind.Array
        then e_val
        else
          let value =
            if L.is_undef e_val then
              L.const_pointer_null (build_llvm_type scope.struct_symbols t)
            else e_val
          in
          L.build_store value var_v builder |> ignore;
          var_v
      in
      let actual_value = Option.fold ~none:var_v ~some:get_init_val init in
      Symbol_table.add_entry i actual_value scope.var_symbols |> ignore;
      true
  | Stmt s -> codegen_stmt fdef scope builder s

let codegen_func llmodule scope func =
  let ret_type = build_llvm_type scope.struct_symbols func.typ in
  let formals_types =
    func.formals |> List.map fst
    |> List.map (fun t ->
           match t with
           | TypA (t1, _) ->
               build_llvm_type scope.struct_symbols t1 |> L.pointer_type
           | _ -> build_llvm_type scope.struct_symbols t)
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
  let f_builder = L.entry_block f |> L.builder_at_end llcontext in
  let build_param scope builder (t, i) p =
    match t with
    | TypA (t1, _) ->
        let l =
          L.build_alloca
            (build_llvm_type scope.struct_symbols t1 |> L.pointer_type)
            "" builder
        in
        Symbol_table.add_entry i l scope.var_symbols |> ignore;
        L.build_store p l builder |> ignore
    | _ ->
        let l =
          L.build_alloca (build_llvm_type scope.struct_symbols t) "" builder
        in
        Symbol_table.add_entry i l scope.var_symbols |> ignore;
        L.build_store p l builder |> ignore
  in
  List.iter2
    (build_param local_scope f_builder)
    func.formals
    (L.params f |> Array.to_list);
  codegen_stmt f local_scope f_builder func.body |> ignore;
  match func.typ with
  | TypV -> add_terminator f_builder L.build_ret_void
  | _ -> add_terminator f_builder (ret_type |> L.undef |> L.build_ret)

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
      let named_s = L.named_struct_type llcontext s.sname in
      Symbol_table.add_entry s.sname
        (named_s, s.fields |> List.map snd)
        scope.struct_symbols
      |> ignore;
      let fields_t =
        s.fields
        |> List.map (fun (t, _) -> build_llvm_type scope.struct_symbols t)
      in
      L.struct_set_body named_s (Array.of_list fields_t) false

let add_rt_support llmodule scope =
  let params_to_array params =
    params |> List.map fst
    |> List.map (build_llvm_type scope.struct_symbols)
    |> Array.of_list
  in
  let fun_type f =
    L.function_type
      (build_llvm_type scope.struct_symbols f.typ)
      (params_to_array f.formals)
  in
  Util.rt_support
  |> List.map (fun (n, (_, f)) -> (n, fun_type f))
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
      fun_symbols = Symbol_table.empty_table ();
      var_symbols = Symbol_table.empty_table ();
      struct_symbols = Symbol_table.empty_table ();
    }
  in
  add_rt_support llmodule init_scope;
  List.iter (codegen_topdecl llmodule init_scope) topdecls;
  llmodule
