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

let codegen_func llmodule scope func =
  let ret_type = build_llvm_type scope.struct_symbols func.typ in
  let formals_types =
    func.formals |> List.map fst
    |> List.map (build_llvm_type scope.struct_symbols)
  in
  let f_type = L.function_type ret_type (Array.of_list formals_types) in
  let f = L.define_function func.fname f_type llmodule in
  Symbol_table.add_entry func.fname f scope.fun_symbols  |> ignore

let codegen_topdecl llmodule scope n =
  match n.node with
  | Fundecl f -> codegen_func llmodule scope f |> ignore
  | Vardec (t, i, None) -> ()
  | Vardec (t, i, Some v) -> ()
  | Structdecl s -> 
    let fields_t = s.fields |> List.map (fun (t, f) -> (build_llvm_type scope.struct_symbols t,f)) in 
    let struct_t = L.struct_type llcontext (Array.of_list (List.map fst fields_t)) in 
    Symbol_table.add_entry s.sname (struct_t, fields_t) scope.struct_symbols |> ignore

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
