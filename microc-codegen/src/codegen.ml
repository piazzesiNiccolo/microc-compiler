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
    fun_symbols: L.llvalue Symbol_table.t
  ; var_symbols: L.llvalue Symbol_table.t
  ; struct_symbols: struct_type Symbol_table.t
}

let to_ir (Prog(topdecls)) =
  let module_name = "microc_module" in
  let llmodule = L.create_module llcontext module_name in
  llmodule

