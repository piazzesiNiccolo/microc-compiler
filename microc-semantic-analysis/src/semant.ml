open Ast
open Symbol_table

type pos = Lexing.position * Lexing.position
type var_info = pos * typ
type fun_info = 
  (* we keep builtins functions implemented externally separated from normal
  function declararions *)
    | External of typ * typ list 
    | Declaration of pos*fun_decl

type symbols = {fun_symbols: fun_info Symbol_table.t ; 
                var_symbols: var_info Symbol_table.t
                }

let check_func f scope loc =
  try
    Symbol_table.add_entry f.fname (Declaration(loc, f)) scope.fun_symbols |> ignore 
  with 
    Symbol_table.DuplicateEntry ->  "function "^ f.fname ^ " already defined" |> Util.raise_semantic_error loc 
  
let check_topdecl scope node = 
  match node.node with
  |Fundecl(f) ->  check_func f scope node.loc
                
  |Vardec(t,i) -> Symbol_table.add_entry i (node.loc, t) scope.var_symbols |> ignore

let dummy = Lexing.dummy_pos, Lexing.dummy_pos
let check_global_properties scope =
  let m =  Symbol_table.lookup "main" scope.fun_symbols in 
  match m with
  | Some Declaration(_,{typ=TypV;fname="main";formals=[]}) -> ignore
  | Some Declaration(_,{typ=TypI;fname="main";formals=[]}) -> ignore
  | Some _ -> Util.raise_semantic_error dummy "Invalid signature of main"
  | None -> Util.raise_semantic_error dummy " No main function defined"

  let prelude_functions =
  let init_scope = Symbol_table.empty_table |> Symbol_table.begin_block in 
  List.iter (fun (name, rt, p) -> Symbol_table.add_entry name (External(rt, p)) init_scope |> ignore) Util.prelude;
  init_scope

  let check (Prog(topdecls)) = 
      let toplevel_scope = {
        fun_symbols = prelude_functions;
        var_symbols = Symbol_table.empty_table |> Symbol_table.begin_block
      }
      in List.iter (check_topdecl toplevel_scope) topdecls;check_global_properties toplevel_scope |> ignore 
