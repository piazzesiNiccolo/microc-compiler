exception DuplicateEntry

type 'a t = Empty_table | Table of 'a t * (string,'a) Hashtbl.t

let empty_table = Empty_table 

let begin_block table = 
  match table with
  |Empty_table -> Table(Empty_table, (Hashtbl.create 0))
  | v -> Table(v, (Hashtbl.create 0))

let end_block table = 
  match table with
  | Empty_table -> Empty_table
  | Table(p,_) -> p

let add_entry symbol info table =
  match table with
  | Empty_table -> failwith "No scope currently defined"
  | Table(p,t) as x->
        match Hashtbl.find_opt t symbol with
        | None -> Hashtbl.add t symbol info;x 
        | Some(_) -> raise DuplicateEntry

let rec lookup symbol table = 
      match table with
      | Empty_table -> raise Not_found
      | Table(p,t) ->
        match Hashtbl.find_opt t symbol with
        | None -> lookup symbol p
        | Some(v) -> v 

