exception DuplicateEntry

type 'a t = Dummy | Table of 'a t * (string, 'a) Hashtbl.t

let empty_table () =  Table(Dummy,Hashtbl.create 0)

let begin_block parent =
  Table(parent,Hashtbl.create 0)

let end_block table =
  match table with Dummy -> Dummy | Table (p, _) -> p

let add_entry symbol info table =
  match table with
  | Dummy -> failwith "No scope currently defined"
  | Table (_, t) as x -> (
      match Hashtbl.find_opt t symbol with
      | None ->
          Hashtbl.add t symbol info;
          x
      | Some _ -> raise DuplicateEntry)

let rec lookup symbol table =
  match table with
  | Dummy -> None
  | Table (p, t) -> (
      match Hashtbl.find_opt t symbol with
      | None -> lookup symbol p
      | Some v -> Some v)
