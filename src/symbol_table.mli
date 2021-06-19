exception DuplicateEntry

type 'a t

(*Since the table is polymorphic, i turned empty table into a function that takes unit 
to correctly initialize the symbol table with the type inferred by the call context *)
val empty_table : unit -> 'a t

val begin_block : 'a t -> 'a t

val end_block : 'a t -> 'a t

val add_entry : Ast.identifier -> 'a -> 'a t -> 'a t

(*i prefer to use an optional to explicitly declare that lookup may fail instead of an exception *)
val lookup : Ast.identifier -> 'a t -> 'a option
