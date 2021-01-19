type t

val create : working_directory:string -> t
val copy : t -> t

(** Returns empty string if no assignment *)
val assign_get : t -> key:string -> string

(** Returns old assigned value *)
val assign_set : t -> key:string -> data:string -> string

(** Changes the working directory *)
val cd : t -> dir:string -> unit

(** Returns the working directory *)
val cwd : t -> string

val export_add : t -> key:string -> unit
val exports : t -> string list
val exports_print : t -> write_callback:(string -> unit) -> unit
