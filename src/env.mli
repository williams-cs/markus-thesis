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
