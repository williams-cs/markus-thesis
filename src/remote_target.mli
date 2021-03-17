type t =
  { host : string
  ; port : int option
  ; setting : string
  }
[@@deriving fields, sexp, bin_io, compare, hash]

val create : host:string -> port:int option -> setting:string -> t
val to_string : t -> string
val with_setting : t -> setting:string -> t
val split_remote_target_setting : string -> string * string
val resolve : string -> t option
