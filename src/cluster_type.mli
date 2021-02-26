type t =
  | AdHoc
  | MapReduce
[@@deriving sexp, bin_io]

val maybe_t_of_string : string -> t option
val default : t
