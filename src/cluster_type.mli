type t =
  | AdHoc
  | MapReduce
[@@deriving sexp, bin_io]

val maybe_t_of_string : string -> t option
val string_of_t : t -> string
val default : t
val provider : (module Application_class.Provider with type t = t)
