type t =
  | AdHoc
  | MapReduce
[@@deriving sexp, bin_io]

let maybe_t_of_string s =
  match s with
  | "adhoc" -> Some AdHoc
  | "mapreduce" -> Some MapReduce
  | _ -> None
;;

let default = AdHoc
