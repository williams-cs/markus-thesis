module Stable = struct
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

  let string_of_t t =
    match t with
    | AdHoc -> "adhoc"
    | MapReduce -> "mapreduce"
  ;;

  let default = AdHoc

  let application_class_backend_of_t t =
    match t with
    | AdHoc -> Single_command_backend.create ()
    | MapReduce -> Single_command_backend.create ()
  ;;
end

include Stable

let provider = (module Stable : Application_class.Provider with type t = t)
