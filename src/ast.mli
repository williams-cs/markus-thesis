open Core

type t =
  | Noop
  | Command of string * string list
  | Series of t * t

val parse : string -> t Or_error.t
