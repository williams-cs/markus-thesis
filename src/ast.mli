open Core

type t =
  | Noop
  | Command of token * token list
  | Series of t * t

and token_part =
  | Literal of string
  | Variable of string
  | Subshell of t

and token = token_part list

val parse : string -> t Or_error.t
