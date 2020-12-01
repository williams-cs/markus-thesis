open Core

type t = (and_or_list * separator) list

and token_part =
  | Literal of string
  | Variable of string
  | Command_substitution of t

and token = token_part list

and simple_command = token list

and case_item = string list * t

and command =
  | Simple_command of simple_command
  | Subshell of t

and pipeline = bang option * command list

and and_or_list = pipeline * (and_or * pipeline) list

and bang = Bang

and and_or =
  | And
  | Or

and separator =
  | Ampersand
  | Semicolon

val parse : string -> t Or_error.t
val parse_field_split : string -> string list
