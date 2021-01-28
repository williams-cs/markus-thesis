open Core

type t = (and_or_list * separator) list

and token_part =
  | Literal of string
  | Variable of string
  | Command_substitution of t

and token = token_part list

and io_file_op =
  (* < *)
  | Less
  (* <& *)
  | Lessand
  (* > *)
  | Great
  (* >& *)
  | Greatand
  (* >> *)
  | Dgreat
  (* <> *)
  | Lessgreat
  (* >| *)
  | Clobber

and io_here_op =
  (* << *)
  | Dless
  (* <<- *)
  | Dlessdash

and io_redirect_part =
  | Io_file of io_file_op * token
  | Io_here of io_here_op * string

and io_redirect = int option * io_redirect_part

and assignment = string * token

and simple_command = token list * assignment list * io_redirect list

and case_item = string list * t

and remote_command =
  | Remote_subshell of t
  | Remote_name of string

and command =
  | Simple_command of simple_command
  | If_clause of (t * t) list * t option
  | Subshell of t
  (* Remote extension *)
  | Remote_command of remote_command * string

and pipeline = bang option * command list

and and_or_list = pipeline * (and_or * pipeline) list

and bang = Bang

and and_or =
  | And
  | Or

and separator =
  | Ampersand
  | Semicolon
[@@deriving sexp]

val parse : string -> t Or_error.t

val parse_partial
  :  ?state:t Angstrom_extended.Buffered.state
  -> string
  -> t Angstrom_extended.Buffered.state

val parse_field_split : string -> string list
