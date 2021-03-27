open Core
open Async

type env = Unix.env [@@deriving sexp]

val pipe : unit -> Fd.t * Fd.t
val closed_read_fd : unit -> Fd.t

type t =
  { pid : Pid.t
  ; stdin : Fd.t
  ; stdout : Fd.t
  ; stderr : Fd.t
  ; prog : string
  ; args : string list
  ; working_dir : string option
  ; env : env
  ; wait : Unix.Exit_or_signal.t Deferred.t Lazy.t
  }
[@@deriving fields, sexp_of]

val create
  :  ?argv0:string (* -> ?buf_len:int *)
  -> ?env:env (** default is [`Extend []] *)
  -> ?prog_search_path:string list (* -> ?stdin:string *)
  -> ?working_dir:string
  -> prog:string
  -> args:string list
  -> in_fd:Fd.t
  -> out_fd:Fd.t
  -> err_fd:Fd.t
  -> unit
  -> t Async.Deferred.Or_error.t

val wait : t -> Unix.Exit_or_signal.t Deferred.t
