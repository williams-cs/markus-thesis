open Core
open Async

val builtins
  : (env:Env.t
     -> stdout:Writer.t
     -> stderr:Writer.t
     -> args:string list
     -> int Deferred.t)
    String.Map.t

val export_single : string -> env:Env.t -> unit
