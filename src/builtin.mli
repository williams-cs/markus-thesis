open Core
open Async

type t =
  | Function of
      (env:Cluster_type.t Env.t
       -> stdout:Writer.t
       -> stderr:Writer.t
       -> args:string list
       -> int Deferred.t)
  | Source

val builtins : t String.Map.t
val export_single : string -> env:Cluster_type.t Env.t -> unit
