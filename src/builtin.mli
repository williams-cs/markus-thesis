open Core
open Async

val builtins : (env:Env.t -> args:string list -> int Deferred.t) String.Map.t
