open Core
open Async

val builtins : (string list -> int Deferred.t) String.Map.t
