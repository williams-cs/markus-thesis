open Core
open Async

val builtins : (string list -> unit Deferred.t) String.Map.t
