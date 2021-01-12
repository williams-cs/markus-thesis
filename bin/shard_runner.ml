open Core
open Async

let () =
  Shard.run () |> ignore;
  never_returns (Scheduler.go ())
;;
