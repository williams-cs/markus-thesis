open Core
open Async

let () =
  Dresh_shell.run () |> ignore;
  never_returns (Scheduler.go ())
;;
