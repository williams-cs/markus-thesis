open Core
open Async

module Job_status = struct
  type t =
    | Not_connected
    | Connected of Process.t
    | Cancelled
    | Complete
end

type t =
  { status : Job_status.t ref
  ; id : string
  }
[@@deriving fields]

let jobs : (string, t) Hashtbl.t = Hashtbl.create (module String)
let random_session_key () = Uuid.create_random (Util.random_state ()) |> Uuid.to_string

let create () : t =
  let key = random_session_key () in
  let t = { status = ref Job_status.Not_connected; id = key } in
  Hashtbl.add_exn jobs ~key ~data:t;
  t
;;

let kill process = Process.send_signal process Signal.kill

let cancel (t : t) =
  let status = status t in
  match !status with
  | Not_connected -> status := Cancelled
  | Connected process ->
    kill process;
    status := Cancelled
  | Complete | Cancelled -> ()
;;

let complete (t : t) =
  let status = status t in
  status := Complete
;;

let connect (t : t) process =
  let status = status t in
  match !status with
  | Not_connected -> status := Connected process
  | Connected old_process ->
    kill old_process;
    status := Connected process
  | Complete | Cancelled -> kill process
;;

let should_connect (t : t) =
  let status = status t in
  match !status with
  | Not_connected | Connected _ -> true
  | Complete | Cancelled -> false
;;

let cancel_all () = Hashtbl.iter jobs ~f:(fun job -> cancel job)
