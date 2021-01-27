open Core
open Async

module Job_status = struct
  type t =
    | Not_connected
    | Connected of Process.t
    | Canceled
    | Complete
end

type t =
  { status : Job_status.t ref
  ; id : string
  }
[@@deriving fields]

let kill process = Process.send_signal process Signal.kill

let cancel (t : t) =
  let status = status t in
  match !status with
  | Not_connected -> status := Canceled
  | Connected process ->
    kill process;
    status := Canceled
  | Complete | Canceled -> ()
;;

let cancel_without_signal (t : t) =
  let status = status t in
  match !status with
  | Not_connected -> status := Canceled
  | Connected _process -> status := Canceled
  | Complete | Canceled -> ()
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
  | Complete | Canceled -> kill process
;;

let should_connect (t : t) =
  let status = status t in
  match !status with
  | Not_connected | Connected _ -> true
  | Complete | Canceled -> false
;;

let canceled (t : t) =
  let status = status t in
  match !status with
  | Canceled -> true
  | Not_connected | Connected _ | Complete -> false
;;

module Job_group = struct
  type t_inner =
    { jobs : (string, t) Hashtbl.t
    ; canceled : bool ref
    }

  let create () = { jobs = Hashtbl.create (module String); canceled = ref false }
  let add { jobs; canceled = _ } ~job = Hashtbl.add_exn jobs ~key:(id job) ~data:job

  let cancel { jobs; canceled } =
    Hashtbl.iter jobs ~f:(fun job -> cancel job);
    canceled := true
  ;;

  let canceled { jobs = _; canceled } = !canceled

  let reset { jobs; canceled } =
    canceled := false;
    (* TODO: cancel existing? *)
    Hashtbl.clear jobs
  ;;

  type t = t_inner
end

let all_jobs = Job_group.create ()
let random_session_key () = Uuid.create_random (Util.random_state ()) |> Uuid.to_string

let create ?groups () : t =
  let key = random_session_key () in
  let t = { status = ref Job_status.Not_connected; id = key } in
  let groups =
    match groups with
    | Some groups -> groups
    | None -> []
  in
  let groups = all_jobs :: groups in
  List.iter groups ~f:(fun group -> Job_group.add group ~job:t);
  t
;;
