open Core
open Async

module Job_status = struct
  type t =
    | Not_connected
    | Connected of Process.t list
    | Canceled
    | Complete
end

type t =
  { status : Job_status.t ref
  ; id : string
  }
[@@deriving fields]

let kill processes =
  List.map processes ~f:(fun process -> Process.send_signal process Signal.int) |> ignore
;;

let cancel (t : t) =
  let status = status t in
  match !status with
  | Not_connected -> status := Canceled
  | Connected processes ->
    kill processes;
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

let attach ~process (t : t) =
  let status = status t in
  match !status with
  | Connected old_processes -> status := Connected (process :: old_processes)
  | Not_connected | Complete | Canceled -> kill [ process ]
;;

let connect ?process (t : t) =
  let status = status t in
  match !status with
  | Not_connected -> status := Connected []
  | Connected old_processes ->
    kill old_processes;
    status := Connected []
  | Complete | Canceled ->
    kill [];
    Option.iter process ~f:(fun process -> attach t ~process)
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
  let all_jobs_inner = create ()
  let add { jobs; canceled = _ } ~job = Hashtbl.add_exn jobs ~key:(id job) ~data:job

  let cancel { jobs; canceled } =
    Hashtbl.iter jobs ~f:(fun job -> cancel job);
    canceled := true
  ;;

  let canceled { jobs = _; canceled } =
    if !(all_jobs_inner.canceled) then true else !canceled
  ;;

  let reset { jobs; canceled } =
    canceled := false;
    (* TODO: cancel existing? *)
    Hashtbl.clear jobs
  ;;

  type t = t_inner
end

let all_jobs = Job_group.all_jobs_inner
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
