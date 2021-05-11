open Core
open Async

(* times in ms *)
let interval = 500
let initial_delay = 30000
let local_delay = 10000
let remote_delay = 30000
let enable_keepalive = true

let schedule dispatch =
  if enable_keepalive
  then (
    let stop_ivar = Ivar.create () in
    Clock.run_at_intervals'
      ~start:(Time.now ())
      ~stop:(Ivar.read stop_ivar)
      (Time.Span.of_ms (Int.to_float interval))
      (fun () ->
        (* TODO debug *)
        (* fprintf (force Writer.stderr) "keepalive once\n"; *)
        let%bind res = dispatch () in
        match res with
        | Ok () -> return ()
        | Error err ->
          (* TODO debug *)
          fprintf (force Writer.stderr) "keepalive ending\n";
          fprintf (force Writer.stderr) "%s\n" (Error.to_string_hum err);
          Ivar.fill stop_ivar ();
          return ()))
  else ()
;;

module Chain = struct
  type t =
    { deferred : unit Deferred.t
    ; next : t Ivar.t
    }
  [@@deriving fields]

  let fill t filling = Ivar.fill (next t) filling
  let create deferred = { deferred; next = Ivar.create () }

  let append_one previous deferred =
    let t = create deferred in
    fill previous t;
    t
  ;;

  let rec append t deferred =
    let next_ivar = next t in
    if Ivar.is_full next_ivar
    then append (Ivar.value_exn next_ivar) deferred
    else append_one t deferred
  ;;

  let rec wait t =
    let next_ivar = next t in
    let%bind res =
      if Ivar.is_full next_ivar
      then (
        let%bind t = Ivar.read next_ivar in
        return (`Next t))
      else
        Deferred.choose
          [ Deferred.choice (deferred t) (fun () -> `Current)
          ; Deferred.choice (Ivar.read next_ivar) (fun t -> `Next t)
          ]
    in
    match res with
    | `Current -> return ()
    | `Next t -> wait t
  ;;
end
