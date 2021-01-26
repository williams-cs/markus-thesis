open Core
open Async
open Rpc

module Query = struct
  module Program = struct
    type t =
      | Sexp of Sexp.t
      | Name of string
    [@@deriving bin_io]
  end

  type t =
    { host : string
    ; program : Program.t
    }
  [@@deriving bin_io, fields]
end

module Close_query = struct
  type t = Close [@@deriving bin_io]
end

module Response = struct
  type t =
    | Write_callback of bytes * int
    | Close_callback
  [@@deriving bin_io]
end

module State = struct
  type t = { close_ivar : unit Ivar.t } [@@deriving fields]

  let create () = { close_ivar = Ivar.create () }
end

let rpc =
  Pipe_rpc.create
    ~name:"shard_ssh_client_rpc_dispatch"
    ~version:1
    ~bin_query:Query.bin_t
    ~bin_response:Response.bin_t
    ~bin_error:Error.bin_t
    ()
;;

let close_rpc =
  Rpc.create
    ~name:"shard_ssh_client_rpc_close"
    ~version:1
    ~bin_query:Close_query.bin_t
    ~bin_response:Close_query.bin_t
;;

let handle_query _state query =
  Deferred.Or_error.return
    (Pipe.create_reader ~close_on_exception:true (fun pipe ->
         In_thread.run (fun () ->
             let host = Query.host query in
             let program =
               match Query.program query with
               | Name s -> `Name s
               | Sexp s -> `Sexp s
             in
             Remote_unsafe.remote_run
               ~host
               ~program
               ~write_callback:(fun b len ->
                 Pipe.write_without_pushback pipe (Response.Write_callback (b, len)))
               ~close_callback:(fun () ->
                 Pipe.write_without_pushback pipe Response.Close_callback);
             Pipe.close pipe)))
;;

let handle_close state query =
  let ivar = State.close_ivar state in
  Ivar.fill_if_empty ivar ();
  return query
;;

let implementations =
  Implementations.create_exn
    ~implementations:
      [ Pipe_rpc.implement rpc handle_query; Rpc.implement close_rpc handle_close ]
    ~on_unknown_rpc:`Raise
;;

let run_server port state =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr _conn -> state)
    ~where_to_listen:(Tcp.Where_to_listen.of_port port)
    ()
;;

let ready_message = "ready"

let start_server ~port =
  let state = State.create () in
  let%bind _tcp = run_server port state in
  (* Signal to the client that the connection is ready *)
  print_endline ready_message;
  let ivar = State.close_ivar state in
  Ivar.read ivar
;;

let run_client host port =
  let conn =
    Connection.client
      (Tcp.Where_to_connect.of_host_and_port (Host_and_port.create ~host ~port))
  in
  Deferred.Result.map_error ~f:(fun exn -> Error.of_exn exn) conn
;;

let dispatch conn ~host ~program =
  let program =
    match program with
    | `Sexp s -> Query.Program.Sexp s
    | `Name s -> Query.Program.Name s
  in
  let query = { Query.host; program } in
  let%map response = Pipe_rpc.dispatch rpc conn query in
  Or_error.join response
;;

let dispatch_close conn =
  let query = Close_query.Close in
  Rpc.dispatch close_rpc conn query |> Deferred.Or_error.ignore_m
;;

let setup_rpc_service ~host ~stderr ~job =
  let continue_if_should_connect f =
    if Job.should_connect job
    then f ()
    else Deferred.Or_error.error_string "Should not connect on attempted connection!"
  in
  continue_if_should_connect (fun () ->
      let%bind local_path =
        (* copy executable to shard folder *)
        In_thread.run (fun () ->
            let local_path, _hash = Remote_unsafe.local_copy ~host in
            local_path)
      in
      let port = 60273 in
      let host = "localhost" in
      let prog = local_path in
      let args = [ "-r"; Int.to_string port ] in
      (* run copied executable with command line argument, which runs the server *)
      continue_if_should_connect (fun () ->
          let%bind.Deferred.Or_error process = Process.create ~prog ~args ~stdin:"" () in
          Job.connect job process;
          let child_stdout = Process.stdout process in
          let child_stderr = Process.stderr process in
          let _err_deferred = Util.glue' ~reader:child_stderr ~writer:stderr in
          (* let _out_deferred = Util.glue' ~reader:child_stdout ~writer:stderr in *)
          let%bind _ready_string = Reader.read_line child_stdout in
          let _out_deferred = Util.glue' ~reader:child_stdout ~writer:stderr in
          (* run the client and return the connection *)
          run_client host port))
;;

let deferred_or_error_swap v =
  match v with
  | Ok x ->
    let%map x = x in
    Or_error.return x
  | Error err -> return (Result.fail err)
;;

let remote_run ~host ~program ~write_callback ~close_callback ~stderr =
  let job = Job.create () in
  (* UUID key should be unique *)
  (let open Deferred.Or_error.Let_syntax in
  let%bind conn = setup_rpc_service ~host ~stderr ~job in
  let%map resp = dispatch conn ~host ~program in
  let reader, _metadata = resp in
  let%bind.Deferred () =
    Pipe.iter ~continue_on_error:true reader ~f:(fun response ->
        match response with
        | Write_callback (b, len) -> write_callback b len
        | Close_callback -> close_callback ())
  in
  Job.complete job;
  dispatch_close conn)
  |> Deferred.map ~f:deferred_or_error_swap
  |> Deferred.join
  |> Deferred.map ~f:Or_error.join
;;
