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
    | Close_callback of unit Or_error.t
  [@@deriving bin_io]
end

module State = struct
  type t =
    { verbose : bool
    ; close_ivar : unit Ivar.t
    }
  [@@deriving fields]

  let create ~verbose = { verbose; close_ivar = Ivar.create () }
end

let verbose_println ~verbose host str =
  if verbose then print_endline (sprintf "[ShardRPC-%s] " host ^ str)
;;

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

let handle_query state query =
  Deferred.Or_error.return
    (Pipe.create_reader ~close_on_exception:true (fun pipe ->
         In_thread.run (fun () ->
             let host = Query.host query in
             let program =
               match Query.program query with
               | Name s -> `Name s
               | Sexp s -> `Sexp s
             in
             let verbose = State.verbose state in
             let res =
               Remote_ssh.remote_run
                 ~host
                 ~program
                 ~verbose
                 ~write_callback:(fun b len ->
                   Pipe.write_without_pushback pipe (Response.Write_callback (b, len)))
                 ~close_callback:(fun () ->
                   Pipe.write_without_pushback pipe (Response.Close_callback (Ok ())))
             in
             Or_error.iter_error res ~f:(fun err ->
                 Pipe.write_without_pushback pipe (Response.Close_callback (Error err)));
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

let run_server state =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr _conn -> state)
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ()
;;

let start_server ~verbose =
  let state = State.create ~verbose in
  let%bind tcp = run_server state in
  (* Signal to the client that the connection is ready *)
  let port = Tcp.Server.listening_on tcp in
  print_endline (Int.to_string port);
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

let setup_rpc_service ~host ~stderr ~verbose ~job =
  let continue_if_should_connect f =
    if Job.should_connect job
    then f ()
    else Deferred.Or_error.error_string "Should not connect on attempted connection!"
  in
  continue_if_should_connect (fun () ->
      verbose_println ~verbose host "Copying local executable...";
      let%bind local_path =
        (* copy executable to shard folder *)
        In_thread.run (fun () ->
            let local_path, _hash = Remote_ssh.local_copy ~verbose ~host in
            local_path)
      in
      let host = "localhost" in
      let prog = local_path in
      let args = [ "-r" ] @ if verbose then [ "-V" ] else [] in
      (* run copied executable with command line argument, which runs the server *)
      continue_if_should_connect (fun () ->
          verbose_println ~verbose host "Starting RPC communicator...";
          let%bind.Deferred.Or_error process = Process.create ~prog ~args ~stdin:"" () in
          Job.connect job process;
          let child_stdout = Process.stdout process in
          let child_stderr = Process.stderr process in
          let _err_deferred = Util.glue' ~reader:child_stderr ~writer:stderr in
          (* let _out_deferred = Util.glue' ~reader:child_stdout ~writer:stderr in *)
          let%bind ready_string = Reader.read_line child_stdout in
          match ready_string with
          | `Eof ->
            return (Or_error.error_string "EOF reached when waiting for ready port")
          | `Ok port_string ->
            verbose_println ~verbose host (sprintf "RPC port: %s" port_string);
            let port = Int.of_string port_string in
            let _out_deferred = Util.glue' ~reader:child_stdout ~writer:stderr in
            (* run the client and return the connection *)
            verbose_println ~verbose host "Starting RPC client...";
            run_client host port))
;;

let deferred_or_error_swap v =
  match v with
  | Ok x ->
    let%map x = x in
    Or_error.return x
  | Error err -> return (Result.fail err)
;;

let remote_run ~host ~program ~verbose ~write_callback ~close_callback ~stderr =
  let job = Job.create () in
  (* UUID key should be unique *)
  (let open Deferred.Or_error.Let_syntax in
  let%bind conn = setup_rpc_service ~host ~stderr ~verbose ~job in
  let%map resp = dispatch conn ~host ~program in
  let reader, _metadata = resp in
  let%bind.Deferred maybe_error =
    Pipe.fold reader ~init:(Ok ()) ~f:(fun accum response ->
        match response with
        | Write_callback (b, len) ->
          let%bind.Deferred () = write_callback b len in
          Deferred.return accum
        | Close_callback err ->
          let%bind.Deferred () = close_callback () in
          Deferred.return err)
  in
  Job.complete job;
  let%bind () = dispatch_close conn in
  Deferred.return maybe_error)
  |> Deferred.map ~f:deferred_or_error_swap
  |> Deferred.join
  |> Deferred.map ~f:Or_error.join
;;
