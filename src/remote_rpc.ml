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

module Response = struct
  type t =
    | Write_callback of bytes * int
    | Close_callback
  [@@deriving bin_io]
end

let rpc =
  Pipe_rpc.create
    ~name:"shard_ssh_client_rpc"
    ~version:1
    ~bin_query:Query.bin_t
    ~bin_response:Response.bin_t
    ~bin_error:Error.bin_t
    ()
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
             Remote.remote_run
               ~host
               ~program
               ~write_callback:(fun b len ->
                 Pipe.write_without_pushback pipe (Response.Write_callback (b, len)))
               ~close_callback:(fun () ->
                 Pipe.write_without_pushback pipe Response.Close_callback);
             Pipe.close pipe)))
;;

let implementations =
  Implementations.create_exn
    ~implementations:[ Pipe_rpc.implement rpc handle_query ]
    ~on_unknown_rpc:`Raise
;;

let run_server port =
  Connection.serve
    ~implementations
    ~initial_connection_state:(fun _addr conn -> conn)
    ~where_to_listen:(Tcp.Where_to_listen.of_port port)
    ()
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

let setup_rpc_service () =
  (* TODO *)
  (* copy executable to shard folder *)
  (* run copied executable with command line argument, which runs the server *)
  (* run the client *)
  (* return the connection *)
  let port = 60273 in
  let host = "localhost" in
  run_server port |> ignore;
  run_client host port
;;

let deferred_or_error_swap v =
  match v with
  | Ok x ->
    let%map x = x in
    Or_error.return x
  | Error err -> return (Result.fail err)
;;

let remote_run ~host ~program ~write_callback ~close_callback =
  (let open Deferred.Or_error.Let_syntax in
  let%bind conn = setup_rpc_service () in
  let%map resp = dispatch conn ~host ~program in
  let reader, _metadata = resp in
  Pipe.iter ~continue_on_error:true reader ~f:(fun response ->
      match response with
      | Write_callback (b, len) -> write_callback b len
      | Close_callback -> close_callback ()))
  |> Deferred.map ~f:deferred_or_error_swap
  |> Deferred.join
;;
