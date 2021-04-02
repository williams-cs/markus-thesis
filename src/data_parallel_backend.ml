open Core
open Async

module Cluster_info = struct
  module Connection = struct
    type t =
      { sender_conn : Rpc_local_sender.t
      ; receiver_conn : Rpc_local_receiver.t
      ; remote_port : int
      ; reader_map : id:string -> Reader.t Deferred.t
      }
    [@@deriving fields]
  end

  type t = { connections : Connection.t Deferred.t String.Table.t } [@@deriving fields]

  let create () = { connections = String.Table.create () }
  let get_conn { connections } conn = String.Table.find connections conn

  let add_conn { connections } conn sender_receiver_deferred =
    String.Table.add_exn connections ~key:conn ~data:sender_receiver_deferred
  ;;
end

let clusters : Cluster_info.t String.Table.t = String.Table.create ()

let remote_target_string target =
  let host = Remote_target.host target in
  let port = Remote_target.port target in
  let port_string =
    match port with
    | None -> ""
    | Some p -> Int.to_string p
  in
  sprintf "%s%s" host port_string
;;

module Application_class_impl = struct
  let remote_run
      ~cluster_id
      ~remote_targets
      ~setting
      ~program
      ~env_image
      ~verbose
      ~stdin
      ~stdout
      ~stderr
    =
    let job = Job.create () in
    (* Set up connection to hosts in cluster as necessary*)
    let cluster_info = String.Table.find clusters cluster_id in
    let cluster_info =
      match cluster_info with
      | None ->
        let info = Cluster_info.create () in
        String.Table.add_exn clusters ~key:cluster_id ~data:info;
        info
      | Some info -> info
    in
    let%bind.Deferred.Or_error () =
      Deferred.Or_error.List.iter ~how:`Parallel remote_targets ~f:(fun target ->
          let target_string = remote_target_string target in
          match Cluster_info.get_conn cluster_info target_string with
          | Some conn_deferred ->
            let%bind _wait_for_conn = conn_deferred in
            Deferred.Or_error.return ()
          | None ->
            let ivar = Ivar.create () in
            Cluster_info.add_conn cluster_info target_string (Ivar.read ivar);
            let host = Remote_target.host target in
            let port = Remote_target.port target in
            let%bind.Deferred.Or_error sender_conn, receiver_conn =
              Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job
            in
            let%bind.Deferred.Or_error remote_port =
              Rpc_local_sender.dispatch_open sender_conn ~host ~port
            in
            let%map.Deferred.Or_error resp =
              Rpc_local_receiver.dispatch' receiver_conn ~host ~port ~remote_port
            in
            let reader_map, _metadata, _deferred = resp in
            Ivar.fill
              ivar
              { Cluster_info.Connection.sender_conn
              ; receiver_conn
              ; remote_port
              ; reader_map
              })
    in
    let dispatch_command_raw connection program send_lines env_image =
      let sender_conn = Cluster_info.Connection.sender_conn connection in
      let bufsize = 1024 in
      let resp_buf = Buffer.create bufsize in
      let%bind.Deferred.Or_error id =
        Rpc_local_sender.dispatch_header sender_conn ~program ~env_image
      in
      let send =
        let%bind _res =
          Deferred.List.fold send_lines ~init:(Or_error.return ()) ~f:(fun accum s ->
              match accum with
              | Error error -> return (Error error)
              | Ok () ->
                let buf = Bytes.of_string (s ^ "\n") in
                Rpc_local_sender.dispatch_write
                  sender_conn
                  ~id
                  ~buf
                  ~amt:(Bytes.length buf))
        in
        Rpc_local_sender.dispatch_close_single sender_conn ~id
      in
      let reader_map = Cluster_info.Connection.reader_map connection in
      let%bind reader = reader_map ~id in
      let reader_pipe = Reader.pipe reader in
      let%bind () =
        Pipe.iter_without_pushback reader_pipe ~f:(fun s -> Buffer.add_string resp_buf s)
      in
      let%bind _send = send in
      Deferred.Or_error.return resp_buf
    in
    let dispatch_map_reduce program env_image write_lines remote_target_string =
      let%bind connection =
        Option.value_exn (Cluster_info.get_conn cluster_info remote_target_string)
      in
      dispatch_command_raw connection program write_lines env_image
    in
    match setting with
    | "map" ->
      (* Start processing inputs: For each line, send to some host in cluster
         for processing. Repeat until done. Stream back data line by line in
         key,value format.*)
      (* For now, iterate through the remote targets *)
      let rec loop remote_target_index deferreds =
        let%bind line = Reader.read_line stdin in
        match line with
        | `Eof -> return deferreds
        | `Ok line ->
          let remote_target = List.nth_exn remote_targets remote_target_index in
          let program = program in
          let env_image = Env_image.add_assignment env_image ~key:"in" ~data:line in
          let process =
            let%bind.Deferred.Or_error res_raw =
              dispatch_map_reduce
                program
                env_image
                []
                (remote_target_string remote_target)
            in
            let res_string = Buffer.contents res_raw in
            (* TODO better escaping? *)
            let dispatch_res =
              String.split res_string ~on:'\n'
              |> List.drop_last
              |> Option.value ~default:[]
              |> String.concat ~sep:","
            in
            Writer.write_line stdout dispatch_res;
            Writer.flushed stdout |> Deferred.map ~f:Or_error.return
          in
          let next_remote_target =
            (remote_target_index + 1) % List.length remote_targets
          in
          loop next_remote_target (process :: deferreds)
      in
      let%bind deferreds = loop 0 [] in
      let%map or_errors = Deferred.all deferreds in
      let%map.Or_error units = Or_error.combine_errors or_errors in
      ignore units
    | "reduce" ->
      (* Process inputs: For each line, send to host in cluster depending on
         key. When all lines are sent, send 'start' signal to hosts. Gather all
         results locally, sort by key, then output. *)
      let inputs : (string, string list) Hashtbl.t = Hashtbl.create (module String) in
      let rec loop () =
        let%bind line = Reader.read_line stdin in
        match line with
        | `Eof -> Deferred.Or_error.return ()
        | `Ok line ->
          let split_2 s =
            let sl = String.split s ~on:',' in
            match sl with
            | [] | [ _ ] -> Error (Error.raise_s [%message "Invalid key-value pair" s])
            | _ ->
              let v = List.last_exn sl in
              let kl = List.drop_last_exn sl in
              let k = String.concat ~sep:"," kl in
              Ok (k, v)
          in
          let%bind.Deferred.Or_error pair = return (split_2 line) in
          let key, data = pair in
          Hashtbl.add_multi inputs ~key ~data;
          loop ()
      in
      let%bind.Deferred.Or_error () = loop () in
      let l = Hashtbl.to_alist inputs in
      let deferreds =
        List.fold l ~init:[] ~f:(fun accum (key, data) ->
            let remote_target_index = Util.simple_hash key % List.length remote_targets in
            let remote_target = List.nth_exn remote_targets remote_target_index in
            let program = program in
            let env_image = Env_image.add_assignment env_image ~key:"key" ~data:key in
            let process =
              let%bind.Deferred.Or_error res_raw =
                dispatch_map_reduce
                  program
                  env_image
                  data
                  (remote_target_string remote_target)
              in
              let res = Buffer.contents res_raw in
              (* let res_lines = String.split_lines res in *)
              (* List.iter res_lines ~f:(fun line -> fprintf stdout "%s,%s\n" key line); *)
              Deferred.Or_error.return (key, res)
            in
            process :: accum)
      in
      let%map or_errors = Deferred.all deferreds in
      let%map.Or_error pairs = Or_error.combine_errors or_errors in
      let sorted =
        List.fold pairs ~init:String.Map.empty ~f:(fun accum (key, data) ->
            Map.add_exn accum ~key ~data)
      in
      Map.iteri sorted ~f:(fun ~key ~data ->
          let res_lines = String.split_lines data in
          List.iter res_lines ~f:(fun line -> fprintf stdout "%s,%s\n" key line))
    | _ ->
      return
        (Error.raise_s
           [%message
             "Invalid usage of data parallel backend: Usage: (command)@@map/host | \
              (command)@@reduce/host"])
  ;;
end

let create () = (module Application_class_impl : Application_class.Backend)
