open Core
open Async

module Cluster_info = struct
  type t =
    { connections : (Rpc_local_sender.t * Rpc_local_receiver.t) Deferred.t String.Table.t
    }

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
  (* let deferred_or_error_swap v =
    match v with
    | Ok x ->
      let%map x = x in
      Or_error.return x
    | Error err -> return (Result.fail err)
  ;; *)

  (* let remote_run_single
      ~cluster_id
      ~remote_target
      ~setting
      ~program
      ~env_image
      ~verbose
      ~stdin
      ~stdout
      ~stderr
    =
    let _cluster_id = cluster_id in
    let _setting = setting in
    let job = Job.create () in
    let sconn = ref None in
    let rconn = ref None in
    (* UUID key should be unique *)
    let host = Remote_target.host remote_target in
    let port = Remote_target.port remote_target in
    let%bind res1d =
      (let%bind.Deferred.Or_error sender_conn, receiver_conn =
         Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job
       in
       sconn := Some sender_conn;
       rconn := Some receiver_conn;
       let%bind.Deferred.Or_error remote_port =
         Rpc_local_sender.dispatch_open sender_conn ~host ~port ~program ~env_image
       in
       let%map.Deferred.Or_error resp =
         Rpc_local_receiver.dispatch receiver_conn ~host ~port ~remote_port
       in
       let reader, _metadata = resp in
       let _send =
         Reader.pipe stdin
         |> Pipe.fold ~init:(Or_error.return ()) ~f:(fun accum s ->
                match accum with
                | Error error -> return (Error error)
                | Ok () ->
                  let buf = Bytes.of_string s in
                  Rpc_local_sender.dispatch_write sender_conn ~buf ~amt:(Bytes.length buf))
       in
       let%bind maybe_error =
         Pipe.fold reader ~init:(Ok ()) ~f:(fun accum response ->
             match response with
             | Write_callback (b, len) ->
               let write_callback b len = return (Writer.write_bytes stdout b ~len) in
               let%bind.Deferred () = write_callback b len in
               return accum
             | Close_callback err ->
               let close_callback () = return () in
               let%bind.Deferred () = close_callback () in
               return err)
       in
       Job.complete job;
       return maybe_error)
      |> Deferred.map ~f:deferred_or_error_swap
      |> Deferred.join
      |> Deferred.map ~f:Or_error.join
    in
    let res2 =
      Option.map !rconn ~f:(fun conn -> Rpc_local_receiver.dispatch_close conn)
      |> Option.value ~default:Deferred.Or_error.ok_unit
    in
    let res3 =
      Option.map !sconn ~f:(fun conn -> Rpc_local_sender.dispatch_close conn)
      |> Option.value ~default:Deferred.Or_error.ok_unit
    in
    let%bind res2d = res2
    and res3d = res3 in
    Or_error.combine_errors [ res1d; res2d; res3d ] |> Or_error.map ~f:ignore |> return
  ;; *)

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
            let%map.Deferred.Or_error sender_conn, receiver_conn =
              Remote_rpc.setup_rpc_service ~host ~port ~stderr ~verbose ~job
            in
            Ivar.fill ivar (sender_conn, receiver_conn))
    in
    let dispatch_command_raw
        sender_conn
        receiver_conn
        host
        port
        program
        send_lines
        env_image
      =
      let bufsize = 1024 in
      let resp_buf = Buffer.create bufsize in
      let%bind.Deferred.Or_error remote_port =
        Rpc_local_sender.dispatch_open sender_conn ~host ~port ~program ~env_image
      in
      let%bind.Deferred.Or_error resp =
        Rpc_local_receiver.dispatch receiver_conn ~host ~port ~remote_port
      in
      let reader, _metadata = resp in
      let _send =
        Deferred.List.fold send_lines ~init:(Or_error.return ()) ~f:(fun accum s ->
            match accum with
            | Error error -> return (Error error)
            | Ok () ->
              let buf = Bytes.of_string s in
              Rpc_local_sender.dispatch_write sender_conn ~buf ~amt:(Bytes.length buf))
      in
      let%bind maybe_error =
        Pipe.fold reader ~init:(Ok ()) ~f:(fun accum response ->
            match response with
            | Write_callback (b, len) ->
              let write_callback b len =
                let bs = Bytes.sub b ~pos:0 ~len in
                Buffer.add_bytes resp_buf bs;
                return ()
              in
              let%bind.Deferred () = write_callback b len in
              return accum
            | Close_callback err ->
              let close_callback () = return () in
              let%bind.Deferred () = close_callback () in
              return err)
      in
      let contents_with_maybe_error = Or_error.map ~f:(fun () -> resp_buf) maybe_error in
      Deferred.return contents_with_maybe_error
    in
    let dispatch_map_reduce program env_image write_lines remote_target_string =
      let parse_remote_target_string s =
        let sl = String.split remote_target_string ~on:':' in
        match sl with
        | [ h ] -> Ok (h, None)
        | [ h; p ] -> Ok (h, Some (Int.of_string p))
        | _ -> Error (Error.raise_s [%message "Invalid remote target string" s])
      in
      let%bind.Deferred.Or_error host, port =
        Deferred.return (parse_remote_target_string remote_target_string)
      in
      let%bind sender_conn, receiver_conn =
        Option.value_exn (Cluster_info.get_conn cluster_info remote_target_string)
      in
      dispatch_command_raw
        sender_conn
        receiver_conn
        host
        port
        program
        write_lines
        env_image
    in
    match setting with
    | "map" ->
      (* Start processing inputs: For each line, send to some host in cluster
         for processing. Repeat until done. Stream back data line by line in
         key,value format.*)
      (* For now, iterate through the remote targets *)
      let rec loop remote_target_index =
        let%bind line = Reader.read_line stdin in
        match line with
        | `Eof -> Deferred.Or_error.return ()
        | `Ok line ->
          let remote_target = List.nth_exn remote_targets remote_target_index in
          let program = program in
          let env_image = Env_image.add_assignment env_image ~key:"in" ~data:line in
          let%bind.Deferred.Or_error res_raw =
            dispatch_map_reduce program env_image [] (remote_target_string remote_target)
          in
          let res_string = Buffer.contents res_raw in
          (* TODO better escaping? *)
          let dispatch_res =
            String.split res_string ~on:'\n'
            |> List.drop_last
            |> Option.value ~default:[]
            |> String.concat ~sep:","
          in
          let () = Writer.write_line stdout dispatch_res in
          let next_remote_target =
            (remote_target_index + 1) % List.length remote_targets
          in
          loop next_remote_target
      in
      loop 0
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
            | [ x; y ] -> Ok (x, y)
            | _ -> Error (Error.raise_s [%message "Invalid key-value pair" s])
          in
          let%bind.Deferred.Or_error pair = return (split_2 line) in
          let key, data = pair in
          Hashtbl.add_multi inputs ~key ~data;
          loop ()
      in
      let%bind.Deferred.Or_error () = loop () in
      let l = Hashtbl.to_alist inputs in
      Deferred.Or_error.List.iter l ~f:(fun (key, data) ->
          let remote_target_index = Util.simple_hash key % List.length remote_targets in
          let remote_target = List.nth_exn remote_targets remote_target_index in
          let program = program in
          let env_image = Env_image.add_assignment env_image ~key:"key" ~data:key in
          let%bind.Deferred.Or_error res_raw =
            dispatch_map_reduce
              program
              env_image
              data
              (remote_target_string remote_target)
          in
          let res = Buffer.contents res_raw in
          let res_lines = String.split_lines res in
          List.iter res_lines ~f:(fun line -> fprintf stdout "%s,%s\n" key line);
          Deferred.Or_error.return ())
    | _ ->
      return
        (Error.raise_s
           [%message
             "Invalid usage of data parallel backend: Usage: (command)@@map/host | \
              (command)@@reduce/host"])
  ;;
end

let create () = (module Application_class_impl : Application_class.Backend)
