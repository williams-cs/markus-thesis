open Core
open Async

let clusters : Remote_cluster.t String.Table.t = String.Table.create ()

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
    let _start_time = Time_ns.now () in
    (* Set up connection to hosts in cluster as necessary*)
    let cluster_info = String.Table.find clusters cluster_id in
    let cluster_info =
      match cluster_info with
      | None ->
        let info = Remote_cluster.create () in
        String.Table.add_exn clusters ~key:cluster_id ~data:info;
        info
      | Some info -> info
    in
    let%bind.Deferred.Or_error () =
      Remote_cluster.init_targets cluster_info ~targets:remote_targets ~verbose ~stderr
    in
    match setting with
    | "map" ->
      (* Start processing inputs: For each line, send to some host in cluster
         for processing. Repeat until done. Stream back data line by line in
         key,value format.*)
      (* For now, iterate through the remote targets *)
      let rec loop deferreds =
        let%bind line = Reader.read_line stdin in
        match line with
        | `Eof -> return deferreds
        | `Ok line ->
          let program = program in
          let env_image = Env_image.add_assignment env_image ~key:"in" ~data:line in
          let process =
            let%bind.Deferred.Or_error res_string =
              Remote_cluster.run_task
                cluster_info
                ~target:`Any
                ~program
                ~env_image
                ~send_lines:[]
            in
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
          loop (process :: deferreds)
      in
      let%bind deferreds = loop [] in
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
            let program = program in
            let env_image = Env_image.add_assignment env_image ~key:"key" ~data:key in
            let process =
              let%bind.Deferred.Or_error res =
                Remote_cluster.run_task
                  cluster_info
                  ~target:`Any
                  ~program
                  ~env_image
                  ~send_lines:data
              in
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
