open Core
open Base.Poly

(* Configuration *)
(* Default timeout: 3 seconds *)
let default_ssh_timeout = 3000000L
let debug = true
let ssh_debug = false
let version_string = "v0001"
let auth_publickey (ssh : Libssh.ssh) = ssh#userauth_publickey_auto ()

let debug_println ~verbose str =
  if debug && verbose
  then (
    fprintf stderr "%s\n" ("[Shard-debug] " ^ str);
    Out_channel.flush stderr)
;;

(* TODO: fix verify known hosts *)
let _verify_known_hosts ~(ssh : Libssh.ssh) ~verbose =
  let _spub = ssh#get_server_publickey () in
  let state = ssh#is_known_server () in
  match state with
  | Libssh.SSH_KNOWN_HOSTS_OK -> ()
  | Libssh.SSH_KNOWN_HOSTS_CHANGED ->
    debug_println ~verbose "Host key changed, connection will be aborted.";
    exit 1
  | Libssh.SSH_KNOWN_HOSTS_OTHER ->
    debug_println ~verbose "Host key not found, connection will be aborted.";
    exit 1
  | Libssh.SSH_KNOWN_HOSTS_NOT_FOUND | Libssh.SSH_KNOWN_HOSTS_UNKNOWN ->
    debug_println
      ~verbose
      "Server is unknown, please connect to it with ssh normally to add to known hosts.";
    exit 1
;;

let authenticate ssh =
  let rc = ssh#userauth_none () in
  match rc with
  | Libssh.SSH_AUTH_ERROR -> rc
  | _ ->
    let methods = ssh#userauth_list () in
    let auths = [ Libssh.SSH_AUTH_METHOD_PUBLICKEY, auth_publickey ] in
    let rec loop = function
      | [] -> Libssh.SSH_AUTH_DENIED
      | (typ, fn) :: xs ->
        if List.mem methods typ ~equal:(fun a b -> a = b)
        then (
          let rc = fn ssh in
          match rc with
          | Libssh.SSH_AUTH_ERROR -> rc
          | Libssh.SSH_AUTH_SUCCESS -> rc
          | _ -> loop xs)
        else loop xs
    in
    loop auths
;;

let connect ~host ~port ~verbose =
  let ssh = new Libssh.ssh () in
  let port_string =
    match port with
    | None -> "N/A"
    | Some i -> Int.to_string i
  in
  debug_println ~verbose (sprintf "ssh-connect: host: %s; port: %s" host port_string);
  ssh#options_set (Libssh.SSH_OPTIONS_HOST host);
  (match port with
  | None -> ()
  | Some i -> ssh#options_set (Libssh.SSH_OPTIONS_PORT i));
  ssh#options_set (Libssh.SSH_OPTIONS_TIMEOUT_USEC default_ssh_timeout);
  (* SSH debug mode *)
  if ssh_debug
  then ssh#options_set (Libssh.SSH_OPTIONS_LOG_VERBOSITY Libssh.SSH_LOG_DEBUG);
  ssh#options_parse_config None;
  (* verify_known_hosts ssh; *)
  ssh#connect ();
  debug_println ~verbose "Attempting to authenticate...";
  match authenticate ssh with
  | Libssh.SSH_AUTH_SUCCESS ->
    debug_println ~verbose "Success!";
    ssh
  | Libssh.SSH_AUTH_DENIED ->
    debug_println ~verbose "Access denied!";
    exit 1
  | _ ->
    debug_println ~verbose "Auth Error!";
    exit 1
;;

let send_copy ssh ~dir ~src ~dest =
  let file = Unix.openfile ~mode:[ Unix.O_RDONLY ] src in
  let stat = Unix.fstat file in
  let fsize = stat.st_size in
  let perms = 0744 in
  let scp = new Libssh.scp ssh Libssh.SSH_SCP_WRITE dir in
  scp#push_file dest fsize perms;
  let bufsize = 1024 in
  let buf = Bytes.make bufsize Char.min_value in
  let rec loop () =
    let amt = Unix.read file ~buf in
    scp#write buf 0 amt;
    if amt <> bufsize then () else loop ()
  in
  loop ();
  scp#close ()
;;

let read_fixed (channel : Libssh.channel) ~buf =
  try channel#read buf 0 (Bytes.length buf) with
  | End_of_file | Libssh.LibsshError _ -> 0
;;

let source tag =
  match tag with
  | `Sender -> "Shard_ssh_sender"
  | `Receiver -> "Shard_ssh_receiver"
  | `Copy -> "Shard_ssh_copy"
;;

let local_command str =
  let env = Array.create ~len:0 "" in
  let channels = Unix.open_process_full str ~env in
  let { Unix.Process_channels.stdout; _ } = channels in
  Stdio.In_channel.input_all stdout
;;

let hash_command str = sprintf "md5sum %s| cut -d ' ' -f 1" str

let local_copy ~host ~port ~verbose =
  let stderr = force Async.Writer.stderr in
  Util.verbose_println
    ~name:(source `Copy)
    ~verbose
    ~stderr
    ~host
    ~port
    "Looking for local copy of Shard...";
  let dir = sprintf "/tmp/shard/%s" version_string in
  let dest = sprintf "%s/shard.exe" dir in
  let dest_hash = local_command (hash_command dest) in
  let src = Unix.readlink "/proc/self/exe" in
  let src_hash = local_command (hash_command src) in
  if not (String.equal dest_hash src_hash)
  then (
    Util.verbose_println
      ~name:(source `Copy)
      ~verbose
      ~stderr
      ~host
      ~port
      "Local copy not found.";
    Util.verbose_println
      ~name:(source `Copy)
      ~verbose
      ~stderr
      ~host
      ~port
      "Installing local copy of Shard...";
    Unix.mkdir_p dir;
    Gc.compact ();
    local_command (sprintf "cp %s %s" src dest) |> ignore;
    Unix.chmod dest ~perm:0o744;
    Util.verbose_println
      ~name:(source `Copy)
      ~verbose
      ~stderr
      ~host
      ~port
      "Local installation complete!");
  dest, src_hash
;;

let remote_command ssh command =
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  channel#request_exec command;
  channel#send_eof ();
  channel#close ()
;;

let remote_command_fixed ssh command ~size =
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  channel#request_exec command;
  let buf = Bytes.make size Char.min_value in
  let amt = read_fixed channel ~buf in
  let res = String.slice (Bytes.to_string buf) 0 amt in
  channel#send_eof ();
  channel#close ();
  res
;;

let remote_command_output ssh command ~write_callback =
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  channel#request_exec command;
  let bufsize = 1024 in
  let buf = Bytes.make bufsize Char.min_value in
  (try
     while true do
       let amt = channel#read buf 0 (Bytes.length buf) in
       write_callback buf amt
       (* if amt <> bufsize then () else loop () *)
     done
   with
  | End_of_file | Libssh.LibsshError _ -> ());
  channel#send_eof ();
  channel#close ()
;;

let remote_command_io ssh command ~header ~read_callback ~write_callback =
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  channel#request_exec command;
  let header_bytes = Bytes.of_string header in
  let bufsize = 1024 in
  let buf = Bytes.make bufsize Char.min_value in
  (* Read header response (single read) *)
  (try
     let amt = channel#read buf 0 (Bytes.length buf) in
     write_callback buf amt
     (* if amt <> bufsize then () else loop () *)
   with
  | End_of_file | Libssh.LibsshError _ -> ());
  (* Send headers *)
  (try
     let index = ref 0 in
     let header_len = Bytes.length header_bytes in
     while !index < header_len do
       let max_amt = header_len - !index in
       let amt = Int.min (Bytes.length buf) max_amt in
       Bytes.blit ~src:header_bytes ~src_pos:!index ~dst:buf ~dst_pos:0 ~len:amt;
       channel#write buf 0 amt |> ignore;
       index := !index + amt
     done
   with
  | End_of_file | Libssh.LibsshError _ -> ());
  (* Stream writes *)
  (try
     let complete = ref false in
     while not !complete do
       let amt = read_callback buf bufsize in
       if Int.equal amt (-1)
       then complete := true
       else (
         let write_amt = channel#write buf 0 amt in
         if not (Int.equal amt write_amt) then complete := true)
     done
   with
  | End_of_file | Libssh.LibsshError _ -> ());
  channel#send_eof ();
  channel#close ()
;;

let shard_dir = sprintf "/tmp/shard/%s" version_string
let shard_exe = sprintf "%s/shard.exe" shard_dir
let rslog = sprintf "%s/rslog.txt" shard_dir
let rrlog = sprintf "%s/rrlog.txt" shard_dir

let remote_run_sender_unsafe ~host ~port ~verbose ~header ~port_callback ~read_callback =
  let stderr = force Async.Writer.stderr in
  let _local_path, program_hash = local_copy ~verbose ~host ~port in
  debug_println ~verbose program_hash;
  Util.verbose_println
    ~name:(source `Sender)
    ~verbose
    ~stderr
    ~host
    ~port
    "Connecting to remote...";
  let ssh = connect ~host ~port ~verbose in
  (* Check for remote copy of Shard *)
  Util.verbose_println
    ~name:(source `Sender)
    ~verbose
    ~stderr
    ~host
    ~port
    "Checking for remote copy of Shard...";
  let remote_hash = remote_command_fixed ssh (hash_command shard_exe) ~size:1024 in
  debug_println ~verbose remote_hash;
  let remote_exists = String.equal remote_hash program_hash in
  if not remote_exists
  then (
    (* If remote copy does not exist, make directory *)
    Util.verbose_println
      ~name:(source `Sender)
      ~verbose
      ~stderr
      ~host
      ~port
      "Remote copy does not exist.";
    Util.verbose_println
      ~name:(source `Sender)
      ~verbose
      ~stderr
      ~host
      ~port
      "Making remote directory...";
    remote_command ssh (sprintf "mkdir -p %s" shard_dir);
    (* Copy Shard to remote *)
    Util.verbose_println
      ~name:(source `Sender)
      ~verbose
      ~stderr
      ~host
      ~port
      "Copying executable to remote...";
    send_copy ssh ~dir:shard_dir ~src:shard_exe ~dest:shard_exe;
    (* Set permissions of Shard *)
    Util.verbose_println
      ~name:(source `Sender)
      ~verbose
      ~stderr
      ~host
      ~port
      "Setting permissions of remote executable...";
    remote_command ssh (sprintf "chmod 744 %s" shard_exe));
  (* Run command on remote Shard *)
  Util.verbose_println
    ~name:(source `Sender)
    ~verbose
    ~stderr
    ~host
    ~port
    "Starting receiver on remote Shard...";
  let command = sprintf "%s -Rrr 2>> %s" shard_exe rrlog in
  remote_command_io ssh command ~header ~read_callback ~write_callback:(fun b amt ->
      let sub = Bytes.sub ~pos:0 ~len:amt b in
      let port_string = Bytes.to_string sub in
      let port_string = String.chop_suffix_if_exists ~suffix:"\n" port_string in
      port_callback (Int.of_string port_string));
  Util.verbose_println ~name:(source `Sender) ~verbose ~stderr ~host ~port "Complete!"
;;

let remote_run_receiver_unsafe
    ~host
    ~port
    ~verbose
    ~remote_port
    ~write_callback
    ~close_callback
  =
  let stderr = force Async.Writer.stderr in
  Util.verbose_println
    ~name:(source `Receiver)
    ~verbose
    ~stderr
    ~host
    ~port
    "Conneting to remote...";
  let ssh = connect ~host ~port ~verbose in
  (* Run command on remote Shard *)
  Util.verbose_println
    ~name:(source `Receiver)
    ~verbose
    ~stderr
    ~host
    ~port
    "Starting sender on remote Shard...";
  let command = sprintf "%s -Rrs %d -s 2>> %s" shard_exe remote_port rslog in
  remote_command_output ssh command ~write_callback;
  close_callback ();
  Util.verbose_println ~name:(source `Receiver) ~verbose ~stderr ~host ~port "Complete!"
;;

let remote_run_sender ~host ~port ~verbose ~header ~port_callback ~read_callback =
  Or_error.try_with (fun () ->
      remote_run_sender_unsafe ~host ~port ~verbose ~header ~port_callback ~read_callback)
;;

let remote_run_receiver ~host ~port ~verbose ~remote_port ~write_callback ~close_callback =
  Or_error.try_with (fun () ->
      remote_run_receiver_unsafe
        ~host
        ~port
        ~verbose
        ~remote_port
        ~write_callback
        ~close_callback)
;;
