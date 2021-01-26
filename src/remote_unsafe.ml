open Core
open Base.Poly

(* Configuration *)
(* Default timeout: 3 seconds *)
let default_ssh_timeout = 3000000L
let debug = true
let verbose = true
let ssh_debug = false
let ssh_println str = str |> ignore
let version_string = "v0001"

module Session = struct
  type t =
    | Not_connected of bool ref
    | Connected of Libssh.ssh
    | Complete

  let create status =
    match status with
    | `Not_connected -> Not_connected (ref true)
    | `Connected ssh -> Connected ssh
    | `Complete -> Complete
  ;;

  let disconnect t =
    match t with
    | Not_connected to_connect -> to_connect := false
    | Connected ssh -> ssh#disconnect ()
    | Complete -> ()
  ;;

  let should_connect t =
    match t with
    | Not_connected to_connect -> !to_connect
    | Connected _ -> false
    | Complete -> false
  ;;
end

let active_sessions : (string, Session.t ref) Hashtbl.t = Hashtbl.create (module String)

let disconnect_active_sessions () =
  Hashtbl.iter active_sessions ~f:(fun session_ref ->
      let session = !session_ref in
      Session.disconnect session)
;;

(* print_endline str *)

let auth_publickey (ssh : Libssh.ssh) = ssh#userauth_publickey_auto ()

(* TODO: fix verify known hosts *)
let _verify_known_hosts (ssh : Libssh.ssh) =
  let _spub = ssh#get_server_publickey () in
  let state = ssh#is_known_server () in
  match state with
  | Libssh.SSH_KNOWN_HOSTS_OK -> ()
  | Libssh.SSH_KNOWN_HOSTS_CHANGED ->
    ssh_println "Host key changed, connection will be aborted.";
    exit 1
  | Libssh.SSH_KNOWN_HOSTS_OTHER ->
    ssh_println "Host key not found, connection will be aborted.";
    exit 1
  | Libssh.SSH_KNOWN_HOSTS_NOT_FOUND | Libssh.SSH_KNOWN_HOSTS_UNKNOWN ->
    ssh_println
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

let connect host =
  let ssh = new Libssh.ssh () in
  ssh#options_set (Libssh.SSH_OPTIONS_HOST host);
  ssh#options_set (Libssh.SSH_OPTIONS_TIMEOUT_USEC default_ssh_timeout);
  (* SSH debug mode *)
  if ssh_debug
  then ssh#options_set (Libssh.SSH_OPTIONS_LOG_VERBOSITY Libssh.SSH_LOG_DEBUG);
  ssh#options_parse_config None;
  (* verify_known_hosts ssh; *)
  ssh#connect ();
  match authenticate ssh with
  | Libssh.SSH_AUTH_SUCCESS ->
    ssh_println "Success!";
    ssh
  | Libssh.SSH_AUTH_DENIED ->
    ssh_println "Access denied!";
    exit 1
  | _ ->
    ssh_println "Auth Error!";
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

let debug_println str = if debug then print_endline ("[Shard-debug] " ^ str)

let verbose_println host str =
  if verbose then print_endline (sprintf "[Shard-%s] " host ^ str)
;;

let local_command str =
  let env = Array.create ~len:0 "" in
  let channels = Unix.open_process_full str ~env in
  let { Unix.Process_channels.stdout; _ } = channels in
  Stdio.In_channel.input_all stdout
;;

let hash_command str = sprintf "md5sum %s| cut -d ' ' -f 1" str

let local_copy ~host =
  verbose_println host "Looking for local copy of Shard...";
  let dir = sprintf "/tmp/shard/%s" version_string in
  let dest = sprintf "%s/shard.exe" dir in
  let dest_hash = local_command (hash_command dest) in
  let src = Unix.readlink "/proc/self/exe" in
  let src_hash = local_command (hash_command src) in
  if not (String.equal dest_hash src_hash)
  then (
    verbose_println host "Local copy not found.";
    verbose_println host "Installing local copy of Shard...";
    Unix.mkdir_p dir;
    Gc.compact ();
    local_command (sprintf "cp %s %s" src dest) |> ignore;
    Unix.chmod dest ~perm:0o744;
    verbose_println host "Local installation complete!");
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

let remote_run_unsafe ~host ~program ~write_callback ~close_callback ~session_ref =
  let _local_path, program_hash = local_copy ~host in
  let dir = sprintf "/tmp/shard/%s" version_string in
  let exe = sprintf "%s/shard.exe" dir in
  debug_println program_hash;
  verbose_println host "Conneting to remote...";
  if Session.should_connect !session_ref
  then (
    let ssh = connect host in
    session_ref := Session.create (`Connected ssh);
    (* Check for remote copy of Shard *)
    verbose_println host "Checking for remote copy of Shard...";
    let remote_hash = remote_command_fixed ssh (hash_command exe) ~size:1024 in
    debug_println remote_hash;
    let remote_exists = String.equal remote_hash program_hash in
    let src = "./shard.exe" in
    let dest = sprintf "%s/shard.exe" dir in
    if not remote_exists
    then (
      (* If remote copy does not exist, make directory *)
      verbose_println host "Remote copy does not exist.";
      verbose_println host "Making remote directory...";
      remote_command ssh (sprintf "mkdir -p %s" dir);
      (* Copy Shard to remote *)
      verbose_println host "Copying executable to remote...";
      send_copy ssh ~dir ~src ~dest;
      (* Set permissions of Shard *)
      verbose_println host "Setting permissions of remote executable...";
      remote_command ssh (sprintf "chmod 744 %s" dest));
    (* Run command on remote Shard *)
    verbose_println host "Running command on remote Shard...";
    let command =
      match program with
      | `Sexp sexp -> sprintf "echo '%s' | %s -s" (Sexp.to_string sexp) dest
      | `Name name -> sprintf "echo '%s' | %s" name dest
    in
    remote_command_output ssh command ~write_callback;
    close_callback ();
    session_ref := Session.create `Complete;
    verbose_println host "Complete!")
  else verbose_println host "Connection cancelled!"
;;

let random_state_ref : Random.State.t option ref = ref None
let set_random_state state = random_state_ref := Some state

let random_state () =
  match !random_state_ref with
  | Some state -> state
  | None ->
    let state = Random.State.make_self_init () in
    random_state_ref := Some state;
    state
;;

let random_session_key () = Uuid.create_random (random_state ()) |> Uuid.to_string

let remote_run ~host ~program ~write_callback ~close_callback =
  let key = random_session_key () in
  (* No duplicates should exist from UUID keys *)
  let session_ref = ref (Session.create `Not_connected) in
  Hashtbl.add_exn active_sessions ~key ~data:session_ref;
  try remote_run_unsafe ~host ~program ~write_callback ~close_callback ~session_ref with
  | exn -> verbose_println host (Exn.to_string exn)
;;
