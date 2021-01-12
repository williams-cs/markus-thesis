open Core
open Base.Poly

let ssh_println str = str |> ignore

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
  (* ssh#options_set (Libssh.SSH_OPTIONS_LOG_VERBOSITY Libssh.SSH_LOG_DEBUG); *)
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

let version_string = "v0001"

let send_copy ssh dir =
  let file = Unix.openfile ~mode:[ Unix.O_RDONLY ] "./shard.exe" in
  let stat = Unix.fstat file in
  let fsize = stat.st_size in
  let perms = 0700 in
  let dest = sprintf "%s/shard.exe" dir in
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
  scp#close ();
  dest
;;

(* workaround for bug for libssh bindings *)
let dummy channels = List.iter channels ~f:(fun channel -> channel#send_eof ())

let remote_run ~host ~program ~write_callback ~close_callback =
  (* let args = Sys.get_argv () in
  let host =
    match Array.length args with
    | 0 | 1 -> "localhost"
    | _ -> args.(1)
  in *)
  let ssh = connect host in
  let channels = [] in
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  let dir = sprintf "/tmp/shard/%s" version_string in
  let command = sprintf "mkdir -p %s" dir in
  channel#request_exec command;
  channel#send_eof ();
  let dest = send_copy ssh dir in
  let channels = channel :: channels in
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  let command = sprintf "chmod 744 %s" dest in
  channel#request_exec command;
  channel#send_eof ();
  let channels = channel :: channels in
  let channel = new Libssh.channel ssh in
  channel#open_session ();
  let command = sprintf "echo '%s' | %s" program dest in
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
  close_callback ();
  channel#send_eof ();
  dummy channels
;;
