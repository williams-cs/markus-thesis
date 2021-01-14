(*
 * libssh OCaml bindings
 * Copyright (C) 2019 Pino Toscano <ptoscano@redhat.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

type ssh_t
type sftp_t
type sftp_file_t
type sftp_dir_t
type channel_t
type key_t
type scp_t

type ssh_error_type =
  | SSH_NO_ERROR
  | SSH_REQUEST_DENIED
  | SSH_FATAL
  | SSH_EINTR
  | SSH_ERROR_OTHER of int

type ssh_log_type =
  | SSH_LOG_NONE
  | SSH_LOG_WARN
  | SSH_LOG_INFO
  | SSH_LOG_DEBUG
  | SSH_LOG_TRACE

type ssh_option =
  | SSH_OPTIONS_HOST of string
  | SSH_OPTIONS_PORT of int
  | SSH_OPTIONS_USER of string
  | SSH_OPTIONS_SSH_DIR of string
  | SSH_OPTIONS_ADD_IDENTITY of string
  | SSH_OPTIONS_KNOWNHOSTS of string
  | SSH_OPTIONS_TIMEOUT of int
  | SSH_OPTIONS_TIMEOUT_USEC of int64
  | SSH_OPTIONS_LOG_VERBOSITY of ssh_log_type
  | SSH_OPTIONS_CIPHERS_C_S of string list
  | SSH_OPTIONS_CIPHERS_S_C of string list
  | SSH_OPTIONS_COMPRESSION_C_S of string
  | SSH_OPTIONS_COMPRESSION_S_C of string
  | SSH_OPTIONS_PROXYCOMMAND of string
  | SSH_OPTIONS_BINDADDR of string
  | SSH_OPTIONS_STRICTHOSTKEYCHECK of bool
  | SSH_OPTIONS_COMPRESSION of string
  | SSH_OPTIONS_COMPRESSION_LEVEL of int
  | SSH_OPTIONS_KEY_EXCHANGE of string list
  | SSH_OPTIONS_HOSTKEYS of string list
  | SSH_OPTIONS_GSSAPI_SERVER_IDENTITY of string
  | SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY of string
  | SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS of bool
  | SSH_OPTIONS_HMAC_C_S of string list
  | SSH_OPTIONS_HMAC_S_C of string list
  | SSH_OPTIONS_PASSWORD_AUTH of bool
  | SSH_OPTIONS_PUBKEY_AUTH of bool
  | SSH_OPTIONS_KBDINT_AUTH of bool
  | SSH_OPTIONS_GSSAPI_AUTH of bool
  | SSH_OPTIONS_GLOBAL_KNOWNHOSTS of string
  | SSH_OPTIONS_NODELAY of bool
  | SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES of string list

type ssh_auth_code =
  | SSH_AUTH_SUCCESS
  | SSH_AUTH_DENIED
  | SSH_AUTH_PARTIAL
  | SSH_AUTH_INFO
  | SSH_AUTH_AGAIN
  | SSH_AUTH_ERROR

type ssh_auth_method =
  | SSH_AUTH_METHOD_NONE
  | SSH_AUTH_METHOD_PASSWORD
  | SSH_AUTH_METHOD_PUBLICKEY
  | SSH_AUTH_METHOD_HOSTBASED
  | SSH_AUTH_METHOD_INTERACTIVE
  | SSH_AUTH_METHOD_GSSAPI_MIC

type sftp_filetype =
  | SSH_FILEXFER_TYPE_UNKNOWN
  | SSH_FILEXFER_TYPE_REGULAR
  | SSH_FILEXFER_TYPE_DIRECTORY
  | SSH_FILEXFER_TYPE_SYMLINK
  | SSH_FILEXFER_TYPE_SPECIAL

type sftp_statvfs_flag =
  | SSH_FXE_STATVFS_ST_RDONLY
  | SSH_FXE_STATVFS_ST_NOSUID

type sftp_open_flag =
  | O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_CREAT
  | O_EXCL
  | O_TRUNC

type ssh_keytype =
  | SSH_KEYTYPE_UNKNOWN
  | SSH_KEYTYPE_DSS
  | SSH_KEYTYPE_RSA
  | SSH_KEYTYPE_RSA1
  | SSH_KEYTYPE_ECDSA
  | SSH_KEYTYPE_ED25519
  | SSH_KEYTYPE_DSS_CERT01
  | SSH_KEYTYPE_RSA_CERT01
  | SSH_KEYTYPE_OTHER of string

type ssh_publickey_hash =
  | SSH_PUBLICKEY_HASH_SHA1
  | SSH_PUBLICKEY_HASH_MD5
  | SSH_PUBLICKEY_HASH_SHA256

type ssh_known_hosts =
  | SSH_KNOWN_HOSTS_NOT_FOUND
  | SSH_KNOWN_HOSTS_UNKNOWN
  | SSH_KNOWN_HOSTS_OK
  | SSH_KNOWN_HOSTS_CHANGED
  | SSH_KNOWN_HOSTS_OTHER

type ssh_scp_request_type =
  | SSH_SCP_REQUEST_NEWDIR
  | SSH_SCP_REQUEST_NEWFILE
  | SSH_SCP_REQUEST_ENDDIR
  | SSH_SCP_REQUEST_WARNING

type ssh_scp_request_mode =
  | SSH_SCP_WRITE
  | SSH_SCP_READ

type ssh_keycmp =
  | SSH_KEY_CMP_PUBLIC
  | SSH_KEY_CMP_PRIVATE

type sftp_attribute_flag =
  | SSH_FILEXFER_ATTR_SIZE
  | SSH_FILEXFER_ATTR_PERMISSIONS
  | SSH_FILEXFER_ATTR_ACCESSTIME
  | SSH_FILEXFER_ATTR_ACMODTIME
  | SSH_FILEXFER_ATTR_CREATETIME
  | SSH_FILEXFER_ATTR_MODIFYTIME
  | SSH_FILEXFER_ATTR_ACL
  | SSH_FILEXFER_ATTR_OWNERGROUP
  | SSH_FILEXFER_ATTR_SUBSECOND_TIMES
  | SSH_FILEXFER_ATTR_EXTENDED
  | SSH_FILEXFER_ATTR_UIDGID

type sftp_error_type =
  | SSH_FX_OK
  | SSH_FX_EOF
  | SSH_FX_NO_SUCH_FILE
  | SSH_FX_PERMISSION_DENIED
  | SSH_FX_FAILURE
  | SSH_FX_BAD_MESSAGE
  | SSH_FX_NO_CONNECTION
  | SSH_FX_CONNECTION_LOST
  | SSH_FX_OP_UNSUPPORTED
  | SSH_FX_INVALID_HANDLE
  | SSH_FX_NO_SUCH_PATH
  | SSH_FX_FILE_ALREADY_EXISTS
  | SSH_FX_WRITE_PROTECT
  | SSH_FX_NO_MEDIA
  | SSH_FX_OTHER of int

type sftp_attributes = {
  name : string option;
  longname : string option; (** ls -l output on openssh, not reliable else *)
  flags : sftp_attribute_flag list;
  typ : sftp_filetype;
  size : int64;
  uid : int;
  gid : int;
  owner : string option; (** set if openssh and version 4 *)
  group : string option; (** set if openssh and version 4 *)
  permissions : int;
  atime64 : int64;
  atime : int;
  atime_nseconds : int;
  createtime : int64;
  createtime_nseconds : int;
  mtime64 : int64;
  mtime : int;
  mtime_nseconds : int;
  acl : string option;
  extended_count : int;
  extended_type : string option;
  extended_data : string option;
}

type sftp_statvfs = {
  f_bsize : int64;   (** file system block size *)
  f_frsize : int64;   (** fundamental fs block size *)
  f_blocks : int64;   (** number of blocks (unit f_frsize) *)
  f_bfree : int64;   (** free blocks in file system *)
  f_bavail : int64;   (** free blocks for non-root *)
  f_files : int64;   (** total file inodes *)
  f_ffree : int64;   (** free file inodes *)
  f_favail : int64;   (** free file inodes for to non-root *)
  f_fsid : int64;   (** file system id *)
  f_flag : sftp_statvfs_flag list;    (** bit mask of f_flag values *)
  f_namemax : int64;   (** maximum filename length *)
}

type userauth_kbdint_prompt = {
  prompt : string;
  echo : bool;
}

exception LibsshError of ssh_error_type * string
exception LibsshSftpError of sftp_error_type * ssh_error_type * string

external libssh_version : unit -> int * int * int = "ocaml_libssh_libssh_version"
external libssh_version_string : unit -> string = "ocaml_libssh_libssh_version_string"

external ssh_set_log_level : ssh_log_type -> unit = "ocaml_libssh_ssh_set_log_level"
external ssh_get_log_level : unit -> ssh_log_type = "ocaml_libssh_ssh_get_log_level"
external ssh_copyright : unit -> string = "ocaml_libssh_ssh_copyright"

type ssh_auth =
  | Passphrase of string
  | Callback of (string -> bool -> bool -> string option)

external ssh_key_type : key_t -> ssh_keytype = "ocaml_libssh_ssh_key_type"
external ssh_pki_export_privkey_file : key_t -> ?auth:ssh_auth -> string -> unit = "ocaml_libssh_ssh_pki_export_privkey_file"
external ssh_pki_export_privkey_base64 : key_t -> ?auth:ssh_auth -> unit -> bytes = "ocaml_libssh_ssh_pki_export_privkey_base64"
external ssh_get_publickey_hash : key_t -> ssh_publickey_hash -> bytes = "ocaml_libssh_ssh_get_publickey_hash"
external ssh_key_is_public : key_t -> bool = "ocaml_libssh_ssh_key_is_public"
external ssh_key_is_private : key_t -> bool = "ocaml_libssh_ssh_key_is_private"
external ssh_pki_key_ecdsa_name : key_t -> string option = "ocaml_libssh_ssh_pki_key_ecdsa_name"
external ssh_pki_export_privkey_to_pubkey : key_t -> key_t = "ocaml_libssh_ssh_pki_export_privkey_to_pubkey"
external ssh_pki_export_pubkey_base64 : key_t -> bytes = "ocaml_libssh_ssh_pki_export_pubkey_base64"

class ssh_key h =
  object (self)
    method typ () = ssh_key_type h
    method export_privkey_file = ssh_pki_export_privkey_file h
    method export_privkey_base64 = ssh_pki_export_privkey_base64 h
    method get_publickey_hash = ssh_get_publickey_hash h
    method is_public () = ssh_key_is_public h
    method is_private () = ssh_key_is_private h
    method ecdsa_name () = ssh_pki_key_ecdsa_name h
    method export_privkey_to_pubkey () =
      let k = ssh_pki_export_privkey_to_pubkey h in
      new ssh_key k
    method export_pubkey_base64 () = ssh_pki_export_pubkey_base64 h

    method handle () = h
end

external ssh_pki_generate : ssh_keytype -> int -> key_t = "ocaml_libssh_ssh_pki_generate"
let ssh_pki_generate typ param =
  let k = ssh_pki_generate typ param in
  new ssh_key k
external ssh_get_fingerprint_hash : ssh_publickey_hash -> bytes -> string = "ocaml_libssh_ssh_get_fingerprint_hash"
external ssh_pki_import_pubkey_base64 : bytes -> ssh_keytype -> key_t = "ocaml_libssh_ssh_pki_import_pubkey_base64"
let ssh_pki_import_pubkey_base64 data typ =
  let k = ssh_pki_import_pubkey_base64 data typ in
  new ssh_key k
external ssh_pki_import_pubkey_file : string -> key_t = "ocaml_libssh_ssh_pki_import_pubkey_file"
let ssh_pki_import_pubkey_file file =
  let k = ssh_pki_import_pubkey_file file in
  new ssh_key k
external ssh_pki_import_cert_base64 : bytes -> ssh_keytype -> key_t = "ocaml_libssh_ssh_pki_import_cert_base64"
let ssh_pki_import_cert_base64 data typ =
  let k = ssh_pki_import_cert_base64 data typ in
  new ssh_key k
external ssh_pki_import_cert_file : string -> key_t = "ocaml_libssh_ssh_pki_import_cert_file"
let ssh_pki_import_cert_file file =
  let k = ssh_pki_import_cert_file file in
  new ssh_key k
external ssh_key_cmp : ssh_keycmp -> key_t -> key_t -> int = "ocaml_libssh_ssh_key_cmp"
let ssh_key_cmp cmp a b =
  ssh_key_cmp cmp (a#handle ()) (b#handle ())
external ssh_pki_import_privkey_file : ?auth:ssh_auth -> string -> key_t = "ocaml_libssh_ssh_pki_import_privkey_file"
let ssh_pki_import_privkey_file ?auth file =
  let k = ssh_pki_import_privkey_file ?auth file in
  new ssh_key k
external ssh_pki_import_privkey_base64 : ?auth:ssh_auth -> bytes -> key_t = "ocaml_libssh_ssh_pki_import_privkey_base64"
let ssh_pki_import_privkey_base64 ?auth data =
  let k = ssh_pki_import_privkey_base64 ?auth data in
  new ssh_key k

type ssh_knownhosts_entry = {
  hostname : string;
  unparsed : string;
  publickey : ssh_key;
  comment : string option;
}
and ssh_knownhosts_entry_internal = {
  i_hostname : string;
  i_unparsed : string;
  i_publickey : key_t;
  i_comment : string option;
}

let ssh_knownhosts_entry_of_internal { i_hostname; i_unparsed; i_publickey; i_comment } =
  {
    hostname = i_hostname;
    unparsed = i_unparsed;
    publickey = new ssh_key i_publickey;
    comment = i_comment
  }

external ssh_known_hosts_parse_line : string -> string -> ssh_knownhosts_entry_internal = "ocaml_libssh_ssh_known_hosts_parse_line"
let ssh_known_hosts_parse_line hostname line =
  ssh_knownhosts_entry_of_internal (ssh_known_hosts_parse_line hostname line)

external ssh_new : unit -> ssh_t = "ocaml_libssh_libssh_new"
external ssh_get_error : ssh_t -> ssh_error_type * string = "ocaml_libssh_ssh_get_error"
external ssh_options_parse_config : ssh_t -> string option -> unit = "ocaml_libssh_ssh_options_parse_config"
external ssh_options_set : ssh_t -> ssh_option -> unit = "ocaml_libssh_ssh_options_set"
external ssh_set_blocking : ssh_t -> bool -> unit = "ocaml_libssh_ssh_set_blocking"
external ssh_is_blocking : ssh_t -> bool = "ocaml_libssh_ssh_is_blocking"
external ssh_connect : ssh_t -> unit = "ocaml_libssh_ssh_connect"
external ssh_userauth_none : ssh_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_none"
external ssh_userauth_list : ssh_t -> ssh_auth_method list = "ocaml_libssh_ssh_userauth_list"
external ssh_userauth_publickey_auto : ssh_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_publickey_auto"
external ssh_userauth_gssapi : ssh_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_gssapi"
external ssh_userauth_kbdint : ssh_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_kbdint"
external ssh_userauth_kbdint_getname : ssh_t -> string option = "ocaml_libssh_ssh_userauth_kbdint_getname"
external ssh_userauth_kbdint_getinstruction : ssh_t -> string option = "ocaml_libssh_ssh_userauth_kbdint_getinstruction"
external ssh_userauth_kbdint_getprompts : ssh_t -> userauth_kbdint_prompt list = "ocaml_libssh_ssh_userauth_kbdint_getprompts"
external ssh_userauth_kbdint_setanswers : ssh_t -> string list -> unit = "ocaml_libssh_ssh_userauth_kbdint_setanswers"
external ssh_userauth_password : ssh_t -> string -> ssh_auth_code = "ocaml_libssh_ssh_userauth_password"
external ssh_userauth_try_publickey : ssh_t -> key_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_try_publickey"
external ssh_userauth_publickey : ssh_t -> key_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_publickey"
external ssh_userauth_agent : ssh_t -> ssh_auth_code = "ocaml_libssh_ssh_userauth_agent"
external ssh_get_issue_banner : ssh_t -> string option = "ocaml_libssh_ssh_get_issue_banner"
external ssh_disconnect : ?silent:bool -> ssh_t -> unit = "ocaml_libssh_ssh_disconnect"
external ssh_get_clientbanner : ssh_t -> string option = "ocaml_libssh_ssh_get_clientbanner"
external ssh_get_serverbanner : ssh_t -> string option = "ocaml_libssh_ssh_get_serverbanner"
external ssh_is_connected : ssh_t -> bool = "ocaml_libssh_ssh_is_connected"
external ssh_get_version : ssh_t -> int = "ocaml_libssh_ssh_get_version"
external ssh_blocking_flush : ssh_t -> int -> unit = "ocaml_libssh_ssh_blocking_flush"
external ssh_get_server_publickey : ssh_t -> key_t = "ocaml_libssh_ssh_get_server_publickey"
external ssh_session_is_known_server : ssh_t -> ssh_known_hosts = "ocaml_libssh_ssh_session_is_known_server"
external ssh_session_update_known_hosts : ssh_t -> unit = "ocaml_libssh_ssh_session_update_known_hosts"
external ssh_get_openssh_version : ssh_t -> (int * int * int) option = "ocaml_libssh_ssh_get_openssh_version"
external ssh_get_cipher_in : ssh_t -> string option = "ocaml_libssh_ssh_get_cipher_in"
external ssh_get_cipher_out : ssh_t -> string option = "ocaml_libssh_ssh_get_cipher_out"
external ssh_get_hmac_in : ssh_t -> string option = "ocaml_libssh_ssh_get_hmac_in"
external ssh_get_hmac_out : ssh_t -> string option = "ocaml_libssh_ssh_get_hmac_out"
external ssh_session_export_known_hosts_entry : ssh_t -> string = "ocaml_libssh_ssh_session_export_known_hosts_entry"
external ssh_get_kex_algo : ssh_t -> string option = "ocaml_libssh_ssh_get_kex_algo"
external ssh_send_ignore : ssh_t -> string -> unit = "ocaml_libssh_ssh_send_ignore"
external ssh_send_debug : ssh_t -> string -> bool -> unit = "ocaml_libssh_ssh_send_debug"

class ssh () =
  let h = ssh_new () in
  object (self)
    method get_error () = ssh_get_error h
    method options_parse_config = ssh_options_parse_config h
    method options_set = ssh_options_set h
    method set_blocking = ssh_set_blocking h
    method is_blocking () = ssh_is_blocking h
    method connect () = ssh_connect h
    method userauth_none () = ssh_userauth_none h
    method userauth_list () = ssh_userauth_list h
    method userauth_publickey_auto () = ssh_userauth_publickey_auto h
    method userauth_gssapi () = ssh_userauth_gssapi h
    method userauth_kbdint () = ssh_userauth_kbdint h
    method userauth_kbdint_getname () = ssh_userauth_kbdint_getname h
    method userauth_kbdint_getinstruction () = ssh_userauth_kbdint_getinstruction h
    method userauth_kbdint_getprompts () = ssh_userauth_kbdint_getprompts h
    method userauth_kbdint_setanswers = ssh_userauth_kbdint_setanswers h
    method userauth_password = ssh_userauth_password h
    method userauth_try_publickey (key : ssh_key) =
      ssh_userauth_try_publickey h (key#handle ())
    method userauth_publickey (key : ssh_key) =
      ssh_userauth_publickey h (key#handle ())
    method userauth_agent () = ssh_userauth_agent h
    method get_issue_banner () = ssh_get_issue_banner h
    method disconnect ?silent () = ssh_disconnect ?silent h
    method get_clientbanner () = ssh_get_clientbanner h
    method get_serverbanner () = ssh_get_serverbanner h
    method is_connected () = ssh_is_connected h
    method get_version () = ssh_get_version h
    method blocking_flush = ssh_blocking_flush h
    method get_server_publickey () =
      let k = ssh_get_server_publickey h in
      new ssh_key k
    method is_known_server () = ssh_session_is_known_server h
    method update_known_hosts () = ssh_session_update_known_hosts h
    method get_openssh_version () = ssh_get_openssh_version h
    method get_cipher_in () = ssh_get_cipher_in h
    method get_cipher_out () = ssh_get_cipher_out h
    method get_hmac_in () = ssh_get_hmac_in h
    method get_hmac_out () = ssh_get_hmac_out h
    method export_known_hosts_entry () = ssh_session_export_known_hosts_entry h
    method get_kex_algo () = ssh_get_kex_algo h
    method send_ignore = ssh_send_ignore h
    method send_debug = ssh_send_debug h

    method handle () = h
end

external ssh_channel_new : ssh_t -> channel_t = "ocaml_libssh_ssh_channel_new"
external ssh_channel_open_session : channel_t -> unit = "ocaml_libssh_ssh_channel_open_session"
external ssh_channel_open_auth_agent : channel_t -> unit = "ocaml_libssh_ssh_channel_open_auth_agent"
external ssh_channel_open_forward : channel_t -> string -> int -> string -> int -> unit = "ocaml_libssh_ssh_channel_open_forward"
external ssh_channel_send_eof : channel_t -> unit = "ocaml_libssh_ssh_channel_send_eof"
external ssh_channel_close : channel_t -> unit = "ocaml_libssh_ssh_channel_close"
external ssh_channel_is_eof : channel_t -> bool = "ocaml_libssh_ssh_channel_is_eof"
external ssh_channel_is_open : channel_t -> bool = "ocaml_libssh_ssh_channel_is_open"
external ssh_channel_is_closed : channel_t -> bool = "ocaml_libssh_ssh_channel_is_closed"
external ssh_channel_request_exec : channel_t -> string -> unit = "ocaml_libssh_ssh_channel_request_exec"
external ssh_channel_request_shell : channel_t -> unit = "ocaml_libssh_ssh_channel_request_shell"
external ssh_channel_request_subsystem : channel_t -> string -> unit = "ocaml_libssh_ssh_channel_request_subsystem"
external ssh_channel_request_auth_agent : channel_t -> unit = "ocaml_libssh_ssh_channel_request_auth_agent"
external ssh_channel_request_env : channel_t -> string -> string -> unit = "ocaml_libssh_ssh_channel_request_env"
external ssh_channel_request_send_signal : channel_t -> string -> unit = "ocaml_libssh_ssh_channel_send_signal"
external ssh_channel_request_send_break : channel_t -> int -> unit = "ocaml_libssh_ssh_channel_request_send_break"
external ssh_channel_request_sftp : channel_t -> unit = "ocaml_libssh_ssh_channel_request_sftp"
external ssh_channel_read : channel_t -> ?is_stderr:bool -> ?timeout:int -> bytes -> int -> int -> int = "ocaml_libssh_ssh_channel_read_bytecode" "ocaml_libssh_ssh_channel_read_native"
external ssh_channel_write : channel_t -> ?is_stderr:bool -> bytes -> int -> int -> int = "ocaml_libssh_ssh_channel_write"
external ssh_channel_poll : channel_t -> ?is_stderr:bool -> ?timeout:int -> unit -> int = "ocaml_libssh_ssh_channel_poll"

class channel ?chan ssh =
  let h =
    match chan with
    | None -> ssh_channel_new (ssh#handle ())
    | Some c -> c in
  object (self)
    method open_session () = ssh_channel_open_session h
    method open_auth_agent () = ssh_channel_open_auth_agent h
    method open_forward = ssh_channel_open_forward h
    method send_eof () = ssh_channel_send_eof h
    method close () = ssh_channel_close h
    method is_eof () = ssh_channel_is_eof h
    method is_open () = ssh_channel_is_open h
    method is_closed () = ssh_channel_is_closed h
    method request_exec = ssh_channel_request_exec h
    method request_shell () = ssh_channel_request_shell h
    method request_subsystem  = ssh_channel_request_subsystem h
    method request_auth_agent () = ssh_channel_request_auth_agent h
    method read = ssh_channel_read h
    method write = ssh_channel_write h
    method request_env = ssh_channel_request_env h
    method request_send_signal = ssh_channel_request_send_signal h
    method request_send_break = ssh_channel_request_send_break h
    method request_sftp () = ssh_channel_request_sftp h
    method poll = ssh_channel_poll h

    method handle () = h
end

external ssh_channel_listen_forward : ?address:string -> ?port:int -> ssh_t -> int = "ocaml_libssh_ssh_channel_listen_forward"
let ssh_channel_listen_forward ?address ?port ssh =
  ssh_channel_listen_forward ?address ?port (ssh#handle ())
external ssh_channel_accept_forward : ssh_t -> int -> (channel_t * int) option = "ocaml_libssh_ssh_channel_accept_forward"
let ssh_channel_accept_forward ssh timeout =
  let ret = ssh_channel_accept_forward (ssh#handle ()) timeout in
  match ret with
  | None -> None
  | Some (chan, port) -> Some (new channel ~chan ssh, port)
external ssh_channel_cancel_forward : ssh_t -> string -> int -> unit = "ocaml_libssh_ssh_channel_cancel_forward"
let ssh_channel_cancel_forward ssh =
  ssh_channel_cancel_forward (ssh#handle ())

external sftp_fstat : sftp_file_t -> sftp_attributes = "ocaml_libssh_sftp_fstat"
external sftp_file_set_blocking : sftp_file_t -> bool -> unit = "ocaml_libssh_sftp_file_set_blocking"
external sftp_fstatvfs : sftp_file_t -> sftp_statvfs = "ocaml_libssh_sftp_fstatvfs"
external sftp_fsync : sftp_file_t -> unit = "ocaml_libssh_sftp_fsync"
external sftp_seek : sftp_file_t -> int64 -> unit = "ocaml_libssh_sftp_seek"
external sftp_rewind : sftp_file_t -> unit = "ocaml_libssh_sftp_rewind"
external sftp_tell : sftp_file_t -> int64 = "ocaml_libssh_sftp_tell"
external sftp_read : sftp_file_t -> bytes -> int -> int -> int = "ocaml_libssh_sftp_read"
external sftp_write : sftp_file_t -> bytes -> int -> int -> int = "ocaml_libssh_sftp_write"

class sftp_file h =
  object (self)
    method set_blocking = sftp_file_set_blocking h
    method fstat () = sftp_fstat h
    method fstatvfs () = sftp_fstatvfs h
    method fsync () = sftp_fsync h
    method seek = sftp_seek h
    method rewind () = sftp_rewind h
    method tell () = sftp_tell h
    method read = sftp_read h
    method write = sftp_write h
end

external sftp_readdir : sftp_dir_t -> sftp_attributes = "ocaml_libssh_sftp_readdir"
external sftp_dir_eof : sftp_dir_t -> bool = "ocaml_libssh_sftp_dir_eof"

class sftp_dir h =
  object (self)
    method readdir () = sftp_readdir h
    method is_eof () = sftp_dir_eof h
end

external sftp_new : ssh_t -> sftp_t = "ocaml_libssh_sftp_new"
external sftp_new_channel : ssh_t -> channel_t -> sftp_t = "ocaml_libssh_sftp_new_channel"
external sftp_server_version : sftp_t -> int = "ocaml_libssh_sftp_server_version"
external sftp_stat : sftp_t -> string -> sftp_attributes = "ocaml_libssh_sftp_stat"
external sftp_lstat : sftp_t -> string -> sftp_attributes = "ocaml_libssh_sftp_lstat"
external sftp_statvfs : sftp_t -> string -> sftp_statvfs = "ocaml_libssh_sftp_statvfs"
external sftp_extension_supported : sftp_t -> string -> string -> bool = "ocaml_libssh_sftp_extension_supported"
external sftp_extensions : sftp_t -> (string * string) array = "ocaml_libssh_sftp_extensions"
external sftp_canonicalize_path : sftp_t -> string -> string = "ocaml_libssh_sftp_canonicalize_path"
external sftp_open : sftp_t -> string -> sftp_open_flag list -> int -> sftp_file_t = "ocaml_libssh_sftp_open"
external sftp_unlink : sftp_t -> string -> unit = "ocaml_libssh_sftp_unlink"
external sftp_rmdir : sftp_t -> string -> unit = "ocaml_libssh_sftp_rmdir"
external sftp_mkdir : sftp_t -> string -> int -> unit = "ocaml_libssh_sftp_mkdir"
external sftp_rename : sftp_t -> string -> string -> unit = "ocaml_libssh_sftp_rename"
external sftp_chown : sftp_t -> string -> int -> int -> unit = "ocaml_libssh_sftp_chown"
external sftp_chmod : sftp_t -> string -> int -> unit = "ocaml_libssh_sftp_chmod"
external sftp_symlink : sftp_t -> string -> string -> unit = "ocaml_libssh_sftp_symlink"
external sftp_readlink : sftp_t -> string -> string = "ocaml_libssh_sftp_readlink"
external sftp_opendir : sftp_t -> string -> sftp_dir_t = "ocaml_libssh_sftp_opendir"

class sftp ?chan ssh =
  let h =
    match chan with
    | None -> sftp_new (ssh#handle ())
    | Some chan -> sftp_new_channel (ssh#handle ()) (chan#handle ()) in
  object (self)
    method server_version () = sftp_server_version h
    method stat = sftp_stat h
    method lstat = sftp_lstat h
    method statvfs = sftp_statvfs h
    method extension_supported = sftp_extension_supported h
    method extensions () = sftp_extensions h
    method canonicalize_path = sftp_canonicalize_path h
    method open_file path flags mode =
      let f = sftp_open h path flags mode in
      new sftp_file f
    method unlink = sftp_unlink h
    method rmdir = sftp_rmdir h
    method mkdir = sftp_mkdir h
    method rename = sftp_rename h
    method chown = sftp_chown h
    method chmod = sftp_chmod h
    method symlink = sftp_symlink h
    method readlink = sftp_readlink h
    method opendir path =
      let d = sftp_opendir h path in
      new sftp_dir d
end

external ssh_scp_new : ?recursive:bool -> ssh_t -> ssh_scp_request_mode -> string -> scp_t = "ocaml_libssh_ssh_scp_new"
external ssh_scp_pull_request : scp_t -> ssh_scp_request_type = "ocaml_libssh_ssh_scp_pull_request"
external ssh_scp_request_get_size : scp_t -> int64 = "ocaml_libssh_ssh_scp_request_get_size"
external ssh_scp_request_get_filename : scp_t -> string = "ocaml_libssh_ssh_scp_request_get_filename"
external ssh_scp_request_get_permissions : scp_t -> int = "ocaml_libssh_ssh_scp_request_get_permissions"
external ssh_scp_accept_request : scp_t -> unit = "ocaml_libssh_ssh_scp_accept_request"
external ssh_scp_deny_request : scp_t -> string -> unit = "ocaml_libssh_ssh_scp_deny_request"
external ssh_scp_read : scp_t -> bytes -> int -> int -> int = "ocaml_libssh_ssh_scp_read"
external ssh_scp_request_get_warning : scp_t -> string = "ocaml_libssh_ssh_scp_request_get_warning"
external ssh_scp_push_directory : scp_t -> string -> int -> unit = "ocaml_libssh_ssh_scp_push_directory"
external ssh_scp_leave_directory : scp_t -> unit = "ocaml_libssh_ssh_scp_leave_directory"
external ssh_scp_push_file : scp_t -> string -> int64 -> int -> unit = "ocaml_libssh_ssh_scp_push_file"
external ssh_scp_write : scp_t -> bytes -> int -> int -> unit = "ocaml_libssh_ssh_scp_write"
external ssh_scp_close : scp_t -> unit = "ocaml_libssh_ssh_scp_close"

class scp ?recursive ssh mode location =
  let h = ssh_scp_new ?recursive (ssh#handle ()) mode location in
  object (self)
    method pull_request () = ssh_scp_pull_request h
    method request_get_size () = ssh_scp_request_get_size h
    method request_get_filename () = ssh_scp_request_get_filename h
    method request_get_permissions () = ssh_scp_request_get_permissions h
    method accept_request () = ssh_scp_accept_request h
    method deny_request = ssh_scp_deny_request h
    method read = ssh_scp_read h
    method request_get_warning () = ssh_scp_request_get_warning h
    method push_directory = ssh_scp_push_directory h
    method leave_directory () = ssh_scp_leave_directory h
    method push_file = ssh_scp_push_file h
    method write = ssh_scp_write h
    method close () = ssh_scp_close h
end

let () =
  Callback.register_exception "Libssh.LibsshError" (LibsshError (SSH_NO_ERROR, ""));
  Callback.register_exception "Libssh.LibsshSftpError" (LibsshSftpError (SSH_FX_OK, SSH_NO_ERROR, ""))
