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
type sftp_file_t
type sftp_dir_t
type channel_t
type key_t

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

val libssh_version : unit -> int * int * int
val libssh_version_string : unit -> string

val ssh_set_log_level : ssh_log_type -> unit
val ssh_get_log_level : unit -> ssh_log_type
val ssh_copyright : unit -> string

type ssh_auth =
  | Passphrase of string
  | Callback of (string -> bool -> bool -> string option)

class ssh_key : key_t -> object
  method typ : unit -> ssh_keytype
  method export_privkey_file : ?auth:ssh_auth -> string -> unit
  method export_privkey_base64 : ?auth:ssh_auth -> unit -> bytes
  method get_publickey_hash : ssh_publickey_hash -> bytes
  method is_public : unit -> bool
  method is_private : unit -> bool
  method ecdsa_name : unit -> string option
  method export_privkey_to_pubkey : unit -> ssh_key
  method export_pubkey_base64 : unit -> bytes

  method handle : unit -> key_t
end

val ssh_pki_generate : ssh_keytype -> int -> ssh_key
val ssh_get_fingerprint_hash : ssh_publickey_hash -> bytes -> string
val ssh_pki_import_pubkey_base64 : bytes -> ssh_keytype -> ssh_key
val ssh_pki_import_pubkey_file : string -> ssh_key
val ssh_pki_import_cert_base64 : bytes -> ssh_keytype -> ssh_key
val ssh_pki_import_cert_file : string -> ssh_key
val ssh_key_cmp : ssh_keycmp -> ssh_key -> ssh_key -> int
val ssh_pki_import_privkey_file : ?auth:ssh_auth -> string -> ssh_key
val ssh_pki_import_privkey_base64 : ?auth:ssh_auth -> bytes -> ssh_key

type ssh_knownhosts_entry = {
  hostname : string;
  unparsed : string;
  publickey : ssh_key;
  comment : string option;
}

val ssh_known_hosts_parse_line : string -> string -> ssh_knownhosts_entry

class ssh : unit -> object
  method get_error : unit -> ssh_error_type * string
  method options_parse_config : string option -> unit
  method options_set : ssh_option -> unit
  method set_blocking : bool -> unit
  method is_blocking : unit -> bool
  method connect : unit -> unit
  method userauth_none : unit -> ssh_auth_code
  method userauth_list : unit -> ssh_auth_method list
  method userauth_publickey_auto : unit -> ssh_auth_code
  method userauth_gssapi : unit -> ssh_auth_code
  method userauth_kbdint : unit -> ssh_auth_code
  method userauth_kbdint_getname : unit -> string option
  method userauth_kbdint_getinstruction : unit -> string option
  method userauth_kbdint_getprompts : unit -> userauth_kbdint_prompt list
  method userauth_kbdint_setanswers : string list -> unit
  method userauth_password : string -> ssh_auth_code
  method userauth_try_publickey : ssh_key -> ssh_auth_code
  method userauth_publickey : ssh_key -> ssh_auth_code
  method userauth_agent : unit -> ssh_auth_code
  method get_issue_banner : unit -> string option
  method disconnect : ?silent:bool -> unit -> unit
  method get_clientbanner : unit -> string option
  method get_serverbanner : unit -> string option
  method is_connected : unit -> bool
  method get_version : unit -> int
  method blocking_flush : int -> unit
  method get_server_publickey : unit -> ssh_key
  method is_known_server : unit -> ssh_known_hosts
  method update_known_hosts : unit -> unit
  method get_openssh_version : unit -> (int * int * int) option
  method get_cipher_in : unit -> string option
  method get_cipher_out : unit -> string option
  method get_hmac_in : unit -> string option
  method get_hmac_out : unit -> string option
  method export_known_hosts_entry : unit -> string
  method get_kex_algo : unit -> string option
  method send_ignore : string -> unit
  method send_debug : string -> bool -> unit

  method handle : unit -> ssh_t
end

class channel : ?chan:channel_t -> ssh -> object
  method open_session : unit -> unit
  method open_auth_agent : unit -> unit
  method open_forward : string -> int -> string -> int -> unit
  method send_eof : unit -> unit
  method close : unit -> unit
  method is_eof : unit -> bool
  method is_open : unit -> bool
  method is_closed : unit -> bool
  method request_exec : string -> unit
  method request_shell : unit -> unit
  method request_subsystem : string -> unit
  method request_auth_agent : unit -> unit
  method request_env : string -> string -> unit
  method request_send_signal : string -> unit
  method request_send_break : int -> unit
  method request_sftp : unit -> unit
  method read : ?is_stderr:bool -> ?timeout:int -> bytes -> int -> int -> int
  method write : ?is_stderr:bool -> bytes -> int -> int -> int
  method poll : ?is_stderr:bool -> ?timeout:int -> unit -> int

  method handle : unit -> channel_t
end

val ssh_channel_listen_forward : ?address:string -> ?port:int -> ssh -> int
val ssh_channel_accept_forward : ssh -> int -> (channel * int) option
val ssh_channel_cancel_forward : ssh -> string -> int -> unit

class sftp_file : sftp_file_t -> object
  method set_blocking : bool -> unit
  method fstat : unit -> sftp_attributes
  method fstatvfs : unit -> sftp_statvfs
  method fsync : unit -> unit
  method seek : int64 -> unit
  method rewind : unit -> unit
  method tell : unit -> int64
  method read : bytes -> int -> int -> int
  method write : bytes -> int -> int -> int
end

class sftp_dir : sftp_dir_t -> object
  method readdir : unit -> sftp_attributes
  method is_eof : unit -> bool
end

class sftp : ?chan:channel -> ssh -> object
  method server_version : unit -> int
  method stat : string -> sftp_attributes
  method lstat : string -> sftp_attributes
  method statvfs : string -> sftp_statvfs
  method extension_supported : string -> string -> bool
  method extensions : unit -> (string * string) array
  method canonicalize_path : string -> string
  method open_file : string -> sftp_open_flag list -> int -> sftp_file
  method unlink : string -> unit
  method rmdir : string -> unit
  method mkdir : string -> int -> unit
  method rename : string -> string -> unit
  method chown : string -> int -> int -> unit
  method chmod : string -> int -> unit
  method symlink : string -> string -> unit
  method readlink : string -> string
  method opendir : string -> sftp_dir
end

class scp : ?recursive:bool -> ssh -> ssh_scp_request_mode -> string -> object
  method pull_request : unit -> ssh_scp_request_type
  method request_get_size : unit -> int64
  method request_get_filename : unit -> string
  method request_get_permissions : unit -> int
  method accept_request : unit -> unit
  method deny_request : string -> unit
  method read : bytes -> int -> int -> int
  method request_get_warning : unit -> string
  method push_directory : string -> int -> unit
  method leave_directory : unit -> unit
  method push_file : string -> int64 -> int -> unit
  method write : bytes -> int -> int -> unit
  method close : unit -> unit
end
