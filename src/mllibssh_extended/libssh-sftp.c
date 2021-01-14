/*
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
 */

#include "libssh-ocaml.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static value Val_sftp_error_type(int code)
{
  CAMLparam0();
  CAMLlocal1(rv);

  if (code >= SSH_FX_OK && code <= SSH_FX_NO_MEDIA) {
    rv = Val_int(code);
  } else {
    rv = caml_alloc(1, 0);
    Store_field(rv, 0, Val_int(code));
  }

  CAMLreturn(rv);
}

static void raise_libsshsftp_error(sftp_session sftp, ssh_session session)
{
  value args[3];
  const int sftp_code = sftp_get_error(sftp);
  const int code = ssh_get_error_code(session);
  const char *err_string = ssh_get_error(session);

  args[0] = Val_sftp_error_type(sftp_code);
  args[1] = Val_ssh_error_type(code);
  args[2] = caml_copy_string(err_string);

  caml_raise_with_args(*caml_named_value("Libssh.LibsshSftpError"), 3, args);
}

static void raise_libsshsftp_error_from_file(value filev)
{
  CAMLparam1(filev);
  CAMLlocal1(sftpv);

  sftpv = Field(filev, 1);
  raise_libsshsftp_error(Sftp_session_val(sftpv), Ssh_session_sftp_val(sftpv));

  CAMLreturn0;
}

static void sftp_session_finalize(value tv)
{
  sftp_session s = _sftp_session_val(tv);
  if (s) {
    sftp_free(s);
  }
}

static struct custom_operations sftp_session_custom_operations = {
  (char *) "sftp_session_custom_operations",
  sftp_session_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

static void sftp_file_finalize(value tv)
{
  sftp_file f = _sftp_file_val(tv);
  if (f) {
    sftp_close(f);
  }
}

static struct custom_operations sftp_file_custom_operations = {
  (char *) "sftp_file_custom_operations",
  sftp_file_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

static void sftp_dir_finalize(value tv)
{
  sftp_dir d = _sftp_dir_val(tv);
  if (d) {
    sftp_closedir(d);
  }
}

static struct custom_operations sftp_dir_custom_operations = {
  (char *) "sftp_dir_custom_operations",
  sftp_dir_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

value Val_sftp_session(value sshv, sftp_session sftp)
{
  CAMLparam1(sshv);
  CAMLlocal2(rv, v);

  v = caml_alloc_custom(&sftp_session_custom_operations,
                        sizeof(sftp_session), 0, 1);
  _sftp_session_val(v) = sftp;
  rv = caml_alloc_tuple(2);
  Store_field(rv, 0, v);
  Store_field(rv, 1, sshv);

  CAMLreturn(rv);
}

value Val_sftp_file(value sftpv, sftp_file file)
{
  CAMLparam1(sftpv);
  CAMLlocal2(rv, v);

  v = caml_alloc_custom(&sftp_file_custom_operations,
                        sizeof(sftp_file), 0, 1);
  _sftp_file_val(v) = file;
  rv = caml_alloc_tuple(2);
  Store_field(rv, 0, v);
  Store_field(rv, 1, sftpv);

  CAMLreturn(rv);
}

value Val_sftp_dir(value sftpv, sftp_dir dir)
{
  CAMLparam1(sftpv);
  CAMLlocal2(rv, v);

  v = caml_alloc_custom(&sftp_dir_custom_operations,
                        sizeof(sftp_dir), 0, 1);
  _sftp_dir_val(v) = dir;
  rv = caml_alloc_tuple(2);
  Store_field(rv, 0, v);
  Store_field(rv, 1, sftpv);

  CAMLreturn(rv);
}

static value Val_sftp_filetype(int type)
{
  CAMLparam0();
  CAMLlocal1(rv);

  switch (type) {
  case SSH_FILEXFER_TYPE_REGULAR:
    rv = Val_int(1);
    break;
  case SSH_FILEXFER_TYPE_DIRECTORY:
    rv = Val_int(2);
    break;
  case SSH_FILEXFER_TYPE_SYMLINK:
    rv = Val_int(3);
    break;
  case SSH_FILEXFER_TYPE_SPECIAL:
    rv = Val_int(4);
    break;
  case SSH_FILEXFER_TYPE_UNKNOWN:
  default:
    rv = Val_int(0);
    break;
  }

  CAMLreturn(rv);
}

static value Val_sftp_attributes(sftp_attributes attrs)
{
  CAMLparam0();
  CAMLlocal3(rv, flagsv, v);

  flagsv = Val_emptylist;
#define CHECK_AND_ADD(enum_value, ocaml_value) \
  do { \
    if (attrs->flags & enum_value) { \
      v = caml_alloc(2, 0); \
      Store_field(v, 0, Val_int(ocaml_value)); \
      Store_field(v, 1, flagsv); \
      flagsv = v; \
    } \
  } while (0)
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_UIDGID, 10);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_EXTENDED, 9);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_SUBSECOND_TIMES, 8);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_OWNERGROUP, 7);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_ACL, 6);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_MODIFYTIME, 5);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_CREATETIME, 4);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_ACMODTIME, 3);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_ACCESSTIME, 2);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_PERMISSIONS, 1);
  CHECK_AND_ADD(SSH_FILEXFER_ATTR_SIZE, 0);
#undef CHECK_AND_ADD

  rv = caml_alloc(22, 0);
  Store_field(rv, 0, Val_optstring(attrs->name));
  Store_field(rv, 1, Val_optstring(attrs->longname));
  Store_field(rv, 2, flagsv);
  Store_field(rv, 3, Val_sftp_filetype(attrs->type));
  Store_field(rv, 4, Val_int64(attrs->size));
  Store_field(rv, 5, Val_int(attrs->uid));
  Store_field(rv, 6, Val_int(attrs->gid));
  Store_field(rv, 7, Val_optstring(attrs->owner));
  Store_field(rv, 8, Val_optstring(attrs->group));
  Store_field(rv, 9, Val_int(attrs->permissions));
  Store_field(rv, 10, Val_int64(attrs->atime64));
  Store_field(rv, 12, Val_int(attrs->atime));
  Store_field(rv, 13, Val_int(attrs->atime_nseconds));
  Store_field(rv, 14, Val_int64(attrs->createtime));
  Store_field(rv, 15, Val_int(attrs->createtime_nseconds));
  Store_field(rv, 16, Val_int64(attrs->mtime64));
  Store_field(rv, 17, Val_int(attrs->mtime));
  Store_field(rv, 18, Val_int(attrs->mtime_nseconds));
  Store_field(rv, 19, Val_ssh_string(attrs->acl));
  Store_field(rv, 20, Val_int(attrs->extended_count));
  Store_field(rv, 21, Val_ssh_string(attrs->extended_type));
  Store_field(rv, 21, Val_ssh_string(attrs->extended_data));

  CAMLreturn(rv);
}

static value Val_sftp_statvfs(sftp_statvfs_t sb)
{
  CAMLparam0();
  CAMLlocal3(rv, flagsv, v);

  flagsv = Val_emptylist;
#define CHECK_AND_ADD(enum_value, ocaml_value) \
  do { \
    if (sb->f_flag & enum_value) { \
      v = caml_alloc(2, 0); \
      Store_field(v, 0, Val_int(ocaml_value)); \
      Store_field(v, 1, flagsv); \
      flagsv = v; \
    } \
  } while (0)
  CHECK_AND_ADD(SSH_FXE_STATVFS_ST_NOSUID, 1);
  CHECK_AND_ADD(SSH_FXE_STATVFS_ST_RDONLY, 0);
#undef CHECK_AND_ADD

  rv = caml_alloc(11, 0);
  Store_field(rv, 0, Val_int64(sb->f_bsize));
  Store_field(rv, 1, Val_int64(sb->f_frsize));
  Store_field(rv, 2, Val_int64(sb->f_blocks));
  Store_field(rv, 3, Val_int64(sb->f_bfree));
  Store_field(rv, 4, Val_int64(sb->f_bavail));
  Store_field(rv, 5, Val_int64(sb->f_files));
  Store_field(rv, 6, Val_int64(sb->f_ffree));
  Store_field(rv, 7, Val_int64(sb->f_favail));
  Store_field(rv, 8, Val_int64(sb->f_fsid));
  Store_field(rv, 9, flagsv);
  Store_field(rv, 10, Val_int64(sb->f_namemax));

  CAMLreturn(rv);
}

/* val sftp_file_set_blocking : sftp_file_t -> bool -> unit */
CAMLprim value ocaml_libssh_sftp_file_set_blocking(value filev, value blockingv)
{
  CAMLparam2(filev, blockingv);
  sftp_file file = Sftp_file_val(filev);

  if (Bool_val(blockingv)) {
    sftp_file_set_blocking(file);
  } else {
    sftp_file_set_nonblocking(file);
  }

  CAMLreturn(Val_unit);
}

/* val sftp_fstat : sftp_file_t -> sftp_attributes */
CAMLprim value ocaml_libssh_sftp_fstat(value filev)
{
  CAMLparam1(filev);
  CAMLlocal1(rv);
  sftp_file file = Sftp_file_val(filev);
  sftp_attributes attrs;

  attrs = sftp_fstat(file);
  if (!attrs) {
    raise_libsshsftp_error_from_file(filev);
  }

  rv = Val_sftp_attributes(attrs);
  sftp_attributes_free(attrs);

  CAMLreturn(rv);
}

/* val sftp_fstatvfs : sftp_file_t -> sftp_statvfs */
CAMLprim value ocaml_libssh_sftp_fstatvfs(value filev)
{
  CAMLparam1(filev);
  CAMLlocal1(rv);
  sftp_file file = Sftp_file_val(filev);
  sftp_statvfs_t sb;

  sb = sftp_fstatvfs(file);
  if (!sb) {
    raise_libsshsftp_error_from_file(filev);
  }

  rv = Val_sftp_statvfs(sb);
  sftp_statvfs_free(sb);

  CAMLreturn(rv);
}

/* val sftp_fsync : sftp_file_t -> unit */
CAMLprim value ocaml_libssh_sftp_fsync(value filev)
{
  CAMLparam1(filev);
  sftp_file file = Sftp_file_val(filev);
  int rc;

  rc = sftp_fsync(file);
  if (rc < 0) {
    raise_libsshsftp_error_from_file(filev);
  }

  CAMLreturn(Val_unit);
}

/* val sftp_seek : sftp_file_t -> int64 -> unit */
CAMLprim value ocaml_libssh_sftp_seek(value filev, value offsetv)
{
  CAMLparam2(filev, offsetv);
  CAMLlocal1(rv);
  sftp_file file = Sftp_file_val(filev);
  int rc;

  rc = sftp_seek(file, Int64_val(offsetv));
  if (rc < 0) {
    raise_libsshsftp_error_from_file(filev);
  }

  CAMLreturn(Val_unit);
}

/* val sftp_rewind : sftp_file_t -> unit */
CAMLprim value ocaml_libssh_sftp_rewind(value filev)
{
  CAMLparam1(filev);
  CAMLlocal1(rv);
  sftp_file file = Sftp_file_val(filev);

  sftp_rewind(file);

  CAMLreturn(Val_unit);
}

/* val sftp_tell : sftp_file_t -> int64 */
CAMLprim value ocaml_libssh_sftp_tell(value filev)
{
  CAMLparam1(filev);
  CAMLlocal1(rv);
  sftp_file file = Sftp_file_val(filev);
  uint64_t val;

  val = sftp_tell64(file);

  CAMLreturn(Val_int64(val));
}

/* val sftp_read : sftp_file_t -> bytes -> int -> int -> int */
CAMLprim value ocaml_libssh_sftp_read(value filev, value bufv, value posv, value lenv)
{
  CAMLparam4(filev, bufv, posv, lenv);
  sftp_file file = Sftp_file_val(filev);
  sftp_session sftp = Sftp_session_file_val(filev);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  ssize_t ret;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  ret = sftp_read(file, Bytes_val(bufv) + pos, MIN(len, 16384));
  if (ret == SSH_AGAIN) {
    caml_raise_sys_blocked_io();
  }
  if (ret == SSH_EOF || (ret == 0 && sftp_get_error(sftp) == SSH_FX_EOF)) {
    caml_raise_end_of_file();
  }
  if (ret <= 0) {
    raise_libsshsftp_error_from_file(filev);
  }

  CAMLreturn(Val_int(ret));
}

/* val sftp_write : sftp_file_t -> bytes -> int -> int -> int */
CAMLprim value ocaml_libssh_sftp_write(value filev, value bufv, value posv, value lenv)
{
  CAMLparam4(filev, bufv, posv, lenv);
  sftp_file file = Sftp_file_val(filev);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  ssize_t ret;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  ret = sftp_write(file, Bytes_val(bufv) + pos, MIN(len, 131072));
  check_ssh_error_return_code(Ssh_session_sftp_val(filev), ret);

  CAMLreturn(Val_int(ret));
}

/* val sftp_readdir : sftp_dir_t -> sftp_attributes */
CAMLprim value ocaml_libssh_sftp_readdir(value dirv)
{
  CAMLparam1(dirv);
  CAMLlocal1(rv);
  sftp_dir dir = Sftp_dir_val(dirv);
  sftp_session sftp = Sftp_session_dir_val(dirv);
  sftp_attributes attrs;

  if (sftp_dir_eof(dir)) {
    caml_raise_end_of_file();
  }

  attrs = sftp_readdir(sftp, dir);
  if (!attrs) {
    caml_raise_end_of_file();
  }

  rv = Val_sftp_attributes(attrs);
  sftp_attributes_free(attrs);

  CAMLreturn(rv);
}

/* val sftp_dir_eof : sftp_dir_t -> bool */
CAMLprim value ocaml_libssh_sftp_dir_eof(value dirv)
{
  CAMLparam1(dirv);
  sftp_dir dir = Sftp_dir_val(dirv);

  CAMLreturn(Val_bool(sftp_dir_eof(dir)));
}

/* val sftp_new : ssh_t -> sftp_t */
CAMLprim value ocaml_libssh_sftp_new(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  sftp_session sftp;
  int rc;

  sftp = sftp_new(session);
  if (!sftp) {
    caml_raise_out_of_memory();
  }

  rc = sftp_init(sftp);
  if (rc < 0) {
    raise_libsshsftp_error(sftp, session);
  }

  CAMLreturn(Val_sftp_session(sshv, sftp));
}

/* val sftp_new_channel : ssh_t -> sftp_t */
CAMLprim value ocaml_libssh_sftp_new_channel(value sshv, value chanv)
{
  CAMLparam2(sshv, chanv);
  ssh_session session = Ssh_session_val(sshv);
  ssh_channel chan = Ssh_channel_val(chanv);
  sftp_session sftp;
  int rc;

  sftp = sftp_new_channel(session, chan);
  if (!sftp) {
    caml_raise_out_of_memory();
  }

  rc = sftp_init(sftp);
  if (rc < 0) {
    raise_libsshsftp_error(sftp, session);
  }

  CAMLreturn(Val_sftp_session(sshv, sftp));
}

/* val sftp_server_version : sftp_t -> int */
CAMLprim value ocaml_libssh_sftp_server_version(value sftpv)
{
  CAMLparam1(sftpv);
  sftp_session sftp = Sftp_session_val(sftpv);

  CAMLreturn(Val_int(sftp_server_version(sftp)));
}

/* val sftp_stat : sftp_t -> string -> sftp_attributes */
CAMLprim value ocaml_libssh_sftp_stat(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  CAMLlocal1(rv);
  sftp_session sftp = Sftp_session_val(sftpv);
  sftp_attributes attrs;

  attrs = sftp_stat(sftp, String_val(pathv));
  if (!attrs) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  rv = Val_sftp_attributes(attrs);
  sftp_attributes_free(attrs);

  CAMLreturn(rv);
}

/* val sftp_lstat : sftp_t -> string -> sftp_attributes */
CAMLprim value ocaml_libssh_sftp_lstat(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  CAMLlocal1(rv);
  sftp_session sftp = Sftp_session_val(sftpv);
  sftp_attributes attrs;

  attrs = sftp_lstat(sftp, String_val(pathv));
  if (!attrs) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  rv = Val_sftp_attributes(attrs);
  sftp_attributes_free(attrs);

  CAMLreturn(rv);
}

/* val sftp_statvfs : sftp_t -> string -> sftp_statvfs */
CAMLprim value ocaml_libssh_sftp_statvfs(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  CAMLlocal1(rv);
  sftp_session sftp = Sftp_session_val(sftpv);
  sftp_statvfs_t sb;

  sb = sftp_statvfs(sftp, String_val(pathv));
  if (!sb) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  rv = Val_sftp_statvfs(sb);
  sftp_statvfs_free(sb);

  CAMLreturn(rv);
}

/* val sftp_extension_supported : sftp_t -> string -> string -> bool */
CAMLprim value ocaml_libssh_sftp_extension_supported(value sftpv, value namev, value datav)
{
  CAMLparam3(sftpv, namev, datav);
  sftp_session sftp = Sftp_session_val(sftpv);

  CAMLreturn(Val_bool(sftp_extension_supported(sftp, String_val(namev), String_val(datav))));
}

/* val sftp_extensions : sftp_t -> (string * string) array */
CAMLprim value ocaml_libssh_sftp_extensions(value sftpv)
{
  CAMLparam1(sftpv);
  CAMLlocal2(rv, v);
  sftp_session sftp = Sftp_session_val(sftpv);
  int i, count;

  count = sftp_extensions_get_count(sftp);
  rv = caml_alloc(count, 0);
  for (i = 0; i < count; ++i) {
    const char *name = sftp_extensions_get_name(sftp, i);
    const char *data = sftp_extensions_get_data(sftp, i);

    v = caml_alloc(2, 0);
    Store_field(v, 0, caml_copy_string(name));
    Store_field(v, 1, caml_copy_string(data));

    Store_field(rv, i, v);
  }

  CAMLreturn(rv);
}

/* val sftp_canonicalize_path : sftp_t -> string -> string */
CAMLprim value ocaml_libssh_sftp_canonicalize_path(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  CAMLlocal1(rv);
  sftp_session sftp = Sftp_session_val(sftpv);
  char *str;

  str = sftp_canonicalize_path(sftp, String_val(pathv));
  if (!str) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  rv = caml_copy_string(str);
  ssh_string_free_char(str);

  CAMLreturn(rv);
}

/* val sftp_open : sftp_t -> string -> sftp_open_flag list -> int -> sftp_file_t */
CAMLprim value ocaml_libssh_sftp_open(value sftpv, value pathv, value flagsv, value modev)
{
  CAMLparam4(sftpv, pathv, flagsv, modev);
  CAMLlocal1(hd);
  sftp_session sftp = Sftp_session_val(sftpv);
  sftp_file file;
  int flags = 0;

  while (flagsv != Val_emptylist) {
    hd = Field(flagsv, 0);
    switch (Int_val(hd)) {
    case 0:  /* O_RDONLY */
      flags |= O_RDONLY;
      break;
    case 1:  /* O_WRONLY */
      flags |= O_WRONLY;
      break;
    case 2:  /* O_RDWR */
      flags |= O_RDWR;
      break;
    case 3:  /* O_CREAT */
      flags |= O_CREAT;
      break;
    case 4:  /* O_EXCL */
      flags |= O_EXCL;
      break;
    case 5:  /* O_TRUNC */
      flags |= O_TRUNC;
      break;
    }
    flagsv = Field(flagsv, 1);
  }

  file = sftp_open(sftp, String_val(pathv), flags, Int_val(modev));
  if (!file) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_sftp_file(sftpv, file));
}

/* val sftp_unlink : sftp_t -> string -> unit */
CAMLprim value ocaml_libssh_sftp_unlink(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_unlink(sftp, String_val(pathv));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_rmdir : sftp_t -> string -> unit */
CAMLprim value ocaml_libssh_sftp_rmdir(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_rmdir(sftp, String_val(pathv));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_mkdir : sftp_t -> string -> int -> unit */
CAMLprim value ocaml_libssh_sftp_mkdir(value sftpv, value pathv, value modev)
{
  CAMLparam3(sftpv, pathv, modev);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_mkdir(sftp, String_val(pathv), Int_val(modev));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_rename : sftp_t -> string -> string -> unit */
CAMLprim value ocaml_libssh_sftp_rename(value sftpv, value origv, value destv)
{
  CAMLparam3(sftpv, origv, destv);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_rename(sftp, String_val(origv), String_val(destv));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_chown : sftp_t -> string -> int -> int -> unit */
CAMLprim value ocaml_libssh_sftp_chown(value sftpv, value pathv, value ownerv, value groupv)
{
  CAMLparam4(sftpv, pathv, ownerv, groupv);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_chown(sftp, String_val(pathv), Int_val(ownerv), Int_val(groupv));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_chmod : sftp_t -> string -> int -> unit */
CAMLprim value ocaml_libssh_sftp_chmod(value sftpv, value pathv, value modev)
{
  CAMLparam3(sftpv, pathv, modev);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_chmod(sftp, String_val(pathv), Int_val(modev));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_symlink : sftp_t -> string -> string -> unit */
CAMLprim value ocaml_libssh_sftp_symlink(value sftpv, value targetv, value destv)
{
  CAMLparam3(sftpv, targetv, destv);
  sftp_session sftp = Sftp_session_val(sftpv);
  int rc;

  rc = sftp_symlink(sftp, String_val(targetv), String_val(destv));
  if (rc < 0) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_unit);
}

/* val sftp_readlink : sftp_t -> string -> unit */
CAMLprim value ocaml_libssh_sftp_readlink(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  CAMLlocal1(rv);
  sftp_session sftp = Sftp_session_val(sftpv);
  char *str;

  str = sftp_readlink(sftp, String_val(pathv));
  if (!str) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  rv = caml_copy_string(str);
  ssh_string_free_char(str);

  CAMLreturn(rv);
}

/* val sftp_opendir : sftp_t -> sftp_dir_t */
CAMLprim value ocaml_libssh_sftp_opendir(value sftpv, value pathv)
{
  CAMLparam2(sftpv, pathv);
  sftp_session sftp = Sftp_session_val(sftpv);
  sftp_dir dir;

  dir = sftp_opendir(sftp, String_val(pathv));
  if (!dir) {
    raise_libsshsftp_error(sftp, Ssh_session_sftp_val(sftpv));
  }

  CAMLreturn(Val_sftp_dir(sftpv, dir));
}
