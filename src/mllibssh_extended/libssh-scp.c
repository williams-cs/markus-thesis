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

static void ssh_scp_finalize(value tv)
{
  ssh_scp s = _ssh_scp_val(tv);
  if (s) {
    ssh_scp_free(s);
  }
}

static struct custom_operations ssh_scp_custom_operations = {
  (char *) "ssh_scp_custom_operations",
  ssh_scp_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

value Val_ssh_scp(value sshv, ssh_scp scp)
{
  CAMLparam1(sshv);
  CAMLlocal2(rv, v);

  v = caml_alloc_custom(&ssh_scp_custom_operations,
                        sizeof(ssh_scp), 0, 1);
  _ssh_scp_val(v) = scp;
  rv = caml_alloc_tuple(2);
  Store_field(rv, 0, v);
  Store_field(rv, 1, sshv);

  CAMLreturn(rv);
}

static value Val_ssh_scp_request_type(int type)
{
  CAMLparam0();
  CAMLlocal1(rv);

  switch (type) {
  case SSH_SCP_REQUEST_NEWDIR:
    rv = Val_int(0);
    break;
  case SSH_SCP_REQUEST_NEWFILE:
    rv = Val_int(1);
    break;
  case SSH_SCP_REQUEST_ENDDIR:
    rv = Val_int(2);
    break;
  case SSH_SCP_REQUEST_WARNING:
    rv = Val_int(3);
    break;
  case SSH_SCP_REQUEST_EOF:
  default:
    caml_failwith("unknown SSH_SCP_REQUEST_*");
  }

  CAMLreturn(rv);
}

/* val ssh_scp_new : ?recursive -> ssh_t -> ssh_scp_request_mode -> string -> scp_t */
CAMLprim value ocaml_libssh_ssh_scp_new(value recursivev, value sshv, value modev, value locationv)
{
  CAMLparam4(recursivev, sshv, modev, locationv);
  CAMLlocal1(rv);
  ssh_session session = Ssh_session_val(sshv);
  int mode = 0;
  ssh_scp scp;
  int rc;

  switch (Int_val(modev)) {
  case 0:  /* SSH_SCP_WRITE */
    mode |= SSH_SCP_WRITE;
    break;
  case 1:  /* SSH_SCP_READ */
    mode |= SSH_SCP_READ;
    break;
  }
  if (Optbool_val(recursivev, false)) {
    mode |= SSH_SCP_RECURSIVE;
  }

  scp = ssh_scp_new(session, mode, String_val(locationv));
  if (!scp) {
    caml_raise_out_of_memory();
  }

  rc = ssh_scp_init(scp);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_ssh_scp(sshv, scp));
}

/* val ssh_scp_pull_request : scp_t -> ssh_scp_request_type */
CAMLprim value ocaml_libssh_ssh_scp_pull_request(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_pull_request(scp);
  check_ssh_error_return_code(session, rc);
  if (rc == SSH_SCP_REQUEST_EOF) {
    caml_raise_end_of_file();
  }

  CAMLreturn(Val_ssh_scp_request_type(rc));
}

/* val ssh_scp_request_get_size : scp_t -> int64 */
CAMLprim value ocaml_libssh_ssh_scp_request_get_size(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);

  CAMLreturn(Val_int64(ssh_scp_request_get_size64(scp)));
}

/* val ssh_scp_request_get_filename : scp_t -> string */
CAMLprim value ocaml_libssh_ssh_scp_request_get_filename(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  const char *str;

  str = ssh_scp_request_get_filename(scp);
  if (!str) {
    raise_libssh_error(session);
  }

  CAMLreturn(caml_copy_string(str));
}

/* val ssh_scp_request_get_permissions : scp_t -> int */
CAMLprim value ocaml_libssh_ssh_scp_request_get_permissions(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);

  CAMLreturn(Val_int(ssh_scp_request_get_permissions(scp)));
}

/* val ssh_scp_accept_request : scp_t -> unit */
CAMLprim value ocaml_libssh_ssh_scp_accept_request(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_accept_request(scp);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_deny_request : scp_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_scp_deny_request(value scpv, value reasonv)
{
  CAMLparam2(scpv, reasonv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_deny_request(scp, String_val(reasonv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_read : scp_t -> bytes -> int -> int -> int */
CAMLprim value ocaml_libssh_ssh_scp_read(value scpv, value bufv, value posv, value lenv)
{
  CAMLparam4(scpv, bufv, posv, lenv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  int ret;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  ret = ssh_scp_read(scp, Bytes_val(bufv) + pos, len);
  check_ssh_error_return_code(session, ret);

  CAMLreturn(Val_int(ret));
}

/* val ssh_scp_request_get_warning : scp_t -> string */
CAMLprim value ocaml_libssh_ssh_scp_request_get_warning(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  const char *str;

  str = ssh_scp_request_get_warning(scp);
  if (!str) {
    raise_libssh_error(Ssh_session_scp_val(scpv));
  }

  CAMLreturn(caml_copy_string(str));
}

/* val ssh_scp_push_directory : scp_t -> string -> int -> unit */
CAMLprim value ocaml_libssh_ssh_scp_push_directory(value scpv, value directoryv, value modev)
{
  CAMLparam3(scpv, directoryv, modev);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_push_directory(scp, String_val(directoryv), Int_val(modev));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_leave_directory : scp_t -> unit */
CAMLprim value ocaml_libssh_ssh_scp_leave_directory(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_leave_directory(scp);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_push_file : scp_t -> string -> int64 -> int -> unit */
CAMLprim value ocaml_libssh_ssh_scp_push_file(value scpv, value filenamev, value sizev, value modev)
{
  CAMLparam4(scpv, filenamev, sizev, modev);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_push_file64(scp, String_val(filenamev), Int64_val(sizev), Int_val(modev));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_write : scp_t -> bytes -> int -> int -> unit */
CAMLprim value ocaml_libssh_ssh_scp_write(value scpv, value bufv, value posv, value lenv)
{
  CAMLparam4(scpv, bufv, posv, lenv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  int rc;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  rc = ssh_scp_write(scp, Bytes_val(bufv) + pos, len);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_scp_close : scp_t -> unit */
CAMLprim value ocaml_libssh_ssh_scp_close(value scpv)
{
  CAMLparam1(scpv);
  ssh_scp scp = Ssh_scp_val(scpv);
  ssh_session session = Ssh_session_scp_val(scpv);
  int rc;

  rc = ssh_scp_close(scp);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}
