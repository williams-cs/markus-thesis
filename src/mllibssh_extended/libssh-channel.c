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

static void ssh_channel_finalize(value tv)
{
  ssh_channel c = _ssh_channel_val(tv);
  if (c) {
    // This will be handled by the outer ssh free
    // ssh_channel_free(c);
  }
}

static struct custom_operations ssh_channel_custom_operations = {
  (char *) "ssh_channel_custom_operations",
  ssh_channel_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

value Val_ssh_channel(value sshv, ssh_channel chan)
{
  CAMLparam1(sshv);
  CAMLlocal2(rv, v);

  v = caml_alloc_custom(&ssh_channel_custom_operations,
                        sizeof(ssh_channel), 0, 1);
  _ssh_channel_val(v) = chan;
  rv = caml_alloc_tuple(2);
  Store_field(rv, 0, v);
  Store_field(rv, 1, sshv);

  CAMLreturn(rv);
}

/* val ssh_channel_new : ssh_t -> channel_t */
CAMLprim value ocaml_libssh_ssh_channel_new(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  ssh_channel chan;

  chan = ssh_channel_new(session);
  if (!chan) {
    caml_raise_out_of_memory();
  }

  CAMLreturn(Val_ssh_channel(sshv, chan));
}

/* val ssh_channel_open_session : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_open_session(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_open_session(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_open_auth_agent : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_open_auth_agent(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_open_auth_agent(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_open_forward : channel_t -> string -> int -> string -> int -> unit */
CAMLprim value ocaml_libssh_ssh_channel_open_forward(value chanv, value remotehostv, value remoteportv, value sourcehostv, value localportv)
{
  CAMLparam5(chanv, remotehostv, remoteportv, sourcehostv, localportv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_open_forward(chan, String_val(remotehostv), Int_val(remoteportv), String_val(sourcehostv), Int_val(localportv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_send_eof : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_send_eof(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_send_eof(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_close : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_close(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_close(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_is_eof : channel_t -> bool */
CAMLprim value ocaml_libssh_ssh_channel_is_eof(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);

  CAMLreturn(Val_bool(ssh_channel_is_eof(chan)));
}

/* val ssh_channel_is_open : channel_t -> bool */
CAMLprim value ocaml_libssh_ssh_channel_is_open(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);

  CAMLreturn(Val_bool(ssh_channel_is_open(chan)));
}

/* val ssh_channel_is_closed : channel_t -> bool */
CAMLprim value ocaml_libssh_ssh_channel_is_closed(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);

  CAMLreturn(Val_bool(ssh_channel_is_closed(chan)));
}

/* val ssh_channel_request_exec : channel_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_exec(value chanv, value cmdv)
{
  CAMLparam2(chanv, cmdv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_exec(chan, String_val(cmdv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_shell : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_shell(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_shell(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_subsystem : channel_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_subsystem(value chanv, value subsystemv)
{
  CAMLparam2(chanv, subsystemv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_subsystem(chan, String_val(subsystemv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_auth_agent : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_auth_agent(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_auth_agent(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_env : channel_t -> string -> string -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_env(value chanv, value namev, value envvalv)
{
  CAMLparam3(chanv, namev, envvalv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_env(chan, String_val(namev), String_val(envvalv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_send_signal : channel_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_channel_send_signal(value chanv, value signalv)
{
  CAMLparam2(chanv, signalv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_send_signal(chan, String_val(signalv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_send_break : channel_t -> int -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_send_break(value chanv, value lengthv)
{
  CAMLparam2(chanv, lengthv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_send_break(chan, Int_val(lengthv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_request_sftp : channel_t -> unit */
CAMLprim value ocaml_libssh_ssh_channel_request_sftp(value chanv)
{
  CAMLparam1(chanv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int rc;

  rc = ssh_channel_request_sftp(chan);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_channel_read : channel_t -> ?is_stderr:bool -> ?timeout:int -> bytes -> int -> int -> int */
CAMLprim value ocaml_libssh_ssh_channel_read_native(value chanv, value is_stderrv, value timeoutv, value bufv, value posv, value lenv)
{
  CAMLparam5(chanv, is_stderrv, timeoutv, bufv, posv);
  CAMLxparam1(lenv);
  ssh_channel chan = Ssh_channel_val(chanv);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  const int is_stderr = Optbool_val(is_stderrv, false);
  int ret;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  ret = timeoutv == Val_int(0)
    ? ssh_channel_read(chan, Bytes_val(bufv) + pos, len, is_stderr)  /* None */
    : ssh_channel_read_timeout(chan, Bytes_val(bufv) + pos, len, is_stderr, Int_val(Field(timeoutv, 0)));  /* Some int */
  if (ret == SSH_AGAIN) {
    caml_raise_sys_blocked_io();
  }
  if (ret == SSH_EOF || ret == 0) {
    caml_raise_end_of_file();
  }
  if (ret < 0) {
    raise_libssh_error(Ssh_session_channel_val(chanv));
  }

  CAMLreturn(Val_int(ret));
}
CAMLprim value ocaml_libssh_ssh_channel_read_bytecode(value *argv, int argn)
{
  return ocaml_libssh_ssh_channel_read_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

/* val ssh_channel_write : channel_t -> ?is_stderr:bool -> bytes -> int -> int -> int */
CAMLprim value ocaml_libssh_ssh_channel_write(value chanv, value is_stderrv, value bufv, value posv, value lenv)
{
  CAMLparam5(chanv, is_stderrv, bufv, posv, lenv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  const int pos = Int_val(posv);
  const int len = Int_val(lenv);
  const int buflen = caml_string_length(bufv);
  const int (*channel_write_fn)(ssh_channel channel, const void *data, uint32_t len) = Optbool_val(is_stderrv, false) ? ssh_channel_write_stderr : ssh_channel_write;
  int ret;

  if (pos + len > buflen) {
    caml_invalid_argument("len makes the range out of the buffer");
  }

  ret = channel_write_fn(chan, Bytes_val(bufv) + pos, len);
  check_ssh_error_return_code(session, ret);

  CAMLreturn(Val_int(ret));
}

/* val ssh_channel_poll : channel_t -> ?is_stderr:bool -> ?timeout:int -> unit -> int */
CAMLprim value ocaml_libssh_ssh_channel_poll(value chanv, value is_stderrv, value timeoutv, value unitv)
{
  CAMLparam4(chanv, is_stderrv, timeoutv, unitv);
  ssh_channel chan = Ssh_channel_val(chanv);
  ssh_session session = Ssh_session_channel_val(chanv);
  int ret;

  ret = timeoutv == Val_int(0)
    ? ssh_channel_poll(chan, Optbool_val(timeoutv, false))  /* None */
    : ssh_channel_poll_timeout(chan, Int_val(Field(timeoutv, 0)), Optbool_val(timeoutv, false));  /* Some int */
  check_ssh_error_return_code(session, ret);

  CAMLreturn(Int_val(ret));
}

/* val ssh_channel_listen_forward : ?address:string -> ?port:int -> ssh_t -> int */
CAMLprim value ocaml_libssh_ssh_channel_listen_forward(value addressv, value portv, value sshv)
{
  CAMLparam3(addressv, portv, sshv);
  ssh_session session = Ssh_session_val(sshv);
  int ret;
  int bound_port;

  ret = ssh_channel_listen_forward(session, Optstring_val(addressv), Optint_val(portv, 0), &bound_port);
  check_ssh_error_return_code(session, ret);

  CAMLreturn(Val_int(bound_port));
}

/* val ssh_channel_accept_forward : ssh -> int -> (channel_t * int) option */
CAMLprim value ocaml_libssh_ssh_channel_accept_forward(value sshv, value timeoutv)
{
  CAMLparam2(sshv, timeoutv);
  CAMLlocal2(rv, v);
  ssh_session session = Ssh_session_val(sshv);
  ssh_channel chan;
  int destination_port;

  chan = ssh_channel_accept_forward(session, Int_val(timeoutv), &destination_port);
  if (chan) {  /* Some (channel_t * port) */
    v = caml_alloc(2, 0);
    Store_field(v, 0, Val_ssh_channel(sshv, chan));
    Store_field(v, 1, Val_int(destination_port));

    rv = caml_alloc(1, 0);
    Store_field(rv, 0, v);
  } else  /* None */
    rv = Val_int(0);

  CAMLreturn(rv);
}

/* val ssh_channel_cancel_forward : ssh_t -> string -> int -> unit */
CAMLprim value ocaml_libssh_ssh_channel_cancel_forward(value sshv, value addressv, value portv)
{
  CAMLparam3(sshv, addressv, portv);
  ssh_session session = Ssh_session_val(sshv);
  int ret;

  ret = ssh_channel_cancel_forward(session, String_val(addressv), Int_val(portv));
  check_ssh_error_return_code(session, ret);

  CAMLreturn(Val_unit);
}
