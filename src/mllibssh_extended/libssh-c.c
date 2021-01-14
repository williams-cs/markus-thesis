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

const char *Optstring_val(value strv)
{
  return strv == Val_int(0)
    ? NULL  /* None */
    : String_val(Field(strv, 0));  /* Some string */
}

bool Optbool_val(value boolv, bool defval)
{
  return boolv == Val_int(0)
    ? defval  /* None */
    : Bool_val(Field(boolv, 0));  /* Some bool */
}

int Optint_val(value intv, int defval)
{
  return intv == Val_int(0)
    ? defval  /* None */
    : Int_val(Field(intv, 0));  /* Some int */
}

void raise_libssh_error(ssh_session session)
{
  value args[2];
  const int code = ssh_get_error_code(session);
  const char *err_string = ssh_get_error(session);

  args[0] = Val_ssh_error_type(code);
  args[1] = caml_copy_string(err_string);

  caml_raise_with_args(*caml_named_value("Libssh.LibsshError"), 2, args);
}

void check_ssh_error_return_code(ssh_session session, int code)
{
  switch (code) {
  case SSH_ERROR:
    raise_libssh_error(session);
    break;
  case SSH_AGAIN:
    caml_raise_sys_blocked_io();
    break;
  case SSH_EOF:
    caml_raise_end_of_file();
    break;
  }
}

value Val_optstring(const char *str)
{
  CAMLparam0();
  CAMLlocal1(rv);

  if (str) {  /* Some str */
    rv = caml_alloc(1, 0);
    Store_field(rv, 0, caml_copy_string(str));
  } else  /* None */
    rv = Val_int(0);

  CAMLreturn(rv);
}

value Val_optstring_notempty(const char *str)
{
  CAMLparam0();
  CAMLlocal1(rv);

  if (str && *str) {  /* Some str */
    rv = caml_alloc(1, 0);
    Store_field(rv, 0, caml_copy_string(str));
  } else  /* None */
    rv = Val_int(0);

  CAMLreturn(rv);
}

value Val_ssh_string(const ssh_string str)
{
  CAMLparam0();

  CAMLreturn(Val_optstring(ssh_string_get_char(str)));
}

value Val_ssh_error_type(int code)
{
  CAMLparam0();
  CAMLlocal1(rv);

  if (code >= SSH_NO_ERROR && code <= SSH_EINTR) {
    rv = Val_int(code);
  } else {
    rv = caml_alloc(1, 0);
    Store_field(rv, 0, Val_int(code));
  }

  CAMLreturn(rv);
}
