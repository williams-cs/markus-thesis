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

/* val libssh_version : int * int * int */
CAMLprim value ocaml_libssh_libssh_version(value unitv)
{
  CAMLparam1(unitv);
  CAMLlocal1(rv);

  rv = caml_alloc(3, 0);

  Store_field(rv, 0, Val_int(LIBSSH_VERSION_MAJOR));
  Store_field(rv, 1, Val_int(LIBSSH_VERSION_MINOR));
  Store_field(rv, 2, Val_int(LIBSSH_VERSION_MICRO));

  CAMLreturn(rv);
}

/* val libssh_version_string : unit -> string */
CAMLprim value ocaml_libssh_libssh_version_string(value unitv)
{
  CAMLparam1(unitv);

  CAMLreturn(caml_copy_string(ssh_version(0)));
}

/* val ssh_set_log_level : ssh_log_type -> unit */
CAMLprim value ocaml_libssh_ssh_set_log_level(value levelv)
{
  CAMLparam1(levelv);

  ssh_set_log_level(Int_val(levelv));

  CAMLreturn(Val_unit);
}

/* val ssh_get_log_level : unit -> ssh_log_type */
CAMLprim value ocaml_libssh_ssh_get_log_level(value unitv)
{
  CAMLparam1(unitv);

  CAMLreturn(Val_int(ssh_get_log_level()));
}

/* val ssh_copyright : unit -> string */
CAMLprim value ocaml_libssh_ssh_copyright(value unitv)
{
  CAMLparam1(unitv);

  CAMLreturn(caml_copy_string(ssh_copyright()));
}

/* val ssh_get_fingerprint_hash : ssh_publickey_hash -> bytes -> string */
CAMLprim value ocaml_libssh_ssh_get_fingerprint_hash(value hashtypev, value hashv)
{
  CAMLparam2(hashtypev, hashv);
  CAMLlocal1(rv);
  char *ret;

  ret = ssh_get_fingerprint_hash(Int_val(hashtypev), Bytes_val(hashv), caml_string_length(hashv));
  if (!ret) {
    caml_failwith("ssh_get_fingerprint_hash failed");
  }

  rv = caml_copy_string(ret);
  ssh_string_free_char(ret);

  CAMLreturn(rv);
}

/* val ssh_known_hosts_parse_line : string -> string -> ssh_knownhosts_entry_internal */
CAMLprim value ocaml_libssh_ssh_known_hosts_parse_line(value hostnamev, value linev)
{
  CAMLparam2(hostnamev, linev);
  CAMLlocal1(rv);
  int rc;
  struct ssh_knownhosts_entry *entry = NULL;

  rc = ssh_known_hosts_parse_line(String_val(hostnamev), String_val(linev), &entry);
  if (rc != SSH_OK) {
    caml_failwith("ssh_known_hosts_parse_line failed");
  }

  rv = caml_alloc(4, 0);

  Store_field(rv, 0, caml_copy_string(entry->hostname));
  Store_field(rv, 1, caml_copy_string(entry->unparsed));
  Store_field(rv, 2, Val_ssh_key(entry->publickey));
  Store_field(rv, 3, Val_optstring(entry->comment));

  // Val_ssh_key() took ownership of the private key, so do not try
  // to free it now
  entry->publickey = NULL;
  ssh_knownhosts_entry_free(entry);

  CAMLreturn(rv);
}
