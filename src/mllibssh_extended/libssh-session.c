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

#include <string.h>

static void ssh_session_finalize(value tv)
{
  ssh_session s = Ssh_session_val(tv);
  if (s) {
    ssh_free(s);
  }
}

static struct custom_operations ssh_session_custom_operations = {
  (char *) "ssh_session_custom_operations",
  ssh_session_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

value Val_ssh_session(ssh_session t)
{
  CAMLparam0();
  CAMLlocal1(rv);

  rv = caml_alloc_custom(&ssh_session_custom_operations,
                         sizeof(ssh_session), 0, 1);
  Ssh_session_val(rv) = t;

  CAMLreturn(rv);
}

/* val ssh_new : unit -> ssh_t */
CAMLprim value ocaml_libssh_libssh_new(value unitv)
{
  CAMLparam1(unitv);
  ssh_session session;

  session = ssh_new();
  if (!session) {
    caml_raise_out_of_memory();
  }

  CAMLreturn(Val_ssh_session(session));
}

/* val ssh_get_error : ssh_t -> ssh_error_type * string */
CAMLprim value ocaml_libssh_ssh_get_error(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal1(rv);
  ssh_session session = Ssh_session_val(sshv);
  const int code = ssh_get_error_code(session);
  const char *err_string = ssh_get_error(session);

  rv = caml_alloc(2, 0);

  Store_field(rv, 0, Val_int(code));
  Store_field(rv, 1, caml_copy_string(err_string));

  CAMLreturn(rv);
}

/* val ssh_options_parse_config : ssh_t -> string option -> unit */
CAMLprim value ocaml_libssh_ssh_options_parse_config(value sshv, value filenamev)
{
  CAMLparam2(sshv, filenamev);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_options_parse_config(session, Optstring_val(filenamev));
  if (rc < 0) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_unit);
}

static char *ocaml_list_to_csv_string(value lv)
{
  CAMLparam1(lv);
  CAMLlocal2(hd, lv2);
  char *str, *ptr;
  int length = 0;
  int count = 0;
  int i = 0;

  lv2 = lv;
  while (lv2 != Val_emptylist) {
    hd = Field(lv2, 0);
    length += caml_string_length(hd);
    ++count;
    lv2 = Field(lv2, 1);
  }

  length += count - 1;
  str = malloc(length + 1);
  if (!str) {
    caml_raise_out_of_memory();
  }
  ptr = str;

  lv2 = lv;
  while (lv2 != Val_emptylist) {
    int size;
    hd = Field(lv2, 0);
    size = caml_string_length(hd);
    if (i != 0) {
      *ptr = ',';
      ++ptr;
    }
    memcpy(ptr, String_val(hd), size);
    ptr += size;
    ++i;
    lv2 = Field(lv2, 1);
  }
  *ptr = '\0';

  CAMLreturnT(char *, str);
}

/* val ssh_options_set : ssh_t -> ssh_option -> unit */
CAMLprim value ocaml_libssh_ssh_options_set(value sshv, value optionv)
{
  CAMLparam2(sshv, optionv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;
  int opttype = -1;
  const char *optstring;
  char *new_optstring = NULL;
  int optint;
  long optlong;
  const void *optptr = NULL;

  switch (Tag_val(optionv)) {
  case 0:  /* SSH_OPTIONS_HOST of string */
    opttype = SSH_OPTIONS_HOST;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 1:  /* SSH_OPTIONS_PORT of int */
    opttype = SSH_OPTIONS_PORT;
    optint = Int_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 2:  /* SSH_OPTIONS_USER of string */
    opttype = SSH_OPTIONS_USER;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 3:  /* SSH_OPTIONS_SSH_DIR of string */
    opttype = SSH_OPTIONS_SSH_DIR;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 4:  /* SSH_OPTIONS_ADD_IDENTITY of string */
    opttype = SSH_OPTIONS_ADD_IDENTITY;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 5:  /* SSH_OPTIONS_KNOWNHOSTS of string */
    opttype = SSH_OPTIONS_KNOWNHOSTS;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 6:  /* SSH_OPTIONS_TIMEOUT of int */
    opttype = SSH_OPTIONS_TIMEOUT;
    optlong = Int_val(Field(optionv, 0));
    optptr = &optlong;
    break;
  case 7:  /* SSH_OPTIONS_TIMEOUT_USEC of int64 */
    opttype = SSH_OPTIONS_TIMEOUT_USEC;
    optlong = (long) Int64_val(Field(optionv, 0));
    optptr = &optlong;
    break;
  case 8:  /* SSH_OPTIONS_LOG_VERBOSITY of int */
    opttype = SSH_OPTIONS_LOG_VERBOSITY;
    optint = Int_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 9:  /* SSH_OPTIONS_CIPHERS_C_S of string list */
    opttype = SSH_OPTIONS_CIPHERS_C_S;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 10:  /* SSH_OPTIONS_CIPHERS_S_C of string list */
    opttype = SSH_OPTIONS_CIPHERS_S_C;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 11:  /* SSH_OPTIONS_COMPRESSION_C_S of string */
    opttype = SSH_OPTIONS_COMPRESSION_C_S;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 12:  /* SSH_OPTIONS_COMPRESSION_S_C of string */
    opttype = SSH_OPTIONS_COMPRESSION_S_C;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 13:  /* SSH_OPTIONS_PROXYCOMMAND of string */
    opttype = SSH_OPTIONS_PROXYCOMMAND;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 14:  /* SSH_OPTIONS_BINDADDR of string */
    opttype = SSH_OPTIONS_BINDADDR;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 15:  /* SSH_OPTIONS_STRICTHOSTKEYCHECK of bool */
    opttype = SSH_OPTIONS_STRICTHOSTKEYCHECK;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 16:  /* SSH_OPTIONS_COMPRESSION of string */
    opttype = SSH_OPTIONS_COMPRESSION;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 17:  /* SSH_OPTIONS_COMPRESSION_LEVEL of int */
    opttype = SSH_OPTIONS_COMPRESSION_LEVEL;
    optint = Int_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 18:  /* SSH_OPTIONS_KEY_EXCHANGE of string list */
    opttype = SSH_OPTIONS_KEY_EXCHANGE;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 19:  /* SSH_OPTIONS_HOSTKEYS of string list */
    opttype = SSH_OPTIONS_HOSTKEYS;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 20:  /* SSH_OPTIONS_GSSAPI_SERVER_IDENTITY of string */
    opttype = SSH_OPTIONS_GSSAPI_SERVER_IDENTITY;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 21:  /* SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY of string */
    opttype = SSH_OPTIONS_GSSAPI_CLIENT_IDENTITY;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 22:  /* SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS of bool */
    opttype = SSH_OPTIONS_GSSAPI_DELEGATE_CREDENTIALS;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 23:  /* SSH_OPTIONS_HMAC_C_S of string list */
    opttype = SSH_OPTIONS_HMAC_C_S;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 24:  /* SSH_OPTIONS_HMAC_S_C of string list */
    opttype = SSH_OPTIONS_HMAC_S_C;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  case 25:  /* SSH_OPTIONS_PASSWORD_AUTH of bool */
    opttype = SSH_OPTIONS_PASSWORD_AUTH;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 26:  /* SSH_OPTIONS_PUBKEY_AUTH of bool */
    opttype = SSH_OPTIONS_PUBKEY_AUTH;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 27:  /* SSH_OPTIONS_KBDINT_AUTH of bool */
    opttype = SSH_OPTIONS_KBDINT_AUTH;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 28:  /* SSH_OPTIONS_GSSAPI_AUTH of bool */
    opttype = SSH_OPTIONS_GSSAPI_AUTH;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 29:  /* SSH_OPTIONS_GLOBAL_KNOWNHOSTS of string */
    opttype = SSH_OPTIONS_GLOBAL_KNOWNHOSTS;
    optstring = String_val(Field(optionv, 0));
    optptr = optstring;
    break;
  case 30:  /* SSH_OPTIONS_NODELAY of bool */
    opttype = SSH_OPTIONS_NODELAY;
    optint = Bool_val(Field(optionv, 0));
    optptr = &optint;
    break;
  case 31:  /* SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES of string list */
    opttype = SSH_OPTIONS_PUBLICKEY_ACCEPTED_TYPES;
    optstring = new_optstring = ocaml_list_to_csv_string(Field(optionv, 0));
    optptr = optstring;
    break;
  }

  if (opttype != -1 && optptr) {
    rc = ssh_options_set(session, opttype, optptr);
    free(new_optstring);
    if (rc < 0) {
      raise_libssh_error(session);
    }
  }

  free(new_optstring);

  CAMLreturn(Val_unit);
}

/* val ssh_set_blocking : ssh_t -> bool -> unit */
CAMLprim value ocaml_libssh_ssh_set_blocking(value sshv, value blockingv)
{
  CAMLparam2(sshv, blockingv);
  ssh_session session = Ssh_session_val(sshv);

  ssh_set_blocking(session, Bool_val(blockingv));

  CAMLreturn(Val_unit);
}

/* val ssh_is_blocking : ssh_t -> bool */
CAMLprim value ocaml_libssh_ssh_is_blocking(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_is_blocking(session);

  CAMLreturn(Val_bool(rc));
}

/* val ssh_connect : ssh_t -> unit */
CAMLprim value ocaml_libssh_ssh_connect(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_connect(session);
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_userauth_none : ssh_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_none(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_none(session, NULL);

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_list : ssh_t -> ssh_auth_method list */
CAMLprim value ocaml_libssh_ssh_userauth_list(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal2(rv, v);
  ssh_session session = Ssh_session_val(sshv);
  int methods;

  methods = ssh_userauth_list(session, NULL);
  rv = Val_emptylist;

#define CHECK_AND_ADD(enum_value, ocaml_value) \
  do { \
    if (methods & enum_value) { \
      v = caml_alloc(2, 0); \
      Store_field(v, 0, Val_int(ocaml_value)); \
      Store_field(v, 1, rv); \
      rv = v; \
    } \
  } while (0)
  CHECK_AND_ADD(SSH_AUTH_METHOD_GSSAPI_MIC, 5);
  CHECK_AND_ADD(SSH_AUTH_METHOD_INTERACTIVE, 4);
  CHECK_AND_ADD(SSH_AUTH_METHOD_HOSTBASED, 3);
  CHECK_AND_ADD(SSH_AUTH_METHOD_PUBLICKEY, 2);
  CHECK_AND_ADD(SSH_AUTH_METHOD_PASSWORD, 1);
  CHECK_AND_ADD(SSH_AUTH_METHOD_NONE, 0);
#undef CHECK_AND_ADD

  CAMLreturn(rv);
}

/* val ssh_userauth_publickey_auto : ssh_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_publickey_auto(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_publickey_auto(session, NULL, NULL);

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_gssapi : ssh_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_gssapi(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_gssapi(session);

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_kbdint : ssh_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_kbdint(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_kbdint(session, NULL, NULL);

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_kbdint_getname : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_userauth_kbdint_getname(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);

  CAMLreturn(Val_optstring_notempty(ssh_userauth_kbdint_getname(session)));
}

/* val ssh_userauth_kbdint_getinstruction : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_userauth_kbdint_getinstruction(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);

  CAMLreturn(Val_optstring_notempty(ssh_userauth_kbdint_getinstruction(session)));
}

/* val ssh_userauth_kbdint_getprompts : ssh_t -> userauth_kbdint_prompt list */
CAMLprim value ocaml_libssh_ssh_userauth_kbdint_getprompts(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal3(rv, v, v2);
  ssh_session session = Ssh_session_val(sshv);
  int i, count;

  rv = Val_emptylist;
  count = ssh_userauth_kbdint_getnprompts(session);
  for (i = count - 1; i >= 0; --i) {
    char echo;
    const char *prompt = ssh_userauth_kbdint_getprompt(session, i, &echo);

    v2 = caml_alloc(2, 0);
    Store_field(v2, 0, caml_copy_string(prompt));
    Store_field(v2, 1, Val_bool(echo));

    v = caml_alloc(2, 0);
    Store_field(v, 0, v2);
    Store_field(v, 1, rv);
    rv = v;
  }

  CAMLreturn(rv);
}

/* val ssh_userauth_kbdint_setanswers : ssh_t -> string list -> unit */
CAMLprim value ocaml_libssh_ssh_userauth_kbdint_setanswers(value sshv, value answersv)
{
  CAMLparam2(sshv, answersv);
  CAMLlocal2(hd, lv2);
  ssh_session session = Ssh_session_val(sshv);
  int count = 0;
  unsigned int i;
  int rc;

  lv2 = answersv;
  while (lv2 != Val_emptylist) {
    hd = Field(lv2, 0);
    ++count;
    lv2 = Field(lv2, 1);
  }

  if (count != ssh_userauth_kbdint_getnprompts(session)) {
    caml_invalid_argument("the number of answers does not match the number of prompts");
  }

  lv2 = answersv;
  i = 0;
  while (lv2 != Val_emptylist) {
    hd = Field(lv2, 0);
    rc = ssh_userauth_kbdint_setanswer(session, i, String_val(hd));
    if (rc < 0) {
      raise_libssh_error(session);
    }
    ++i;
    lv2 = Field(lv2, 1);
  }

  CAMLreturn(Val_unit);
}

/* val ssh_userauth_password : ssh_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_userauth_password(value sshv, value passwordv)
{
  CAMLparam2(sshv, passwordv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_password(session, NULL, String_val(passwordv));

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_try_publickey : ssh_t -> key_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_try_publickey(value sshv, value keyv)
{
  CAMLparam2(sshv, keyv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_try_publickey(session, NULL, Ssh_key_val(keyv));

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_publickey : ssh_t -> key_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_publickey(value sshv, value keyv)
{
  CAMLparam2(sshv, keyv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_publickey(session, NULL, Ssh_key_val(keyv));

  CAMLreturn(Val_int(rc));
}

/* val ssh_userauth_agent : ssh_t -> ssh_auth_code */
CAMLprim value ocaml_libssh_ssh_userauth_agent(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_userauth_agent(session, NULL);

  CAMLreturn(Val_int(rc));
}

/* val ssh_get_issue_banner : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_issue_banner(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal1(rv);
  ssh_session session = Ssh_session_val(sshv);
  char *str;

  str = ssh_get_issue_banner(session);
  rv = Val_optstring(str);
  if (str) {
    ssh_string_free_char(str);
  }

  CAMLreturn(rv);
}

/* val ssh_disconnect : ?silent:bool -> ssh_t -> unit */
CAMLprim value ocaml_libssh_ssh_disconnect(value silentv, value sshv)
{
  CAMLparam2(silentv, sshv);
  ssh_session session = Ssh_session_val(sshv);

  if (Optbool_val(silentv, false)) {
    ssh_silent_disconnect(session);
  } else {
    ssh_disconnect(session);
  }

  CAMLreturn(Val_unit);
}

/* val ssh_get_clientbanner : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_clientbanner(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_clientbanner(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_get_serverbanner : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_serverbanner(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_serverbanner(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_is_connected : ssh_t -> bool */
CAMLprim value ocaml_libssh_ssh_is_connected(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_is_connected(session);

  CAMLreturn(Val_bool(rc));
}

/* val ssh_get_version : ssh_t -> int */
CAMLprim value ocaml_libssh_ssh_get_version(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int val;

  val = ssh_get_version(session);
  if (val < 0) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_int(val));
}

/* val ssh_blocking_flush : ssh_t -> int -> unit */
CAMLprim value ocaml_libssh_ssh_blocking_flush(value sshv, value timeoutv)
{
  CAMLparam2(sshv, timeoutv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_blocking_flush(session, Int_val(timeoutv));
  check_ssh_error_return_code(session, rc);

  CAMLreturn(Val_unit);
}

/* val ssh_get_server_publickey : ssh_t -> ssh_key */
CAMLprim value ocaml_libssh_ssh_get_server_publickey(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  ssh_key key = NULL;
  int rc;

  rc = ssh_get_server_publickey(session, &key);
  if (rc != SSH_OK) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_session_is_known_server : ssh_t -> ssh_known_hosts */
CAMLprim value ocaml_libssh_ssh_session_is_known_server(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  enum ssh_known_hosts_e status;

  status = ssh_session_is_known_server(session);
  if (status == SSH_KNOWN_HOSTS_ERROR) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_int(status + 1));
}

/* val ssh_session_update_known_hosts : ssh_t -> unit */
CAMLprim value ocaml_libssh_ssh_session_update_known_hosts(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_session_update_known_hosts(session);
  if (rc != SSH_OK) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_unit);
}

/* val ssh_get_openssh_version : ssh_t -> (int * int * int) option */
CAMLprim value ocaml_libssh_ssh_get_openssh_version(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal2(rv, v);
  ssh_session session = Ssh_session_val(sshv);
  int ver;

  ver = ssh_get_openssh_version(session);
  if (ver != 0) {  /* Some (int * int * int) */
    v = caml_alloc(3, 0);
    Store_field(v, 0, Val_int(ver >> 16));
    Store_field(v, 1, Val_int((ver >> 8) & 0xff));
    Store_field(v, 2, Val_int(ver & 0xff));

    rv = caml_alloc(1, 0);
    Store_field(rv, 0, v);
  } else  /* None */
    rv = Val_int(0);

  CAMLreturn(rv);
}

/* val ssh_get_cipher_in : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_cipher_in(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_cipher_in(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_get_cipher_out : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_cipher_out(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_cipher_out(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_get_hmac_in : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_hmac_in(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_hmac_in(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_get_hmac_out : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_hmac_out(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_hmac_out(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_session_export_known_hosts_entry : ssh_t -> string */
CAMLprim value ocaml_libssh_ssh_session_export_known_hosts_entry(value sshv)
{
  CAMLparam1(sshv);
  CAMLlocal1(rv);
  ssh_session session = Ssh_session_val(sshv);
  char *str;
  int rc;

  rc = ssh_session_export_known_hosts_entry(session, &str);
  if (rc != SSH_OK) {
    raise_libssh_error(session);
  }

  rv = caml_copy_string(str);
  ssh_string_free_char(str);

  CAMLreturn(rv);
}

/* val ssh_get_kex_algo : ssh_t -> string option */
CAMLprim value ocaml_libssh_ssh_get_kex_algo(value sshv)
{
  CAMLparam1(sshv);
  ssh_session session = Ssh_session_val(sshv);
  const char *str;

  str = ssh_get_kex_algo(session);

  CAMLreturn(Val_optstring(str));
}

/* val ssh_send_ignore : ssh_t -> string -> unit */
CAMLprim value ocaml_libssh_ssh_send_ignore(value sshv, value datav)
{
  CAMLparam2(sshv, datav);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_send_ignore(session, String_val(datav));
  if (rc != SSH_OK) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_unit);
}

/* val ssh_send_debug : ssh_t -> string -> bool -> unit */
CAMLprim value ocaml_libssh_ssh_send_debug(value sshv, value messagev, value displayv)
{
  CAMLparam3(sshv, messagev, displayv);
  ssh_session session = Ssh_session_val(sshv);
  int rc;

  rc = ssh_send_debug(session, String_val(messagev), Bool_val(displayv));
  if (rc != SSH_OK) {
    raise_libssh_error(session);
  }

  CAMLreturn(Val_unit);
}
