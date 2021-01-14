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

#include <assert.h>
#include <string.h>

static void ssh_key_finalize(value tv)
{
  ssh_key k = Ssh_key_val(tv);
  if (k) {
    ssh_key_free(k);
  }
}

static struct custom_operations ssh_key_custom_operations = {
  (char *) "ssh_key_custom_operations",
  ssh_key_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
};

struct ocaml_auth_callback_data {
  value *fvp;                  /* The OCaml auth callback. */
};

enum ocaml_auth_data_type {
  auth_passphrase,
  auth_callback
};

struct ocaml_auth_data {
  struct ocaml_auth_callback_data cb_data;
  enum ocaml_auth_data_type type;
  value fv;
  ssh_auth_callback cb;
  char *passphrase;
};

static int ocaml_ssh_auth_callback(const char *prompt, char *buf, size_t len, int echo, int verify, void *userdata)
{
  CAMLparam0();
  CAMLlocal2(rv, v);
  struct ocaml_auth_callback_data *s = userdata;
  int v_len;

  /* Call the auth callback. */
  rv = caml_callback3_exn(*s->fvp, caml_copy_string(prompt), Val_bool(echo), Val_bool(verify));
  if (Is_exception_result(rv)) {
    /* The callback raised an exception, so return an error. */
    CAMLreturnT(int, SSH_ERROR);
  }

  if (rv == Val_int(0)) {
    CAMLreturnT(int, SSH_ERROR);
  }

  v = Field(rv, 0);
  v_len = caml_string_length(v);
  if (v_len >= len) {
    CAMLreturnT(int, SSH_ERROR);
  }

  memcpy(buf, String_val(v), v_len);

  CAMLreturnT(int, SSH_OK);
}

static void callback_unpack(value authv, struct ocaml_auth_data *data)
{
  CAMLparam1(authv);

  memset(data, 0, sizeof(*data));
  if (authv != Val_int(0)) {
    switch (Tag_val(Field(authv, 0))) {
    case 0:  /* passphrase */
      data->type = auth_passphrase;
      // keep a copy of the passphrase string, as its value could move around
      // when calling other OCaml code that allocates memory
      data->passphrase = strdup(String_val(Field(Field(authv, 0), 0)));
      if (data->passphrase == NULL)
        caml_raise_out_of_memory();
      break;
    case 1:  /* callback */
      data->type = auth_callback;
      data->fv = Field(Field(authv, 0), 0);
      data->cb_data.fvp = &data->fv;
      data->cb = &ocaml_ssh_auth_callback;
      // make sure the value is kept around
      caml_register_generational_global_root(&data->fv);
      break;
    default:
      caml_failwith("internal error: unhandled unpack tag");
    }
  }

  CAMLreturn0;
}

static void callback_free(struct ocaml_auth_data *data)
{
  switch (data->type) {
  case auth_passphrase:
    free(data->passphrase);
    break;
  case auth_callback:
    caml_remove_generational_global_root(&data->fv);
    break;
  }
}

value Val_ssh_key(ssh_key k)
{
  CAMLparam0();
  CAMLlocal1(rv);

  rv = caml_alloc_custom(&ssh_key_custom_operations,
                         sizeof(ssh_key), 0, 1);
  Ssh_key_val(rv) = k;

  CAMLreturn(rv);
}

/* val ssh_pki_generate : ssh_keytype -> int -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_generate(value typev, value paramv)
{
  CAMLparam2(typev, paramv);
  int rc;
  ssh_key key = NULL;

  rc = ssh_pki_generate(Int_val(typev), Int_val(paramv), &key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_generate failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_pki_import_pubkey_base64 : bytes -> ssh_keytype -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_pubkey_base64(value datav, value typev)
{
  CAMLparam2(datav, typev);
  int rc;
  ssh_key key = NULL;

  rc = ssh_pki_import_pubkey_base64((const char *)Bytes_val(datav), Int_val(typev), &key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_pubkey_base64 failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_pki_import_pubkey_file : string -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_pubkey_file(value filenamev)
{
  CAMLparam1(filenamev);
  int rc;
  ssh_key key = NULL;

  rc = ssh_pki_import_pubkey_file(String_val(filenamev), &key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_pubkey_file failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_pki_import_cert_base64 : bytes -> ssh_keytype -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_cert_base64(value datav, value typev)
{
  CAMLparam2(datav, typev);
  int rc;
  ssh_key key = NULL;

  rc = ssh_pki_import_cert_base64((const char *)Bytes_val(datav), Int_val(typev), &key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_cert_base64 failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_pki_import_cert_file : string -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_cert_file(value filenamev)
{
  CAMLparam1(filenamev);
  int rc;
  ssh_key key = NULL;

  rc = ssh_pki_import_cert_file(String_val(filenamev), &key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_cert_file failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_key_cmp : ssh_keycmp -> key_t -> key_t -> int */
CAMLprim value ocaml_libssh_ssh_key_cmp(value cmpv, value keyav, value keybv)
{
  CAMLparam3(cmpv, keyav, keybv);
  ssh_key keya = Ssh_key_val(keyav);
  ssh_key keyb = Ssh_key_val(keybv);

  CAMLreturn(Val_int(ssh_key_cmp(keya, keyb, Int_val(cmpv))));
}

/* val ssh_pki_import_privkey_file : ?auth:ssh_auth -> string -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_privkey_file(value authv, value filenamev)
{
  CAMLparam2(authv, filenamev);
  CAMLlocal1(fv);
  struct ocaml_auth_data auth;
  int rc;
  ssh_key key = NULL;

  callback_unpack(authv, &auth);
  rc = ssh_pki_import_privkey_file(String_val(filenamev), auth.passphrase, auth.cb, &auth.cb_data, &key);
  callback_free(&auth);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_privkey_file failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_pki_import_privkey_base64 : ?auth:ssh_auth -> bytes -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_import_privkey_base64(value authv, value datav)
{
  CAMLparam2(authv, datav);
  CAMLlocal1(fv);
  struct ocaml_auth_data auth;
  int rc;
  ssh_key key = NULL;

  callback_unpack(authv, &auth);
  rc = ssh_pki_import_privkey_base64((const char *)Bytes_val(datav), auth.passphrase, auth.cb, &auth.cb_data, &key);
  callback_free(&auth);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_import_privkey_base64 failed");
  }

  CAMLreturn(Val_ssh_key(key));
}

/* val ssh_key_type : key_t -> ssh_keytype */
CAMLprim value ocaml_libssh_ssh_key_type(value keyv)
{
  CAMLparam1(keyv);
  CAMLlocal1(rv);
  ssh_key key = Ssh_key_val(keyv);
  int type;

  type = ssh_key_type(key);
  if (type >= SSH_KEYTYPE_UNKNOWN && type <= SSH_KEYTYPE_RSA_CERT01) {
    rv = Val_int(type);
  } else {
    rv = caml_alloc(1, 0);
    Store_field(rv, 0, caml_copy_string(ssh_key_type_to_char(type)));
  }

  CAMLreturn(rv);
}

/* val ssh_pki_export_privkey_file : key_t -> ?auth:ssh_auth -> string -> unit */
CAMLprim value ocaml_libssh_ssh_pki_export_privkey_file(value keyv, value authv, value filenamev)
{
  CAMLparam3(keyv, authv, filenamev);
  CAMLlocal1(fv);
  ssh_key key = Ssh_key_val(keyv);
  struct ocaml_auth_data auth;
  int rc;

  callback_unpack(authv, &auth);
  rc = ssh_pki_export_privkey_file(key, auth.passphrase, auth.cb, &auth.cb_data, String_val(filenamev));
  callback_free(&auth);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_export_privkey_file failed");
  }

  CAMLreturn(Val_unit);
}

/* val ssh_pki_export_privkey_base64 : key_t -> ?auth:ssh_auth -> unit -> bytes */
CAMLprim value ocaml_libssh_ssh_pki_export_privkey_base64(value keyv, value authv, value unitv)
{
  CAMLparam3(keyv, authv, unitv);
  CAMLlocal2(rv, fv);
  ssh_key key = Ssh_key_val(keyv);
  struct ocaml_auth_data auth;
  int rc, len;
  char *base64 = NULL;

  callback_unpack(authv, &auth);
  rc = ssh_pki_export_privkey_base64(key, auth.passphrase, auth.cb, &auth.cb_data, &base64);
  callback_free(&auth);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_export_privkey_base64 failed");
  }

  len = strlen(base64);
  rv = caml_alloc_string(len);
  memcpy(String_val(rv), base64, len);
  free(base64);

  CAMLreturn(rv);
}

/* val ssh_get_publickey_hash : key_t -> ssh_publickey_hash -> bytes */
CAMLprim value ocaml_libssh_ssh_get_publickey_hash(value keyv, value hashtypev)
{
  CAMLparam2(keyv, hashtypev);
  CAMLlocal1(rv);
  ssh_key key = Ssh_key_val(keyv);
  int rc;
  unsigned char *hash = NULL;
  size_t hlen = 0;

  rc = ssh_get_publickey_hash(key, Int_val(hashtypev), &hash, &hlen);
  if (rc != SSH_OK) {
    caml_failwith("ssh_get_publickey_hash failed");
  }

  rv = caml_alloc_string(hlen);
  memcpy(String_val(rv), hash, hlen);
  ssh_clean_pubkey_hash(&hash);

  CAMLreturn(rv);
}

/* val ssh_key_is_public : key_t -> bool */
CAMLprim value ocaml_libssh_ssh_key_is_public(value keyv)
{
  CAMLparam1(keyv);
  ssh_key key = Ssh_key_val(keyv);

  CAMLreturn(Val_bool(ssh_key_is_public(key)));
}

/* val ssh_key_is_private : key_t -> bool */
CAMLprim value ocaml_libssh_ssh_key_is_private(value keyv)
{
  CAMLparam1(keyv);
  ssh_key key = Ssh_key_val(keyv);

  CAMLreturn(Val_bool(ssh_key_is_private(key)));
}

/* val ssh_pki_key_ecdsa_name : key_t -> string option */
CAMLprim value ocaml_libssh_ssh_pki_key_ecdsa_name(value keyv)
{
  CAMLparam1(keyv);
  ssh_key key = Ssh_key_val(keyv);

  CAMLreturn(Val_optstring(ssh_pki_key_ecdsa_name(key)));
}

/* val ssh_pki_export_privkey_to_pubkey : key_t -> key_t */
CAMLprim value ocaml_libssh_ssh_pki_export_privkey_to_pubkey(value keyv)
{
  CAMLparam1(keyv);
  ssh_key key = Ssh_key_val(keyv);
  int rc;
  ssh_key new_key = NULL;

  rc = ssh_pki_export_privkey_to_pubkey(key, &new_key);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_export_privkey_to_pubkey failed");
  }

  CAMLreturn(Val_ssh_key(new_key));
}

/* val ssh_pki_export_pubkey_base64 : key_t -> bytes */
CAMLprim value ocaml_libssh_ssh_pki_export_pubkey_base64(value keyv)
{
  CAMLparam1(keyv);
  CAMLlocal1(rv);
  ssh_key key = Ssh_key_val(keyv);
  int rc;
  char *data = NULL;

  rc = ssh_pki_export_pubkey_base64(key, &data);
  if (rc != SSH_OK) {
    caml_failwith("ssh_pki_export_pubkey_base64 failed");
  }

  rv = caml_copy_string(data);
  ssh_string_free_char(data);

  CAMLreturn(rv);
}
