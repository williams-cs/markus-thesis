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

#ifndef LIBSSH_OCAML_H
#define LIBSSH_OCAML_H

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>

#include <libssh/libssh.h>
#include <libssh/sftp.h>

#include <stdbool.h>

/* Make sure to not expose our internal helpers as public symbols.
 * https://gcc.gnu.org/wiki/Visibility
 */
#ifdef __GNUC__
#pragma GCC visibility push(hidden)
#endif

#define MIN(a, b) (((a) < (b)) ? (a) : (b))

const char *Optstring_val(value strv);
bool Optbool_val(value boolv, bool defval);
int Optint_val(value intv, int defval);

/* Wrap and unwrap ssh_session handles, with a finalizer. */
#define Ssh_session_val(rv) (*(ssh_session *)Data_custom_val(rv))

#define _sftp_session_val(rv) (*((sftp_session *)Data_custom_val(rv)))
#define Sftp_session_val(rv) _sftp_session_val(Field((rv),0))
#define Ssh_session_sftp_val(rv) Ssh_session_val(Field((rv),1))

#define _sftp_file_val(rv) (*((sftp_file *)Data_custom_val(rv)))
#define Sftp_file_val(rv) _sftp_file_val(Field((rv),0))
#define Sftp_session_file_val(rv) Sftp_session_val(Field((rv),1))

#define _sftp_dir_val(rv) (*((sftp_dir *)Data_custom_val(rv)))
#define Sftp_dir_val(rv) _sftp_dir_val(Field((rv),0))
#define Sftp_session_dir_val(rv) Sftp_session_val(Field((rv),1))

#define _ssh_channel_val(rv) (*((ssh_channel *)Data_custom_val(rv)))
#define Ssh_channel_val(rv) _ssh_channel_val(Field((rv),0))
#define Ssh_session_channel_val(rv) Ssh_session_val(Field((rv),1))

#define Ssh_key_val(rv) (*(ssh_key *)Data_custom_val(rv))

#define _ssh_scp_val(rv) (*((ssh_scp *)Data_custom_val(rv)))
#define Ssh_scp_val(rv) _ssh_scp_val(Field((rv),0))
#define Ssh_session_scp_val(rv) Ssh_session_val(Field((rv),1))

/* Raise an Libssh.LibsshError exception. */
void raise_libssh_error(ssh_session session);

void check_ssh_error_return_code(ssh_session session, int code);

#define Val_int64(val) caml_copy_int64(val)
value Val_optstring(const char *str);
value Val_optstring_notempty(const char *str);
value Val_ssh_string(const ssh_string str);
value Val_ssh_error_type(int code);

value Val_ssh_session(ssh_session t);
value Val_sftp_session(value sshv, sftp_session sftp);
value Val_sftp_file(value sftpv, sftp_file file);
value Val_sftp_dir(value sftpv, sftp_dir dir);
value Val_ssh_channel(value sshv, ssh_channel chan);
value Val_ssh_key(ssh_key k);
value Val_ssh_scp(value sshv, ssh_scp scp);

#ifndef Bytes_val /* OCaml < 4.06.0 */
#define Bytes_val(x) ((unsigned char *) Bp_val(x))
#endif

#ifdef __GNUC__
#pragma GCC visibility pop
#endif

#endif
