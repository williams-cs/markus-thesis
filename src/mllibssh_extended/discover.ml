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

module C = Configurator.V1

let discover c =
  let package = "libssh" in
  let minversion = "0.8" in
  let expr = package ^ " >= " ^ minversion in
  let pkgconfig =
    match C.Pkg_config.get c with
    | None -> C.die "pkg-config not available"
    | Some pc -> pc in
  let conf =
    match C.Pkg_config.query pkgconfig ~package:expr with
    | None -> C.die "%s not found" package
    | Some deps -> deps in
  C.Flags.write_sexp "c_flags.sexp" conf.cflags;
  C.Flags.write_sexp "c_library_flags.sexp" conf.libs

let () =
  C.main ~name:"discover" discover
