; libssh OCaml bindings
; Copyright (C) 2019 Pino Toscano <ptoscano@redhat.com>
;
; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

; common settings, which apparently need to be duplicated for both
; mllibssh and the examples
(env
  (dev
    (flags (:standard -g -w -27 -warn-error CDEFLMPSUVYZX-3 -safe-string -w -9)))
  (release
    (flags (:standard -g -w -27 -warn-error CDEFLMPSUVYZX-3 -safe-string -w -9 -O3))))
