open Async

module Header : sig
  type t =
    { program : Sexp.t
    ; env_image : Env.Image.t
    }
  [@@deriving sexp, bin_io, fields]
end

module Sender_data : sig
  type t =
    | Header of Header.t
    | Message of bytes
    | Close
  [@@deriving sexp, bin_io]
end

module Sender_query : sig
  type t =
    { id : string
    ; data : Sender_data.t
    }
  [@@deriving sexp, bin_io, fields]
end

module Receiver_data : sig
  type t =
    { id : string
    ; data : string
    }
  [@@deriving sexp, bin_io, fields]
end

module Receiver_query : sig
  type t =
    | Data of Receiver_data.t
    | Close of string
    | Heartbeat of string * int
  [@@deriving sexp, bin_io]
end
