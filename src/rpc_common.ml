open Core
open Async

module Header = struct
  type t =
    { program : Sexp.t
    ; env_image : Env.Image.t
    }
  [@@deriving sexp, bin_io, fields]
end

module Sender_data = struct
  type t =
    | Header of Header.t
    | Message of bytes
    | Close
  [@@deriving sexp, bin_io]
end

module Sender_query = struct
  type t =
    { id : string
    ; data : Sender_data.t
    }
  [@@deriving sexp, bin_io, fields]
end

module Receiver_data = struct
  type t =
    { id : string
    ; data : string
    }
  [@@deriving sexp, bin_io, fields]
end

module Receiver_query = struct
  type t =
    | Data of Receiver_data.t
    | Close of string
  [@@deriving sexp, bin_io]
end
