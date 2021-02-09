open Async

module Header = struct
  type t =
    { program : Sexp.t
    ; env_image : Env.Image.t
    }
  [@@deriving sexp, bin_io, fields]
end
