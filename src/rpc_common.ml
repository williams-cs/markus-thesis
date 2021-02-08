open Async

module Header = struct
  type t = { program : Sexp.t } [@@deriving sexp, bin_io, fields]
end
