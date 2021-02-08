open Async

module Header : sig
  type t = { program : Sexp.t } [@@deriving sexp, bin_io, fields]
end
