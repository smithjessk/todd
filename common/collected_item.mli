type t [@@deriving sexp]

val t : t Caqti_type.t

val text : t -> string

val create : string -> Ptime.t -> t
