open Core

type t = {text: string; project: string option} [@@deriving sexp]

let create ~text ~project = {text; project}
