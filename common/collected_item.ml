open Core

type t = {
text: string
; created_at: Core.Time.t
} [@@deriving sexp, fields]

let create text created_at =
    let (date, time) = Ptime.to_date_time created_at in
    let (y, m, d) = date in
    let date = Date.create_exn ~y ~m:(Month.of_int_exn m) ~d in
    let ((hr, min, sec), _) = time in
    let ofday = Time.Ofday.create ~hr ~min ~sec () in
    let created_at = Time.of_date_ofday ~zone:Time.Zone.utc date ofday in
    {text; created_at}

(* does shenangigans to convert back and forth between a [Ptime.t] and a
[Core.Time.t]. sorry. but [Ptime.t] is not sexpable *)
let t =
  let encode {text; created_at} =
    let (date, ofday) = Time.to_date_ofday created_at ~zone:Time.Zone.utc in
    let parts = Time.Ofday.to_parts ofday in
    let date = (Core.Date.year date, Core.Date.month date |> Core.Month.to_int, Core.Date.day date) in
    let time = ((parts.hr, parts.min, parts.sec), 0) in
    match Ptime.of_date_time (date, time) with
    | Some p -> Ok(text, p)
    | None -> Result.fail "No valid time" in
  let decode (text, created_at) = 
    Ok (create text created_at) in
  let rep = Caqti_type.(tup2 string ptime) in
  Caqti_type.custom ~encode ~decode rep
