open Core
open Lwt.Infix

let uri = "postgresql://localhost:5432" |> Uri.of_string

let conn_then_exec_exn f = Caqti_lwt.connect uri >>= Caqti_lwt.or_fail >>= f

let random_from_jar_exn () =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.collect Caqti_type.unit Caqti_type.string
          "SELECT text FROM jar_of_awesome ORDER BY RANDOM() LIMIT 1"
      in
      Db.collect_list req () >>= Caqti_lwt.or_fail >|= List.hd_exn )

let collect s =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let s = sprintf "Brought to you by the Jar of Awesome: %s" s in
      let req =
        Caqti_request.exec Caqti_type.string
          "INSERT INTO collected_item (text) VALUES (?)"
      in
      Db.exec req s >>= Caqti_lwt.or_fail )

let () =
  let f () = random_from_jar_exn () >>= collect in
  let f = Fn.compose Lwt_main.run f in
  Command.run
    (Command.basic ~summary:"Collect item from jar of awesome"
       (Command.Param.return f))
