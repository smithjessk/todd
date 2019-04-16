open Lwt.Infix
open Core
open Todd_common

let uri = "postgresql://localhost:5432" |> Uri.of_string

let conn_then_exec_exn f = Caqti_lwt.connect uri >>= Caqti_lwt.or_fail >>= f

(** Insert some text into a specified table's 'text' column. *)
let insert_text_eponymous ~table text =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let s = sprintf "INSERT INTO %s (text) VALUES((?))" table in
      let req = Caqti_request.exec Caqti_type.string s in
      Db.exec req text >>= Caqti_lwt.or_fail )

let insert_waiting_for = insert_text_eponymous ~table:"waiting_for"

let insert_maybe = insert_text_eponymous ~table:"maybe"

let insert_someday = insert_text_eponymous ~table:"someday"

(** Delete some text from a specified table's 'text' column. *)
let delete_text_eponymous ~table text =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let s = sprintf "DELETE FROM %s WHERE text=(?)" table in
      let req = Caqti_request.exec Caqti_type.string s in
      Db.exec req text >>= Caqti_lwt.or_fail )

let delete_someday = delete_text_eponymous ~table:"someday"

let delete_maybe = delete_text_eponymous ~table:"maybe"

let delete_waiting_for = delete_text_eponymous ~table:"waiting_for"

let delete_collected_item = delete_text_eponymous ~table:"collected_item"

let insert_action ?(project = "") text =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.exec
          Caqti_type.(tup2 string string)
          "INSERT INTO next_action (text, project) VALUES ((?), (?))"
      in
      Db.exec req (text, project) >>= Caqti_lwt.or_fail )

let find_actions ?(project_substring = "") ?(text_substring = "") () =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.collect Caqti_type.unit
          Caqti_type.(tup2 string string)
          "SELECT project, text FROM next_action ORDER BY created_at"
      in
      let open Todd_common in
      Db.collect_list req () >>= Caqti_lwt.or_fail
      >|= List.filter ~f:(fun (p, t) ->
              let f s ~sub =
                String.is_substring (s |> String.lowercase)
                  ~substring:(sub |> String.lowercase)
              in
              f p ~sub:project_substring && f t ~sub:text_substring )
      >|= List.map ~f:(fun (project, text) ->
              let project = match project with "" -> None | s -> Some s in
              Next_action.create ~project ~text ) )

let delete_action_with_text text =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.start () >>= Caqti_lwt.or_fail
      >>= fun () ->
      let req =
        Caqti_request.exec Caqti_type.string
          "DELETE FROM next_action WHERE text=(?)"
      in
      Db.exec req text >>= Caqti_lwt.or_fail >>= Db.commit
      >>= Caqti_lwt.or_fail )

let collect s =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.exec Caqti_type.string
          "INSERT INTO collected_item (text) VALUES (?)"
      in
      Db.exec req s >>= Caqti_lwt.or_fail )

let dump_collected () =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.collect Caqti_type.unit Caqti_type.string
          "SELECT * from collected_item ORDER BY created_at"
      in
      Db.collect_list req () >>= Caqti_lwt.or_fail
      >|= List.map ~f:(fun t -> Todd_common.Collected_item.create t Ptime.epoch)
  )

let dump_distinct_strings_from_table ~field ~table =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      let req =
        Caqti_request.collect Caqti_type.unit Caqti_type.string
          (sprintf "SELECT DISTINCT %s FROM %s" field table)
      in
      Db.collect_list req () >>= Caqti_lwt.or_fail >|= String.Set.of_list )

let dump_projects () =
  dump_distinct_strings_from_table ~field:"project" ~table:"next_action"

let dump_maybe () =
  dump_distinct_strings_from_table ~field:"text" ~table:"maybe"

let dump_someday () =
  dump_distinct_strings_from_table ~field:"text" ~table:"someday"

let dump_maybe_and_someday () =
  dump_maybe () >>= fun m -> dump_someday () >|= fun s -> String.Set.union m s

let dump_waiting_for () =
  dump_distinct_strings_from_table ~field:"text" ~table:"waiting_for"

let delete_collected_item_already_connected (module Db : Caqti_lwt.CONNECTION)
    item =
  let req =
    Caqti_request.exec Caqti_type.string
      "DELETE FROM collected_item WHERE text=(?)"
  in
  Db.exec req (item |> Collected_item.text)

let upload_next_action (module Db : Caqti_lwt.CONNECTION) a =
  ( match a.Next_action.project with
  | None ->
      let req =
        Caqti_request.exec Caqti_type.string
          "INSERT INTO next_action (text) VALUES ((?))"
      in
      Db.exec req a.Next_action.text
  | Some _ ->
      let req =
        Caqti_request.exec
          Caqti_type.(tup2 string string)
          "INSERT INTO next_action (text, project) VALUES ((?), (?))"
      in
      Db.exec req (a.text, a.project |> Option.value_exn) )
  >>= Caqti_lwt.or_fail

let define ?item na =
  conn_then_exec_exn (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.start () >>= Caqti_lwt.or_fail
      >>= fun () ->
      let db = (module Db : Caqti_lwt.CONNECTION) in
      upload_next_action db na
      >>= fun () ->
      ( match item with
      | None -> Lwt.return ()
      | Some item ->
          delete_collected_item_already_connected db item >>= Caqti_lwt.or_fail
      )
      >>= fun () -> Db.commit () >>= Caqti_lwt.or_fail )
