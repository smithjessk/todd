open Core
open Lwt.Infix

let get_text ?batched_text () =
  match batched_text with
  | Some s ->
      Lwt.return s
  | None ->
      Lazy.force LTerm.stdout
      >>= fun term ->
      (new Ui.read_line_const_prompt ~term ~prompt:"Text: ")#run_utf8_conv

let insert_waiting_cmd =
  let f () = get_text () >>= Db.insert_waiting_for in
  Command.basic ~summary:"Insert things into waiting for list"
    (Command.Param.return (Fn.compose Lwt_main.run f))

let insert_maybe_cmd =
  let f () = get_text () >>= Db.insert_maybe in
  Command.basic ~summary:"Insert things into maybe list"
    (Command.Param.return (Fn.compose Lwt_main.run f))

let insert_someday_cmd =
  let f () = get_text () >>= Db.insert_someday in
  Command.basic ~summary:"Insert things into somday list"
    (Command.Param.return (Fn.compose Lwt_main.run f))

let insert_action_cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"Insert action into a project"
    [%map_open
      let project = flag "project" (optional string) ~doc:" Project" in
      let f () =
        get_text ()
        >>= fun s ->
        Db.insert_action ?project s >|= fun () -> print_endline "Inserted"
      in
      Fn.compose Lwt_main.run f]

let cmd =
  Command.group ~summary:"Directly insert things into lists"
    [ ("waiting-for", insert_waiting_cmd)
    ; ("maybe", insert_maybe_cmd)
    ; ("someday", insert_someday_cmd)
    ; ("action", insert_action_cmd) ]
