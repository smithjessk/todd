open Core
open Lwt.Infix

let get_text batched_text =
  match batched_text with
  | Some s -> Lwt.return s
  | None ->
      Lazy.force LTerm.stdout >>= fun term ->
      (new Ui.read_line_const_prompt ~term ~prompt:"Text: ")#run_utf8_conv

let make insert s =
  let open Command.Let_syntax in
  Command.basic ~summary:s
    [%map_open
      let batched_text =
        flag "batch" (optional string) ~doc:" Don't read from STDIN"
      in
      let f () = get_text batched_text >>= insert in
      fun () ->
        let loop () =
          let () = Lwt_main.run (f ()) in
          Utils.play_notification_sound ()
        in
        loop ()]

let insert_waiting_cmd =
  make Db.insert_waiting_for "Insert things into waiting for list"

let insert_maybe_cmd = make Db.insert_maybe "Insert into maybe"

let insert_someday_cmd = make Db.insert_someday "Insert things into somday list"

let insert_collect_cmd = make Db.collect "Insert into intray"

let insert_action_cmd =
  Command.basic ~summary:"insert an action into a project"
    [%map_open.Command
      let () = return () in
      (* jsmith: what if the project doesn't exist? should define it! *)
      fun () ->
        (let%lwt lterm = Lazy.force LTerm.stdout in
         let%lwt next_action = Next_action_builder.create lterm in
         Db.insert_action_and_upsert_normalized_project next_action)
        |> Lwt_main.run]

let insert_jar_of_awesome_cmd =
  make Db.insert_jar_of_awesome "Insert into the jar of awesome"

let insert_bucket_list_cmd =
  make Db.insert_bucket_list "Insert into your bucket list"

let insert_outcome_cmd =
  make (Db.insert_outcome ~type_:`Insert) "Insert into your outcomes list"

let cmd =
  Command.group ~summary:"Directly insert things into lists"
    [
      ("waiting-for", insert_waiting_cmd);
      ("maybe", insert_maybe_cmd);
      ("someday", insert_someday_cmd);
      ("action", insert_action_cmd);
      ("jar-of-awesome", insert_jar_of_awesome_cmd);
      ("bucket-list", insert_bucket_list_cmd);
      ("outcome", insert_outcome_cmd);
    ]
