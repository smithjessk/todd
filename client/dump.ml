open Core
open Lwt.Infix
open Todd_common

let dump_collect_box () =
  Db.dump_collected ()
  >|= function
  | [] -> print_endline "Empty"
  | list ->
      List.iteri list ~f:(fun idx item ->
          printf "[%d]: %s\n" (idx + 1) (Collected_item.text item) )

let dump_collect_cmd =
  Command.basic ~summary:"dump collect box"
    (Command.Param.return (fun () -> Lwt_main.run (dump_collect_box ())))

let param f =
  let f () =
    Lwt_main.run
      ( f () >|= Set.to_list
      >|= List.iteri ~f:(fun i x -> printf "[%d]: %s\n" i x) )
  in
  Command.Param.return f

let dump_projects_cmd =
  Command.basic ~summary:"dump projects" (param Db.dump_projects)

let dump_waiting_for_cmd =
  Command.basic ~summary:"dump waiting for" (param Db.dump_waiting_for)

let dump_maybe_cmd = Command.basic ~summary:"dump maybe" (param Db.dump_maybe)

let dump_someday_cmd =
  Command.basic ~summary:"dump someday" (param Db.dump_someday)

let dump_actions_cmd =
  let rec print_actions (actions : Next_action.t list) n =
    match actions with
    | [] -> Lwt.return ()
    | hd :: l ->
        let open LTerm_text in
        let opener = sprintf "\t[%d]: " n in
        let body = sprintf "%s" hd.text in
        let msg = eval [B_bold true; S opener; E_bold; S body] in
        LTerm.printls msg >>= fun () -> print_actions l (n + 1)
  in
  let open Command.Let_syntax in
  Command.basic ~summary:"View actions"
    [%map_open
      let project =
        flag "project" ~aliases:["p"] (optional string)
          ~doc:" case-insensitive substring of project name"
      and text =
        flag "text" ~aliases:["t"] (optional string)
          ~doc:" text. Case insensitive substring search"
      in
      fun () ->
        Lwt_main.run
          ( Db.find_actions ?project_substring:project ?text_substring:text ()
          >|= List.fold ~init:String.Map.empty ~f:(fun map x ->
                  let key =
                    match x.Next_action.project with None -> "" | Some s -> s
                  in
                  Map.update map key ~f:(function
                    | None -> [x]
                    | Some l -> x :: l ) )
          >>= Map.fold ~init:(Lwt.return ())
                ~f:(fun ~key:project ~data:l accum ->
                  accum
                  >>= fun () ->
                  let open LTerm_text in
                  eval [B_bold true; S (sprintf "[%s]:" project); E_bold]
                  |> LTerm.printls
                  >>= fun () -> print_actions l 0 ) )]

let cmd =
  Command.group ~summary:"Different ways to dump stuff"
    [ ("collect", dump_collect_cmd)
    ; ("projects", dump_projects_cmd)
    ; ("actions", dump_actions_cmd)
    ; ("waiting-for", dump_waiting_for_cmd)
    ; ("maybe", dump_maybe_cmd)
    ; ("someday", dump_someday_cmd) ]
