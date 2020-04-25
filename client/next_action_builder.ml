open Todd_common
open Core
open Lwt.Infix

let get_project term = 
  Db.dump_projects ()
  >>= fun projects ->
  (new Ui.read_line_const_prompt_options
    ~term ~prompt:"Project (empty line for none): "
    ~options:(Set.to_list projects))
  #run_utf8_conv
>|= fun project -> 
match project with "" -> None | s -> Some (s |> String.strip)


let create term =
  get_project term 
  >>= fun project ->
  (new Ui.read_line_const_prompt ~term ~prompt:"Text: ")#run_utf8_conv
  >|= fun text -> Next_action.create ~text ~project
