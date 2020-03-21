open Core
open Lwt.Infix

let connect_and_collect item =
  Db.collect item >|= fun () -> print_endline "Collected"

let cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"collect"
    [%map_open
      let mode = flag "batch" (optional string) ~doc:" Don't read from STDIN" in
      fun () ->
        Lwt_main.run
          ( ( match mode with
            | Some s -> Lwt.return s
            | None ->
                Lazy.force LTerm.stdout >>= fun term ->
                (new Ui.read_line_const_prompt ~term ~prompt:"Text: ")
                  #run_utf8_conv )
          >>= connect_and_collect )]
