open Core

let () =
  Command.group ~summary:"The world's best Ocaml GTD implementation"
    [ ("dump", Dump.cmd)
    ; ("collect", Collect.cmd)
    ; ("insert", Insert.cmd)
    ; ("review", Review.cmd) ]
  |> Command.run
