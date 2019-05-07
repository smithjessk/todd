open Core

let () =
  Command.group ~summary:"No comment :)"
    [ ("dump", Dump.cmd)
    ; ("collect", Collect.cmd)
    ; ("insert", Insert.cmd)
    ; ("review", Review.cmd) ]
  |> Command.run
