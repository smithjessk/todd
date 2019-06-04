open Core

(* perhaps this should have been refactored to just be: t m, t s, with different
flags and such based on the things you want to do.

i don't see that as a problem since reviewing, inserting, and dumping will always
   be available for each list. system-1 says this could actually really reduce
   coupling.

*)
let () =
  Command.group ~summary:"No comment :)"
    [ ("dump", Dump.cmd)
    ; ("collect", Insert.insert_collect_cmd) (* i frequently type this *)
    ; ("insert", Insert.cmd)
    ; ("review", Review.cmd) ]
  |> Command.run
