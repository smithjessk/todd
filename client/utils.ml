open Core

(** Haven't tested b/c new computer doesn't have the right executables (yet)
*)

let escaped_cmd ~unescaped escaped_to_cmd =
  let text = unescaped |> String.escaped in
  let s = escaped_to_cmd text in
  Lwt_unix.system s

let copy =
  escaped_cmd (fun text ->
      sprintf "echo -n \"%s\" | xclip -i -selection clipboard" text )

let open_link = escaped_cmd (fun text -> sprintf "firefox \"%s\" &" text)

let search ~unescaped =
  open_link ~unescaped:(sprintf "https://duckduckgo.com/?q=%s" unescaped)
