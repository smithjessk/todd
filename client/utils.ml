open Core

(** These utils are *begging* to be designed better. So much code reuse
here it makes my eyes bleed *)

let copy ~unescaped =
  let text = unescaped |> String.escaped in
  sprintf "echo -n \"%s\" | xclip -i -selection clipboard" text
  |> Lwt_unix.system

let open_link ~unescaped =
  let text = unescaped |> String.escaped in
  Lwt_unix.system (sprintf "firefox \"%s\" &" text)

let search ~unescaped =
  let text = unescaped |> String.escaped in
  let url = sprintf "https://duckduckgo.com/?q=%s" text in
  Lwt_unix.system (sprintf "firefox \"%s\" &" url)
