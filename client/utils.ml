open Core

(** Should be refactored! But also these aren't totally widely compatible, so...
*)

let copy ~unescaped =
  let text = unescaped |> String.escaped in
  sprintf "printf \"%s\" | pbcopy" text |> Lwt_unix.system

let open_link ~unescaped =
  let text = unescaped |> String.escaped in
  Lwt_unix.system (sprintf "firefox \"%s\" &" text)

let search ~unescaped =
  let text = unescaped |> String.escaped in
  let url = sprintf "https://duckduckgo.com/?q=%s" text in
  Lwt_unix.system (sprintf "firefox \"%s\" &" url)

let play_notification_sound () =
  Lwt.async (fun () ->
      (* jsmith: only works on macs :/ *)
      let command = Lwt_process.shell "afplay ~/bin/notification.mp3" in
      let status_promise = Lwt_process.exec command in
      Lwt.map (fun _ -> ()) status_promise)
