val to_clipboard : unescaped:string -> Lwt_unix.process_status Lwt.t

val open_link : unescaped:string -> Lwt_unix.process_status Lwt.t

val web_search : unescaped:string -> Lwt_unix.process_status Lwt.t
