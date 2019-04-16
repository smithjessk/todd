open Cohttp_lwt_unix
open Core_kernel
open Lwt.Infix

let phone_number = Sys.getenv "PHONE_NUMBER"

let port = try Sys.getenv "PORT" |> Int.of_string with _ -> 3000

let db_uri = Sys.getenv "CAQTI_FRIENDLY_DB_URI" |> Uri.of_string

let add_to_collect_box item =
  Caqti_lwt.connect db_uri >>= Caqti_lwt.or_fail
  >>= fun (module Db : Caqti_lwt.CONNECTION) ->
  let req =
    Caqti_request.exec Caqti_type.string
      "INSERT INTO collected_item (text) VALUES (?)"
  in
  Db.exec req item >>= Caqti_lwt.or_fail

let collect _ req_body =
  let%lwt req_body = Cohttp_lwt.Body.to_string req_body in
  print_endline req_body ;
  let qs = Uri.query_of_encoded req_body in
  List.iter qs ~f:(fun (a, l) ->
      print_endline a ;
      List.iter l ~f:print_endline ) ;
  let m = String.Map.of_alist_exn qs in
  match Map.find_exn m "From" with
  | [hd] when hd = phone_number ->
      let items = Map.find_exn m "Body" in
      assert (List.length items = 1) ;
      let item = List.hd_exn items in
      add_to_collect_box item
      >>= fun () ->
      let body =
        sprintf
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
           <Response>\n    \
           <Message>\n        \
           Collected %s\n    \
           </Message>\n\
           </Response>"
          item
      in
      let headers =
        Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "text/xml"
      in
      Server.respond_string ~headers ~status:`OK ~body ()
  | _ ->
      let body = "Invalid" in
      Server.respond_string ~status:`Bad_request ~body ()

let callback _conn req req_body =
  match Uri.path (Request.uri req) with
  | "/collect" -> collect req req_body
  | _ -> Server.respond_not_found ()

let main () =
  let mode = `TCP (`Port port) in
  Lwt_main.run @@ Server.create ~mode @@ Server.make ~callback ()

let () = main ()
