open Core
open Lwt.Infix
open Ui

module Reviewable = struct
  type t = {item: string; prompt: string option}
end

type list_type = Collect | Maybe | Someday | Waiting | Action

(** produces an offset that's used to find the next item, if any *)
let handle_item term ~position_prompt reviewing item =
  let {Reviewable.prompt; item} = item in
  LTerm.clear_screen term
  >>= fun () ->
  let prompt = match prompt with None -> "" | Some s -> sprintf "%s: " s in
  let open LTerm_text in
  LTerm.printls
    (eval [B_bold true; S (sprintf "[%s%s]: %s" prompt position_prompt item)])
  >>= fun () ->
  let rec loop () =
    (new Ui.read_line_const_prompt_options
       ~term ~prompt:"Action: "
       ~options:
         ( Map.keys Tui_action_for_processed_item.bindings
         |> List.map ~f:Char.to_string ))
      #run_utf8_conv
    >>= fun s ->
    try Tui_action_for_processed_item.with_prefix_exn s |> Lwt.return
    with _ -> loop ()
  in
  let delete =
    match reviewing with
    | Someday ->
        Db.delete_someday
    | Action ->
        Db.delete_action_with_text
    | Maybe ->
        Db.delete_maybe
    | Waiting ->
        Db.delete_waiting_for
    | Collect ->
        Db.delete_collected_item
  in
  loop ()
  >>= fun x ->
  let del_or_no_op inserted_to () =
    match inserted_to with
    | x when x = reviewing ->
        Lwt.return 1
    | _ ->
        delete item >>= fun () -> Lwt.return 1
  in
  match x with
  | _, Tui_action_for_processed_item.Move_to_maybe ->
      let item = sprintf "%s%s" prompt item in
      Db.insert_maybe item >>= del_or_no_op Maybe
  | _, Move_to_someday ->
      let item = sprintf "%s%s" prompt item in
      Db.insert_someday item >>= del_or_no_op Someday
  | _, Move_to_actions ->
      Next_action_builder.create term
      >>= fun na -> Db.define na >>= del_or_no_op Action
  | _, Delete ->
      delete item >|= fun () -> 1
  | _, Copy_to_clipboard ->
      Utils.copy ~unescaped:item
      >>= fun _ -> LTerm.printl "Copied" >|= fun () -> 0
  | cnt, Jump_to_next ->
      Lwt.return cnt
  | cnt, Jump_to_previous ->
      Lwt.return (Int.neg cnt)
  | _, Open_link ->
      Utils.open_link ~unescaped:item
      >>= fun _ -> LTerm.printl "Opened" >|= fun () -> 0
  | _, Search ->
      Utils.search ~unescaped:item
      >>= fun _ -> LTerm.printl "Searched" >|= fun () -> 0

let handle_items ~term all ~reviewing =
  let rec loop idx =
    match List.nth all idx with
    | None ->
        Lwt.return ()
    | Some e ->
        handle_item term reviewing e
          ~position_prompt:(sprintf "%d/%d" idx (List.length all - 1))
        >>= fun offset -> loop (idx + offset)
  in
  loop 0

module Ordering : sig
  type t

  val act : t option -> 'a list -> 'a list

  val arg_type : t Command.Arg_type.t
end = struct
  type t = Shuffle [@@deriving of_sexp]

  module Shuffle : sig
    val sort : 'a list -> 'a list
  end = struct
    type 'a t = {rand: int; elem: 'a}

    let sort l =
      Random.self_init () ;
      let l : 'a t list =
        List.map
          ~f:(fun x ->
            let rand = Random.int Int.max_value in
            {rand; elem= x} )
          l
      in
      let l = List.sort ~compare:(fun x y -> Int.compare x.rand y.rand) l in
      List.map ~f:(fun x -> x.elem) l
  end

  let act o l = match o with Some Shuffle -> Shuffle.sort l | None -> l

  let arg_type = Command.Arg_type.create (Fn.compose t_of_sexp Sexp.of_string)
end

let opt_ordering_param =
  let open Command in
  Param.flag "ordering"
    (Flag.optional Ordering.arg_type)
    ~doc:" change item order"

let text_search_param =
  let open Command in
  Param.flag "text" ~aliases:["t"]
    (Flag.optional Param.string)
    ~doc:" text. Case insensitive substring search"

let review_cmd cmd_name ~dump ~reviewing =
  let f o t () =
    Lazy.force LTerm.stdout
    >>= fun term ->
    dump () >|= Ordering.act o
    >|= List.filter ~f:(fun i ->
            match t with
            | None ->
                true
            | Some substring ->
                let f = String.lowercase in
                let a = f i.Reviewable.item in
                let substring = f substring in
                String.is_substring a ~substring )
    >>= fun items -> handle_items ~term items ~reviewing
  in
  let open Command.Let_syntax in
  ( cmd_name
  , Command.basic
      ~summary:(sprintf "%s review" cmd_name)
      [%map_open
        let ordering = opt_ordering_param and text = text_search_param in
        Fn.compose Lwt_main.run (f ordering text)] )

(** would be nice if this went through [review_cmd] so that I didn't have
to duplicate code to add the ordering argument. there's a todo about this.
i think to solve this I have to better understand [map_open]. *)
let action_cmd =
  let open Command.Let_syntax in
  Command.basic ~summary:"review actions"
    [%map_open
      let project =
        flag "project" ~aliases:["p"] (optional string)
          ~doc:" project name. Case insensitive substring search"
      and no_project =
        flag "no-project" ~aliases:["np"] no_arg
          ~doc:" only print actions with attached to no project."
      and text =
        flag "text" ~aliases:["t"] (optional string)
          ~doc:" text. Case insensitive substring search"
      and ordering = opt_ordering_param in
      let dump () =
        Db.find_actions ?project_substring:project ?text_substring:text ()
        >|= List.filter ~f:(fun na ->
                if no_project then
                  Option.is_none na.Todd_common.Next_action.project
                else true )
        >|= List.map ~f:(fun (action : Todd_common.Next_action.t) ->
                {Reviewable.item= action.text; prompt= action.project} )
        >|= Ordering.act ordering
      in
      let f () =
        Lazy.force LTerm.stdout
        >>= fun term -> dump () >>= handle_items ~term ~reviewing:Action
      in
      Fn.compose Lwt_main.run f]

let cmd =
  let no_prompt s =
    s
    >|= Fn.compose
          (List.map ~f:(fun x -> {Reviewable.item= x; prompt= None}))
          String.Set.to_list
  in
  Command.group ~summary:"Different ways to review"
    [ review_cmd "someday"
        ~dump:(Fn.compose no_prompt Db.dump_someday)
        ~reviewing:Someday
    ; review_cmd "maybe"
        ~dump:(Fn.compose no_prompt Db.dump_maybe)
        ~reviewing:Maybe
    ; ("actions", action_cmd)
    ; review_cmd "waiting"
        ~dump:(Fn.compose no_prompt Db.dump_waiting_for)
        ~reviewing:Waiting
    ; review_cmd "collect"
        ~dump:
          (Fn.compose
             (fun l ->
               let open Todd_common.Collected_item in
               l
               >|= List.map ~f:(fun t -> {Reviewable.item= text t; prompt= None}
                   ) )
             Db.dump_collected)
        ~reviewing:Collect ]
