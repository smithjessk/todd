open Core
open Lwt.Infix

module Reviewable = struct
  type t = { item : string; prompt : string option }
end

(**


1. ways to use this api:

- [bindings], help, with_prefix_exn
- broadly, can be used to create help or to read in user actions
    (why is reading here..? i guess it is associated with this)

2. incorrect ways to use this API:

   - [with_prefix_exn] with a return at the end
   - people instantiating actions and then doing what with them? we want to pattern
   match on them in that site, not here
   - ignoring the bindings and passing in the sexps as strings
   - not catching the exn in with_prefix_exn
   - storing a binding and then getting it later

3. - exn
   - ???
   - exn
   - exn
   - no way; actions can be stored to disk and then used later in client code

4. [with_prefix_exn]: or_error type instead of exn
   - don't expose the variant type, or [sexp]ing them; move the actions inside
     an internal function that would read a string from stdin and then act on it
   (this would also remove [with_prefix_exn])
*)

module Review_action = struct
  type t =
    | Move_to_maybe
    | Move_to_someday
    | Move_to_actions
    | Delete
    | Copy_to_clipboard
    | Jump_to_next
    | Jump_to_previous
    | Open_link
    | Search
    | Quit
    | Help
  [@@deriving variants, sexp_of]

  let bindings =
    let to_string = function
      | Help -> "?"
      | Move_to_maybe -> "m"
      | Move_to_someday -> "s"
      | Move_to_actions -> "d"
      | Delete -> "x"
      | Copy_to_clipboard -> "y"
      | Jump_to_next -> "j"
      | Jump_to_previous -> "k"
      | Open_link -> "o"
      | Search -> "/"
      | Quit -> "qq"
    in
    let f t m _ = Map.add_exn m ~key:(to_string t) ~data:t in
    let m =
      Variants.fold ~init:String.Map.empty ~move_to_maybe:(f Move_to_maybe)
        ~move_to_someday:(f Move_to_someday)
        ~move_to_actions:(f Move_to_actions) ~delete:(f Delete) ~quit:(f Quit)
        ~copy_to_clipboard:(f Copy_to_clipboard) ~jump_to_next:(f Jump_to_next)
        ~jump_to_previous:(f Jump_to_previous) ~open_link:(f Open_link)
        ~search:(f Search) ~help:(f Help)
    in
    m

  (**

     preconditions:
     - [bindings] is a map from string keybindings to no-arg constructors for a type [t] that
     implements [sexp_of]
     - \n creates a newline
     - Core version bla bla bla
     - constructor names are helpful

     postcondition:
     - a string containing a mapping from the string keybinding to the tui action name

     modules which aren't directly referenced:
     - Unix/bash spec for \n creating a newline

  *)

  let help =
    bindings |> Map.to_alist
    |> List.map ~f:(fun (s, t) ->
           sprintf "%s %s" s (t |> sexp_of_t |> Sexp.to_string_hum))
    |> String.concat ~sep:"\n"

  let with_prefix_exn s =
    let cnt = String.take_while s ~f:Char.is_digit in
    let cmd = String.drop_prefix s (String.length cnt) in
    match Map.find bindings cmd with
    | None -> raise (Invalid_argument s)
    | Some t -> (
        match cnt with "" -> (1, t) | cnt -> (Int.of_string cnt, t) )
end

type list_type = Collect | Maybe | Someday | Waiting | Action
[@@deriving equal]

(** produces an offset that's used to find the next item, if any *)
let handle_item term ~position_prompt reviewing item =
  let { Reviewable.prompt; item } = item in
  LTerm.printl "" >>= fun () ->
  let prompt = match prompt with None -> "" | Some s -> sprintf "%s: " s in
  let open LTerm_text in
  LTerm.printls
    (eval [ B_bold true; S (sprintf "[%s%s]: %s" prompt position_prompt item) ])
  >>= fun () ->
  let rec loop () =
    (new Ui.read_line_const_prompt_options
       ~term ~prompt:"Action: "
       ~options:(Map.keys Review_action.bindings))
      #run_utf8_conv
    >>= fun s ->
    try Review_action.with_prefix_exn s |> Lwt.return with _ -> loop ()
  in
  let delete =
    match reviewing with
    | Someday -> Db.delete_someday
    | Action -> Db.delete_action_with_text
    | Maybe -> Db.delete_maybe
    | Waiting -> Db.delete_waiting_for
    | Collect -> Db.delete_collected_item
  in
  loop () >>= fun x ->
  let del_or_no_op inserted_to () =
    match inserted_to with
    | x when equal_list_type x reviewing ->
        Utils.play_notification_sound ();
        Lwt.return 1
    | _ ->
        delete item >>= fun () ->
        Utils.play_notification_sound ();
        Lwt.return 1
  in
  match x with
  | _, Review_action.Move_to_maybe ->
      let item = sprintf "%s%s" prompt item in
      Db.insert_maybe item >>= del_or_no_op Maybe
  | _, Move_to_someday ->
      let item = sprintf "%s%s" prompt item in
      Db.insert_someday item >>= del_or_no_op Someday
  | _, Move_to_actions ->
      Next_action_builder.create term >>= fun na ->
      Db.define na >>= (* jsmith: for multiple actions *)
                   fun () -> Lwt.return 0
  | _, Delete ->
      delete item >|= fun () ->
      Utils.play_notification_sound ();
      1
  | _, Copy_to_clipboard ->
      Utils.copy ~unescaped:item >>= fun _ ->
      LTerm.printl "Copied" >|= fun () -> 0
  | cnt, Jump_to_next -> Lwt.return cnt
  | cnt, Jump_to_previous -> Lwt.return (Int.neg cnt)
  | _, Open_link ->
      Utils.open_link ~unescaped:item >>= fun _ ->
      LTerm.printl "Opened" >|= fun () -> 0
  | _, Search ->
      Utils.search ~unescaped:item >>= fun _ ->
      LTerm.printl "Searched" >|= fun () -> 0
  | _, Quit -> exit 0
  | _, Help -> LTerm.printl Review_action.help >|= fun () -> 0

let handle_items ~term all ~reviewing =
  let rec loop idx =
    match List.nth all idx with
    | None -> Lwt.return ()
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
    type 'a t = { rand : int; elem : 'a }

    let sort l =
      Random.self_init ();
      let l : 'a t list =
        List.map
          ~f:(fun x ->
            let rand = Random.int Int.max_value in
            { rand; elem = x })
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
  Param.flag "text" ~aliases:[ "t" ]
    (Flag.optional Param.string)
    ~doc:" text. Case insensitive substring search"

let review_cmd cmd_name ~dump ~reviewing =
  let f o t () =
    Lazy.force LTerm.stdout >>= fun term ->
    dump () >|= Ordering.act o
    >|= List.filter ~f:(fun i ->
            match t with
            | None -> true
            | Some substring ->
                let f = String.lowercase in
                let a = f i.Reviewable.item in
                let substring = f substring in
                String.is_substring a ~substring)
    >>= fun items -> handle_items ~term items ~reviewing
  in
  let open Command.Let_syntax in
  ( cmd_name,
    Command.basic
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
        flag "project" ~aliases:[ "p" ] (optional string)
          ~doc:" project name. Case insensitive substring search"
      and no_project =
        flag "no-project" ~aliases:[ "np" ] no_arg
          ~doc:" only print actions with attached to no project."
      and text =
        flag "text" ~aliases:[ "t" ] (optional string)
          ~doc:" text. Case insensitive substring search"
      and ordering = opt_ordering_param in
      let dump () =
        Db.find_actions ?project_substring:project ?text_substring:text ()
        >|= List.filter ~f:(fun na ->
                if no_project then
                  Option.is_none na.Todd_common.Next_action.project
                else true)
        >|= List.map ~f:(fun (action : Todd_common.Next_action.t) ->
                { Reviewable.item = action.text; prompt = action.project })
        >|= Ordering.act ordering
      in
      let f () =
        Lazy.force LTerm.stdout >>= fun term ->
        dump () >>= handle_items ~term ~reviewing:Action
      in
      Fn.compose Lwt_main.run f]

let cmd =
  let make_text_items_reviewables items =
    items >|= String.Set.to_list
    >|= List.map ~f:(fun x -> { Reviewable.item = x; prompt = None })
  in
  Command.group ~summary:"Different ways to review"
    [
      review_cmd "someday"
        ~dump:(Fn.compose make_text_items_reviewables Db.dump_someday)
        ~reviewing:Someday;
      review_cmd "maybe"
        ~dump:(Fn.compose make_text_items_reviewables Db.dump_maybe)
        ~reviewing:Maybe;
      ("actions", action_cmd);
      review_cmd "waiting"
        ~dump:(Fn.compose make_text_items_reviewables Db.dump_waiting_for)
        ~reviewing:Waiting;
      review_cmd "collect"
        ~dump:(fun () ->
          Db.dump_collected ()
          >|= List.map ~f:(fun collected_item ->
                  {
                    Reviewable.item =
                      Todd_common.Collected_item.text collected_item;
                    prompt = None;
                  }))
        ~reviewing:Collect;
    ]
