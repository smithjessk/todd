open Core
open React

module Tui_action_for_processed_item : sig
  type t =
    | Move_to_maybe
    | Move_to_someday
    | Move_to_actions  (** can also be used for editing action text *)
    | Delete
    | Copy_to_clipboard
    | Jump_to_next
    | Jump_to_previous
    | Open_link
    | Search
    | Quit
    | Help

  val bindings : t String.Map.t

  val help : string

  val with_prefix_exn : string -> int * t
  (** Takes strings of the regexp format [1-9]*t
      where t is a key in [bindings]
  *)
end = struct
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
      | Help ->
          "?"
      | Move_to_maybe ->
          "m"
      | Move_to_someday ->
          "s"
      | Move_to_actions ->
          "d"
      | Delete ->
          "x"
      | Copy_to_clipboard ->
          "y"
      | Jump_to_next ->
          "j"
      | Jump_to_previous ->
          "k"
      | Open_link ->
          "o"
      | Search ->
          "/"
      | Quit ->
          "qq"
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

  let help =
    bindings |> Map.to_alist
    |> List.map ~f:(fun (s, t) ->
           sprintf "%s %s" s (t |> sexp_of_t |> Sexp.to_string_hum) )
    |> String.concat ~sep:"\n"

  let with_prefix_exn s =
    let cnt = String.take_while s ~f:Char.is_digit in
    let cmd = String.drop_prefix s (String.length cnt) in
    match Map.find bindings cmd with
    | None ->
        raise (Invalid_argument s)
    | Some t -> (
      match cnt with "" -> (1, t) | cnt -> (Int.of_string cnt, t) )
end

class read_line_const_prompt_options ~term ~prompt ~options =
  object (self)
    inherit LTerm_read_line.read_line ()

    inherit [Zed_string.t] LTerm_read_line.term term

    method! completion =
      let prefix = Zed_rope.to_string self#input_prev in
      let options = List.map options ~f:Zed_string.of_utf8 in
      let options =
        List.filter ~f:(fun opt -> Zed_string.starts_with opt ~prefix) options
      in
      self#set_completion 0
        (List.map ~f:(fun opt -> (opt, Zed_string.of_utf8 " ")) options)

    initializer
    self#set_prompt
      (S.const (prompt |> Zed_string.of_utf8 |> LTerm_text.of_string))

    method run_utf8_conv =
      let open Lwt.Infix in
      self#run >|= Zed_string.to_utf8
  end

class read_line_const_prompt ~term ~prompt =
  object (self)
    inherit LTerm_read_line.read_line ()

    inherit [Zed_string.t] LTerm_read_line.term term

    initializer
    self#set_prompt
      (S.const
         (let open LTerm_text in
         eval [B_bold true; S prompt; E_bold]))

    method! show_box = false

    method run_utf8_conv =
      let open Lwt.Infix in
      self#run >|= Zed_string.to_utf8
  end
