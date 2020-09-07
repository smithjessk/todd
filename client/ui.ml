open Core
open React

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
        (List.map ~f:(fun opt -> (opt, Zed_string.of_utf8 "")) options)

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
         eval [ B_bold true; S prompt; E_bold ]))

    method! show_box = false

    method run_utf8_conv =
      let open Lwt.Infix in
      self#run >|= Zed_string.to_utf8
  end
