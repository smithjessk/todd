open! Core

module System = struct
  type t = { n : int; done_ : int; description : string [@default "migrated"] }
  [@@deriving sexp]

  let water t = { n = t.n; done_ = t.done_ + 1; description = t.description }

  let pretty t =
    let { n; done_; description } = t in
    let string_n_times string n =
      List.init n ~f:(fun _ -> string) |> String.concat
    in
    sprintf "description: %s\n[%s%s]\n" description
      (string_n_times Emoji.flexed_biceps done_)
      (string_n_times Emoji.seedling (n - done_))
end

module Systems = struct
  type t = { systems : System.t String.Map.t [@default String.Map.empty] }
  [@@deriving sexp]

  let sexp_systems_path =
    "/Users/jess/text-files-backed-up-to-gdrive/todd/systems.sexp"

  (** Loads a sexp file from [sexp_systems_path] *)
  let load_exn () = Sexp.load_sexp sexp_systems_path |> t_of_sexp

  let plant t ~name ~n ~description =
    match Map.find t.systems name with
    | Some _ ->
        raise_s
          [%message
            "There's already a system with that name"
              (name : string)
              (Map.keys t.systems : string list)]
    | None ->
        let systems =
          Map.add_exn t.systems ~key:name ~data:{ description; done_ = 0; n }
        in
        { systems }

  let water t ~name =
    let systems =
      Map.update t.systems name ~f:(function
        | None ->
            raise_s
              [%message
                "No system with that name"
                  (name : string)
                  (Map.keys t.systems : string list)]
        | Some system -> System.water system)
    in
    { systems }

  let save_exn t =
    sexp_of_t t |> Sexp.to_string_hum |> fun data ->
    Out_channel.write_all sexp_systems_path ~data

  let show t =
    Map.iteri t.systems ~f:(fun ~key:name ~data:system ->
        Out_channel.printf "Name: %s\n" name;
        Out_channel.print_endline (System.pretty system))

  let set t ~new_done ~name =
    let systems =
      Map.update t.systems name ~f:(function
        | None ->
            raise_s
              [%message
                "No system with that name"
                  (name : string)
                  (Map.keys t.systems : string list)]
        | Some system ->
            { done_ = new_done; n = system.n; description = system.description })
    in
    { systems }
end

let load_save ~f =
  Systems.load_exn () |> f |> fun systems ->
  Systems.show systems;
  Systems.save_exn systems

let plant =
  Command.basic ~summary:"start a new system"
    [%map_open.Command
      let name = flag "name" (required string) ~doc:" system name"
      and todo = flag "todo" (required int) ~doc:" num to do"
      and description =
        flag "description" (required string) ~doc:" desc, eg min action"
      in

      fun () -> load_save ~f:(Systems.plant ~name ~n:todo ~description)]

let water =
  Command.basic ~summary:"water a seedling system"
    [%map_open.Command
      let name = flag "name" (required string) ~doc:" system name" in
      fun () ->
        load_save ~f:(fun sys ->
            List.range 1 20
            |> List.iter ~f:(fun _ ->
                   Out_channel.(
                     print_string "⭐";
                     flush stdout);
                   Lwt_main.run (Lwt_unix.sleep 0.1));
            Systems.water sys ~name)]

let peep =
  Command.basic ~summary:"view your systems"
    [%map_open.Command
      let () = return () in
      let systems = Systems.load_exn () in
      fun () -> Systems.show systems]

let raw =
  Command.basic ~summary:"directly edit a goal's value"
    [%map_open.Command
      let name = flag "name" (required string) ~doc:" system name"
      and new_done = flag "new-done" (required int) ~doc:" value to set" in
      fun () -> load_save ~f:(Systems.set ~name ~new_done)]

let command =
  Command.group ~summary:"Fuck goals"
    [ ("water", water); ("plant", plant); ("c", peep); ("raw", raw) ]
