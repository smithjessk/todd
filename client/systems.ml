open! Core

module System = struct
  type t = {
    last_done : Date.t option; [@default None]
    n : int;
    done_ : int;
    description : string; [@default "migrated"]
  }
  [@@deriving sexp]

  let water t =
    {
      n = t.n;
      done_ = t.done_ + 1;
      description = t.description;
      last_done = Some (Date.today ~zone:(Lazy.force Time.Zone.local));
    }

  let progress_bar t =
    let string_n_times string n =
      List.init n ~f:(fun _ -> string) |> String.concat
    in
    sprintf "[%s%s]"
      (string_n_times Emoji.flexed_biceps t.done_)
      (string_n_times Emoji.seedling (t.n - t.done_))

  let pretty t ~name =
    let last_done_string =
      Option.value_map ~f:Date.to_string t.last_done ~default:"n/a"
    in
    sprintf "name: %s\ndescription: %s\nLast done: %s\n\n%s\n" name
      t.description last_done_string (progress_bar t)
end

module Systems = struct
  type t = { systems : System.t String.Map.t [@default String.Map.empty] }
  [@@deriving sexp]

  (** probably only want to use for e.g. viewing *)
  let fuzzy_find t query =
    let all_keys = Map.keys t.systems in
    let closest_key =
      List.fold all_keys ~init:None ~f:(fun closest_so_far this_key ->
          let is_better_match =
            String.is_prefix this_key ~prefix:query
            && String.length this_key
               >= Option.value_map closest_so_far ~f:String.length ~default:0
          in

          if is_better_match then Some this_key else closest_so_far)
    in
    match closest_key with
    | None ->
        raise_s
          [%message
            "Looked through keys but none seemed close"
              (Map.keys t.systems : string list)
              (query : string)]
    | Some key -> (query, Map.find_exn t.systems key)

  let sexp_systems_path, sexp_archived_systems_path =
    let f path =
      sprintf "/Users/jess/text-files-backed-up-to-gdrive/todd/%s.sexp" path
    in
    (f "systems", f "archived-systems")

  let path = function
    | `Current -> sexp_systems_path
    | `Archived -> sexp_archived_systems_path

  (** [loda_exn mode] loads a sexp file from [(path mode)] *)
  let load_exn mode = Sexp.load_sexp (path mode) |> t_of_sexp

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
          Map.add_exn t.systems ~key:name
            ~data:{ last_done = None; description; done_ = 0; n }
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

  let save_exn t mode =
    let path = path mode in
    sexp_of_t t |> Sexp.to_string_hum |> fun data ->
    Out_channel.write_all path ~data

  let show t =
    Map.iteri t.systems ~f:(fun ~key:name ~data:system ->
        Out_channel.printf "Name: %s\n" name;
        Out_channel.print_endline (System.pretty ~name system))

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
            {
              last_done = None;
              done_ = new_done;
              n = system.n;
              description = system.description;
            })
    in
    { systems }

  (** also brings that system to a backup file*)
  let archive t ~name =
    let system = Map.find_exn t.systems name in
    let old = load_exn `Archived in
    let new_old =
      let new_old_systems =
        Map.update old.systems name ~f:(function
          | Some _ ->
              raise_s
                [%message
                  "already an archived system with that name"
                    (name : string)
                    (old.systems : System.t String.Map.t)]
          | None -> system)
      in

      { systems = new_old_systems }
    in
    let () = save_exn new_old `Archived in
    let new_systems = Map.remove t.systems name in
    { systems = new_systems }
end

(** works on current systems *)
let load_save ~show ~f =
  Systems.load_exn `Current |> f |> fun systems ->
  if show then Systems.show systems;
  Systems.save_exn systems `Current

let plant =
  Command.basic ~summary:"start a new system"
    [%map_open.Command
      let name = flag "name" (required string) ~doc:" system name"
      and todo = flag "todo" (required int) ~doc:" num to do"
      and description =
        flag "description" (required string) ~doc:" desc, eg min action"
      in

      fun () ->
        load_save ~show:true ~f:(Systems.plant ~name ~n:todo ~description)]

let print_one systems ~name =
  Out_channel.(
    print_endline
      (Map.find_exn systems.Systems.systems name |> System.pretty ~name);
    flush stdout)

let water =
  Command.basic ~summary:"water a seedling system"
    [%map_open.Command
      let name = anon ("name" %: string) in
      fun () ->
        load_save ~show:false ~f:(fun systems ->
            List.range 1 20
            |> List.iter ~f:(fun _ ->
                   Out_channel.(
                     print_string "⭐";
                     flush stdout);
                   Lwt_main.run (Lwt_unix.sleep 0.1));
            Out_channel.(
              print_endline "";
              flush stdout);
            Utils.play_notification_sound ();
            let systems = Systems.water systems ~name in
            print_one systems ~name;
            systems)]

let peep =
  Command.basic ~summary:"view your systems"
    [%map_open.Command
      let () = return () in
      let systems = Systems.load_exn `Current in
      fun () -> Systems.show systems]

let raw =
  Command.basic ~summary:"directly edit a goal's value"
    [%map_open.Command
      let name = flag "name" (required string) ~doc:" system name"
      and new_done = flag "new-done" (required int) ~doc:" value to set" in
      fun () ->
        load_save ~show:false ~f:(fun sys ->
            let sys = Systems.set ~name ~new_done sys in
            print_one sys ~name;
            sys)]

let archive =
  Command.basic ~summary:"put a goal into a backup file"
    [%map_open.Command
      let name = anon ("name" %: string) in
      fun () ->
        load_save ~show:true ~f:(fun x -> Systems.archive x ~name |> fun x -> x)]

let one =
  Command.basic ~summary:"view one system by its name"
    [%map_open.Command
      let words =
        anon
          (Command.Param.non_empty_sequence_as_list ("word_in_name" %: string))
      in
      fun () ->
        let t = Systems.load_exn `Current in
        let name, system =
          Systems.fuzzy_find t (String.concat ~sep:" " words)
        in
        system |> System.pretty ~name |> Out_channel.print_endline;
        Out_channel.(flush stdout)]

let tight =
  Command.basic ~summary:"one pane view"
    (Command.Param.return (fun () ->
         let systems =
           Systems.load_exn `Current |> fun x -> x.Systems.systems
         in
         Map.iteri systems ~f:(fun ~key:name ~data:system ->
             Out_channel.(
               printf "%s: %s\n" name (System.progress_bar system);
               flush stdout))))

let undone =
  Command.basic ~summary:"one's that have been done any time before today"
    (Command.Param.return (fun () ->
         let systems =
           Systems.load_exn `Current |> fun x -> x.Systems.systems
         in
         Map.filter systems ~f:(fun system ->
             match system.System.last_done with
             | None -> true
             | Some date ->
                 not
                   (Date.equal date
                      (Date.today ~zone:(Lazy.force Time.Zone.local))))
         |> Map.iteri ~f:(fun ~key:name ~data:system ->
                Out_channel.(
                  printf "%s: %s\n" name (System.progress_bar system);
                  flush stdout))))

let command =
  Command.group ~summary:"Fuck goals"
    [
      ("water", water);
      ("plant", plant);
      ("c", peep);
      ("raw", raw);
      ("archive", archive);
      ("one", one);
      ("tight", tight);
      ("undone", undone);
    ]
