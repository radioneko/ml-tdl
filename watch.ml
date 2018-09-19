let watch() =
  let module I = Inotify in
  let open Lwt in
  let rec process inotify watch =
    Lwt_inotify.read inotify
    >>= fun result ->
    let _, events, _, name_opt = result in
    let evls = List.map I.string_of_event_kind events
    and name = match name_opt with
      | Some x -> x
      | None -> "<MISSING>"
    in
    Lwt_io.printf "%s [ %s ]\n" (String.concat " | " evls) name >>= fun () ->
    process inotify watch
  in
  Lwt_inotify.create()
  >>= fun inotify ->
  Lwt_inotify.add_watch inotify "/tmp" [Inotify.S_Create; Inotify.S_Close_write]
  >>= process inotify

let () = print_endline "Starting"
let () = Lwt_main.run @@ watch()
