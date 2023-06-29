open Tlist
open Cmdliner

exception Abort of string

module Tuple = struct
  let first (a, _) = a
  and second (_, b) = b
end

module Pretty = struct
  module T = ANSITerminal
  let p_err = [ T.Bold; T.red ]
  let perr msg =
    T.prerr_string p_err (msg ^ "\n");
end

let path_concat a b = a ^ "/" ^ b
and stat_kind path =
  let st = Unix.stat path in
  st.st_kind
and fail_with msg = Pretty.perr msg; exit 1


(* usage: filter_by_suffix_add_mtime "/path/to/dir" ".suffix.with.dot" => (basename, mtime) Seq.t *)
let filter_by_suffix_add_mtime dir suffix =
  let slen = String.length suffix in
  let filter_by_suffix str =
    let len = String.length str in
    let start = len - (min len slen) in
    String.equal suffix (String.sub str start (len - start))
  (* name => (name, mtime) if S_ISREG(name) *)
  and filter_regular_add_mtime name =
    try
      let st = Unix.stat (path_concat dir name) in
      if st.st_kind = Unix.S_REG then
        Some (name, st.st_mtime)
      else
        None
    with _ -> None (* this is probably EPERM *)
  in
  Sys.readdir dir
  |> Array.to_seq
  |> Seq.filter filter_by_suffix
  (* Filter out non-regular files and add mtime *)
  |> Seq.filter_map filter_regular_add_mtime

and seq_max val_fn map_fn seq =
  (* Find most recent one *)
  let z = Seq.fold_left (fun maxx x -> match maxx with
      | None -> Some (val_fn x, x)
      | Some (max_val, _) ->
        let x_val = val_fn x in
        if x_val > max_val then Some (x_val, x) else maxx
    ) None seq
  in z
  |> function
  | Some (_, x) -> Some (map_fn x)
  | None -> None

let read_lines ichan =
  let rec aux acc =
    try
      aux @@ input_line ichan :: acc
    with End_of_file -> acc
  in
  List.rev @@ aux []

(* Choose torrent file from dir using fzf *)
let choose_torrent dir torrents =
  Array.sort (fun (_, t0) (_, t1) -> - Float.compare t0 t1) torrents;
  let fzf_in, fzf_out = Unix.open_process "fzf -e --reverse --height 40% -m" in
  try
    Array.iter (fun (name, _) ->
        Printf.fprintf fzf_out "%s\n" name
      ) torrents;
    flush fzf_out;
    close_out fzf_out;
    let lines = read_lines fzf_in in
    match Unix.close_process (fzf_in, fzf_out) with
    | Unix.WEXITED status when status = 0 ->
        begin
          match lines with
          | x :: _ -> Some x
          | _ -> None
        end
    | _ -> None
  with _ -> ignore (Unix.close_process (fzf_in, fzf_out)); None

external query_mod_state : unit -> int = "query_mod_state"

(* Find most recent file *)
let find_recent_torrent_in_dir dir torrents =
  torrents |> seq_max Tuple.second (fun (name, _) -> path_concat dir name)

(* find most recent torrent file according to following rules:
 * 1. From command line argument
 * 1.1. if argv[1] is directory => lookup recent torrent there
 * 1.2. if argv[1] is a file => just use it
 * 2. Select most recent torrent from current directory
 *)
let find_torrent pathname =
  let file = match stat_kind pathname with
    | Unix.S_DIR ->
        let torrents = filter_by_suffix_add_mtime pathname ".torrent" |> Array.of_seq in
        begin
        match Array.length torrents with
        | 0 -> None
        | 1 -> Some (fst torrents.(0))
        | _ -> 
            let ctrl_is_held = ((query_mod_state ()) land 4) <> 0 in
            if ctrl_is_held then
              match choose_torrent pathname torrents with
              | Some fn ->
                  let now = Unix.time () in
                  let fname = pathname ^ "/" ^ fn in
                  Unix.utimes fname now now;
                  Some fname
              | None ->
                  None
            else
              (Array.to_seq torrents |> find_recent_torrent_in_dir pathname)
        end
    | Unix.S_REG -> Some pathname
    | _ -> None
  in
  match file with
  | Some x -> x, Tlist.parse x
  | None -> fail_with ("Unable to find torrent file in " ^ pathname)

(* Download single-torrent file *)
let download_simple _torrent _files =
  Some []

(* Download multiple files from torrent *)
and download_sub torrent files =
  let hh = Hashtbl.create @@ List.length files
  and fzf_in, fzf_out = Unix.open_process "fzf -e --reverse --height 40% -m" in
  let dlist =
    try
      List.iter (fun x ->
          let name = Printf.sprintf "%s %s" (if Tlist.check_file torrent x then "✔" else " ") x.tf_name in
          Hashtbl.add hh name x.tf_index;
          Printf.fprintf fzf_out "%s\n" name
        ) files;
      flush fzf_out;
      close_out fzf_out;
      let lines = read_lines fzf_in in
      let selected = match Unix.close_process (fzf_in, fzf_out) with
        | Unix.WEXITED status when status = 0 -> lines
        | _ -> []
      in
      (*List.iter (fun x -> Printf.printf " => #%-3d %s\n" (Hashtbl.find hh x) x) selected;*)
      List.map (fun x -> Hashtbl.find hh x) selected
    with _ -> ignore (Unix.close_process (fzf_in, fzf_out)); []
  in
  match dlist with
  | [] -> None
  | _ -> Some ["--select-file=" ^ (String.concat "," @@ List.map string_of_int dlist)]

let external_ip () =
  let rec get uri =
    let open Cohttp in
    let open Cohttp_lwt in
    let open Cohttp_lwt_unix in
    let open Lwt.Infix in
    Client.get (Uri.of_string uri)
    >>= fun (resp, body) ->
    let status  = Cohttp.Response.status resp in
    match status with
    | `OK -> Lwt_result.ok (Cohttp_lwt.Body.to_string body)
    | `Found
    | `Moved_permanently ->
      Lwt.return_error "Redirect handling not implemented"
    | _ -> Lwt.return_error (Printf.sprintf "Unexpected code: %d" (Code.code_of_status status))
  in
  get "http://api.ipify.org" |> Lwt_main.run |> function
  | Ok body -> Some body
  | Error msg ->
    Printf.eprintf "Unable to obtain exernal ip: %s\n" msg;
    None

(* main function *)
let tdl resolve_ext_ip pathname =
  let tname, torrent = find_torrent pathname in
  let files = Tlist.contents torrent in
  let args =
  if List.length files > 1 then
    download_sub torrent files
  else
    download_simple torrent files
  in
  match args with
  | None -> print_endline "Not downloading anything"
  | Some args ->
    let ext_ip =
      if resolve_ext_ip then
        match external_ip() with
        | Some addr ->
          Printf.printf "Using external ip %s\n" addr;
          ["--bt-external-ip=" ^ addr ]
        | None -> []
      else
        []
    in
    Printf.printf "Downloading from `%s'\n"  tname;
    flush stdout;
    Sys.chdir "/tmp";
    Unix.execvp "aria2c" @@ Array.of_list (
      "aria2c"
      :: args
      @ [tname; "--seed-time=10"]
      @ ext_ip
    )

let ext_ip =
  let doc = "Resolve external ip" in
  Arg.(value & flag & info ["e"; "ext-ip"] ~doc)

let () =
  let path =
    let doc = "Single file or directory." in
    Arg.(value & pos 0 string "." & info [] ~docv:"PATH" ~doc)
  in
  let tdl_t = Term.(const tdl $ ext_ip $ path) in
  Term.exit @@ Term.eval (tdl_t, Term.info "tdl")
