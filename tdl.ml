open Tlist
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

(* Find most recent file *)
let find_recent_torrent_in_dir dir =
  filter_by_suffix_add_mtime dir ".torrent"
  |> seq_max Tuple.second (fun (name, _) -> path_concat dir name)

(* find most recent torrent file according to following rules:
 * 1. From command line argument
 * 1.1. if argv[1] is directory => lookup recent torrent there
 * 1.2. if argv[1] is a file => just use it
 * 2. Select most recent torrent from current directory
 *)
let find_torrent () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let file = match stat_kind path with
    | Unix.S_DIR -> find_recent_torrent_in_dir path
    | Unix.S_REG -> Some path
    | _ -> None
  in
  match file with
  | Some x -> x, Tlist.parse x
  | None -> fail_with ("Unable to find torrent file in " ^ path)

let read_lines ichan =
  let rec aux acc =
    try
      aux @@ input_line ichan :: acc
    with End_of_file -> acc
  in
  List.rev @@ aux []

(* Download single-torrent file *)
let download_simple torrent =
  print_endline "aria2c torrent"

(* Download multiple files from torrent *)
and download_sub tname torrent files =
  let hh = Hashtbl.create @@ List.length files
  and fzf_in, fzf_out = Unix.open_process "fzf --reverse --height 40% -m" in
  let dlist, dnames =
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
      List.map (fun x -> Hashtbl.find hh x) selected, selected
    with _ -> ignore (Unix.close_process (fzf_in, fzf_out)); [], []
  in
  match dlist with
  | [] -> print_endline "Not downloading anything"
  | _ ->
    Printf.printf "Downloading from `%s'\n"  tname;
    List.iter (fun x -> Printf.printf "  * %s\n" x) dnames;
    flush stdout;
    Sys.chdir "/tmp";
    Unix.execvp "aria2c"
      [| "aria2c";
         "--select-file=" ^ (String.concat "," @@ List.map string_of_int dlist);
         tname
      |]

let () =
  let tname, torrent = find_torrent() in
  let files = Tlist.contents torrent in
  if List.length files > 1 then
    download_sub tname torrent files
  else
    download_simple tname
