module Env = struct
  let home = Sys.getenv "HOME"
end

let user_agent = "Mozilla/5.0 (X11; Linux x86_64; rv:62.0) Gecko/20100101 Firefox/62.0"

let path_concat = String.concat "/"

let in_homedir path =
  path_concat [Env.home; path]

let mozilla_profile root =
  let ini_path = path_concat [root; "profiles.ini"] in
  let ini = new Inifiles.inifile ini_path in
  let ppath = ini#getval "Profile0" "Path" in
  path_concat [root; ppath]


(* Find most recent cookies database of firefox/waterfox *)
let mozcookie () =
  let cookie_sqlite path = path_concat [path; "cookies.sqlite"] in
  let with_mtime path =
    path, (Unix.stat path).st_mtime in
  let collect collected dir =
    try
      let candidate =
        in_homedir dir
        |> mozilla_profile
        |> cookie_sqlite
        |> with_mtime in
      candidate :: collected
    with
    | _ -> collected
  in
  let most_recent_first (_, mtime1) (_, mtime2) =
    Float.compare mtime2 mtime1 in
  [ ".mozilla/firefox"; ".waterfox" ]
  |> List.fold_left collect []
  |> List.sort most_recent_first
  |> List.hd |> fst

let stmt = Caqti_request.collect Caqti_type.string Caqti_type.(tup2 string string) "SELECT name, value FROM moz_cookies WHERE baseDomain = ?";;

let prepare () =
  mozcookie ()
  |> print_endline

let rec get uri =
  let open Cohttp in
  let open Cohttp_lwt in
  let open Cohttp_lwt_unix in
  let open Lwt.Infix in
  Client.get (Uri.of_string uri)
  >>= fun (resp, body) ->
  let status = Cohttp.Response.status resp in
  match status with
  | `OK -> Cohttp_lwt.Body.to_string body
  | `Found
  | `Moved_permanently ->
    get uri
  | _ -> failwith (Printf.sprintf "Unexpected code: %d" (Code.code_of_status status))
