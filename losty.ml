(* Download episode webpage from lostfilm.tv *)
(*
 curl 'http://www.lostfilm.tv/u/5460701/'
-H 'Host: www.lostfilm.tv'
-H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0'
-H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
-H 'Accept-Language: ru,en-US;q=0.7,en;q=0.3'
--compressed
-H 'Referer: http://www.lostfilm.tv/series/Siren/season_2/episode_3/'
-H 'Cookie: xxxxx'
-H 'Connection: keep-alive'
-H 'Upgrade-Insecure-Requests: 1'
 *)


open Soup

module Infix = struct
  let (>>=) a f = match a with
    | Some x -> Some (f x)
    | None -> None

  let (>>=) a f = match a with
    | Some x -> f x
    | None -> None
end

let parse_items soup =
  let open Infix in
  let parse_item item =
    let label = item $? "div.inner-box--label" >>= leaf_text
    and link = item $? "a" >>= attribute "href" in
    match label, link with
    | Some label, Some link -> Some (String.trim label, link)
    | _, _ -> None
  in

  (*let doc = read_file "losty_data/torrent_page.htm" |> parse in*)

  soup $$ "div.inner-box--item"
  |> to_list
  |> List.to_seq
  |> Seq.filter_map parse_item

exception Found of string

let find_or_first id items =
  let acc first (label, link) =
    if String.equal id label then raise (Found link);
    match first with
    | None -> Some link
    | _ -> first
  in
  try
    (Seq.fold_left acc None items)
  with Found found -> Some found



(* Just look for first div with "data-episode" attribute and we done! *)
let get_episode_data soup =
  let open Infix in
  let attr = "data-episode" in
  soup $? ("div[" ^ attr ^ "]") >>= attribute attr

(* convert data-eisode into search link *)
let data_to_url data = "http://lostfilm.tv/v_search2.php?a=" ^ data

  (*
let get_cookies =
  let open Lwt.Infix in
  let module Db = (val Caqti_lwt.connect (Uri.of_string "sqlite3:///tmp/cookies.sqlite") >>= Caqti_lwt.or_fail |> Lwt_main.run) in
*)

let () =
  Request.prepare();
  read_file "losty_data/torrent_page.htm" |> parse |> parse_items
  |> find_or_first "SD" |> require |> Printf.printf "===> %s\n"
  (*|> Seq.iter (fun (label, link) -> Printf.printf "<%s> ==> %s\n" label link)*)
  ;
  read_file "losty_data/episode_page.htm" |> parse |> get_episode_data
  |> require |> print_endline
  ;
  Request.get "http://localhost:9999/" |> ignore

