module B = Bencode

type tfile = {
  tf_index: int;
  tf_size: int;
  tf_name: string;
}

type t = {
  t_root: string option;
  t_files: tfile list
}

exception Bad_torrent of string
exception Missing_key of string
exception Bad_key of string * string

(* utilities to ease unwrapping of dictionary *)
module Dict = struct
  let dtype2a = function
    | B.Integer _ -> "Integer"
    | B.String _ -> "String"
    | B.List _ -> "List"
    | B.Dict _ -> "Dict"
  and type2a = function
    | `Integer -> "Integer"
    | `String -> "String"
    | `List -> "List"
    | `Dict -> "Dict"

  let get key dict = match B.dict_get dict key with
    | Some x -> x
    | None -> raise (Missing_key key)

  let opt_string = function
    | B.String x -> Some x
    | _ -> None
  and opt_int = function
    | B.Integer x -> Some x
    | _ -> None
  and opt_dict = function
    | B.Dict x -> Some x
    | _ -> None

  let unwrap ?key:(k="<anon>") opt x = match opt x with
    | Some x -> x
    | None -> raise (Bad_key (k, dtype2a x))

  (* return anonymous converter and getter *)
  let helper converter =
    (fun x -> unwrap converter x),
    (fun key dict -> unwrap ~key converter (get key dict))

  let unwrap_string, get_string = helper opt_string
  and unwrap_int, get_int = helper opt_int
  and unwrap_dict, get_dict = helper opt_dict
end

let list_files info =
  let module D = Dict in
  (* raising exceptions *)
  let make_tfile tf_index tf_name tf_size = { tf_index; tf_name; tf_size }
  in
  (* Build pathname from string or array of strings *)
  let make_pathname = function
    | B.String s -> s
    | B.List  l ->
      List.map D.unwrap_string l
      |> String.concat "/"
    | _ -> raise (Bad_key ("path", "String or List<String>"))
  in
  let extract_file index name_field dict =
    let name = D.get name_field dict |> make_pathname
    and size = D.get_int "length" dict in
    make_tfile index name (Int64.to_int size)
  in let extract_many root =
    List.mapi (fun i entry -> extract_file (i + 1) "path" entry) root
  in
  let files = B.dict_get info "files" in
  match files with
  | Some (B.List files) -> Some (D.get_string "name" info), extract_many files
  | None -> None, [ extract_file 0 "name" info ]
  | Some _ -> raise (Bad_key ("files", "List"))

(****************************************************************)
(* Parse torrent file                                           *)
(****************************************************************)
let parse src = 
  let t_root, t_files = list_files @@ Dict.get "info" @@ B.decode (`File_path src) in
  { t_root; t_files }

let contents torrent = torrent.t_files

let check_file torrent f =
  let full_path = "/tmp/" ^ (match torrent.t_root with
    | Some root -> root ^ "/" ^ f.tf_name
    | None -> f.tf_name)
  in
  try
    let st = Unix.stat full_path in
    match st.st_kind with
    | Unix.S_REG when st.st_size = f.tf_size -> true
    | _ -> false
  with _ -> false
