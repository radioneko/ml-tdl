type torrent_file = {
  tf_index: int;
  tf_size: int;
  tf_name: string;
}

module Result = struct
  type 'a t =
    | Ok of 'a
    | Fail of string

  let fail msg = Fail msg
  let ok r = Ok r
end

exception Bad_torrent of string
exception Missing_key of string
exception Bad_key of string * string

module Fn_fap = struct
  exception No_value
  let unwrap = function
    | Some x -> x
    | None -> raise No_value
  and is_some = function
    | Some _ -> true
    | None -> false
  and (>>=) a b = match a with
    | None -> None
    | Some x -> b x
  and diag name = function
    | Some _ -> Printf.printf "SOME <%s>\n" name
    | None -> Printf.printf "MISSING <%s>\n" name
  let diagi name opt = diag name opt; opt
end

let list_files src =
  let module B = Bencode in
  let open Fn_fap in
  (* raising exceptions *)
  let dict_get_exn key dict = match B.dict_get dict key with
    | Some x -> x
    | None -> raise (Missing_key key)
  in
  let dict_get_string key dict = match dict_get_exn key dict with
    | B.String x -> x
    | _ -> raise (Bad_key (key, "String"))
  and dict_get_int key dict = match dict_get_exn key dict with
    | B.Integer x -> x
    | _ -> raise (Bad_key (key, "Integer"))
  and dict_get_opt key dict = B.dict_get dict key
  and match_list node = match node with
    | B.List l -> Some l
    | _ -> None
  and make_tfile tf_index tf_name tf_size = { tf_index; tf_name; tf_size }
  in
  (* Build pathname from string or array of strings *)
  let make_pathname = function
    | B.String s -> s
    | B.List  l ->
      List.map (function | B.String x -> x | _ -> raise (Bad_key ("path", "String"))) l
      |> String.concat "/"
    | _ -> raise (Bad_key ("path", "String or List<String>"))
  in
  let extract_file index name_field dict =
    let name = dict_get_exn name_field dict |> make_pathname
    and size = dict_get_int "length" dict in
    make_tfile index name size
  in let extract_many root =
    List.mapi (fun i entry -> extract_file (i + 1) "path" entry) root
  in
  let info = dict_get_exn "info" (B.decode (`File_path src)) in
  let files = dict_get_opt "files" info in
  if is_some files then
    files >>= match_list |> unwrap |> extract_many
  else
    [ extract_file 0 "name" info ]
;;
