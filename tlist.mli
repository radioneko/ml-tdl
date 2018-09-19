type torrent_file = { tf_index : int; tf_size : int; tf_name : string; }
exception Bad_torrent of string
exception Missing_key of string
exception Bad_key of string * string
val list_files : string -> torrent_file list
