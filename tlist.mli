type t
type tfile = { tf_index : int; tf_size : int; tf_name : string; }

exception Bad_torrent of string
exception Missing_key of string
exception Bad_key of string * string
val parse : string -> t
val contents : t -> tfile list
val check_file : t -> tfile -> bool
