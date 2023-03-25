(* Open cookies etc *)
val prepare : unit -> unit

(* Perform HTTP get using cookies and browser's user-agent *)
val get : string -> string Lwt.t
