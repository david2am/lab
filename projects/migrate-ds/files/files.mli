val process_file : string -> bool -> unit
(** [process_file path write] : Reads a file given a [path] and writes if [write] is true
*)

val find_files : string -> string -> string list
(** [find_file dir ext] : Recursively find files with a given [extension] in a given [dir]
*)
