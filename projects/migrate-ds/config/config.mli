(* This Module has the app's parameters *)

val keywords : string list
(**
  This is a list of keywords to search line by line in a file
  - This list shouldn't have duplicates,
  - it shouldn't be empty []
*)
val mapping : (string * string) list
(** This is an associated list that contains old_values as keys and new_values
    as values, [(old_value1, new_value1); ...; (old_valuen, new_valuen)], its
    purpose is to be used to replace old_values by new_values in a line of text
    - The list must not contain duplicated keys
    - should not be empty [] or [()]
*)
val mapping_pattern : Str.regexp
(** This is a regex expression that matches old_values from the mapping list
    in a text
*)
