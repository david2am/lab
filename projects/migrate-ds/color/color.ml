let red = function
  | "" -> ""
  | s  -> "\027[31m" ^ s ^ "\027[0m"

let green = function
  | "" -> ""
  | s  -> "\027[32m" ^ s ^ "\027[0m"
