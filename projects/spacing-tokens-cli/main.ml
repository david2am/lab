(* ---------- Configuration ---------- *)

let keywords = [
  " margin: "; " padding: "; " top: "; " bottom: "; " right: "; " left: ";
  "margin-top: "; "margin-bottom: "; "margin-right: "; "margin-left: ";
  "padding-top: "; "padding-bottom: "; "padding-right: "; "padding-left: ";
]

(* ---------- Command-line arguments ---------- *)

let filename = ref ""

let rules = [
  ("-f", Arg.Set_string filename, " file to read");
]

let usage =  "usage: some -f <filename>"

(* ---------- Keyword searching ---------- *)

let contains line line_num =
  let find_keyword kw =
    let pattern = Str.regexp_string_case_fold kw in    
    try
      let _ = Str.search_forward pattern line 0 in
      Printf.printf "Line %d: \n" line_num;
      print_endline line
    with
    | Not_found -> () (* Skip quietly if not found *)
  in

  List.iter find_keyword keywords

(* ---------- File processing ---------- *)

let scan_channel ic =
  let rec loop line_num =
    match In_channel.input_line ic with
    | Some line ->
      contains line line_num;
      loop (line_num + 1)
    | None -> () (* End of file reached smoothly *)        
  in
    loop 1


let scan_file path =
  try
    In_channel.with_open_text path scan_channel
  with
  | Sys_error msg ->
     Printf.printf "Could not read file: %s\n" msg;
     exit 1

(* ---------- Entry point ---------- *)

let () =
  Arg.parse (Arg.align rules) (fun _ -> ()) usage;

  if !filename = "" then (
    print_endline "Please pass -f <filename>";
    exit 1
  );

  scan_file !filename

  
