(* ---------- Configuration ---------- *)

let keywords = [
  " margin: "; " padding: "; " top: "; " bottom: "; " right: "; " left: ";
  "margin-top: "; "margin-bottom: "; "margin-right: "; "margin-left: ";
  "padding-top: "; "padding-bottom: "; "padding-right: "; "padding-left: ";
]

(* (old-value, new-value) *)
let mapping = [
  (* tokens *)
  ("$spacing-2",  "$space-050");
  ("$spacing-4",  "$space-100");
  ("$spacing-8",  "$space-200");
  ("$spacing-12", "$space-300");
  ("$spacing-16", "$space-400");
  ("$spacing-20", "$space-500");
  ("$spacing-24", "$space-600");
  ("$spacing-28", "$space-700");
  ("$spacing-32", "$space-800");
  ("$spacing-36", "$space-900");
  ("$spacing-40", "$space-1000");
  ("$spacing-44", "$space-1100");
  ("$spacing-48", "$space-1200");
  ("$spacing-52", "$space-1300");
  ("$spacing-56", "$space-1400");
  ("$spacing-60", "$space-1500");
  ("$spacing-64", "$space-1600");
  ("$spacing-72", "$space-1800");
  ("$spacing-80", "$space-2000");
  (* numbers *)
  ("0",  "$space-0");
  ("2",  "$space-050");
  ("4",  "$space-100");
  ("8",  "$space-200");
  ("12", "$space-300");
  ("16", "$space-400");
  ("20", "$space-500");
  ("24", "$space-600");
  ("28", "$space-700");
  ("32", "$space-800");
  ("36", "$space-900");
  ("40", "$space-1000");
  ("44", "$space-1100");
  ("48", "$space-1200");
  ("52", "$space-1300");
  ("56", "$space-1400");
  ("60", "$space-1500");
  ("64", "$space-1600");
  ("72", "$space-1800");
  ("80", "$space-2000");
  (* pixels *)
  ("0px ", "$space-0");
  ("2px",  "$space-050");
  ("4px",  "$space-100");
  ("8px",  "$space-200");
  ("12px", "$space-300");
  ("16px", "$space-400");
  ("20px", "$space-500");
  ("24px", "$space-600");
  ("28px", "$space-700");
  ("32px", "$space-800");
  ("36px", "$space-900");
  ("40px", "$space-1000");
  ("44px", "$space-1100");
  ("48px", "$space-1200");
  ("52px", "$space-1300");
  ("56px", "$space-1400");
  ("60px", "$space-1500");
  ("64px", "$space-1600");
  ("72px", "$space-1800");
  ("80px", "$space-2000");
]

(* let mapping_pattern = *)
  (* Str.regexp "\\$spacing-[0-9]+\\|\\b-?[0-9]+px\\b\\|\\b-?[0-9]+\\b" *)

let mapping_pattern =
  let wrap (key, _) =
    if String.length key > 0 && key.[0] = '$' then
      Str.quote key
    else
      "\\b-?" ^ Str.quote key ^ "\\b"
  in
  Str.regexp (String.concat "\\|" (List.map wrap mapping))

(* ---------- Command-line arguments ---------- *)

let filename = ref ""

let rules = [
  ("-f", Arg.Set_string filename, " file to read");
]

let usage =  "usage: some -f <filename>"

(* ------- Keyword matching and replacement ------- *)

let match_keyword line =
  List.exists (fun kw ->
    let pattern = Str.regexp_string_case_fold kw in    
    try
      let _ = Str.search_forward pattern line 0 in
      true
    with Not_found -> false (* Skip quietly if not found *)
  ) keywords

let update_content line =
  Str.global_substitute mapping_pattern (fun matched_txt ->
    let old_value = Str.matched_string matched_txt in
    try List.assoc old_value mapping
    with Not_found -> old_value
  ) line

let process_line line line_num =
  if match_keyword line then begin
    let updated_line = update_content line in
      if updated_line <> line then begin
        Printf.printf "Line %d:\n" line_num;
        print_endline ("- " ^ line);
        print_endline ("+ " ^ updated_line)
      end
  end

(* ---------- File processing ---------- *)

let scan_channel ic =
  print_string "\n";
  let rec loop line_num =
    match In_channel.input_line ic with
    | Some line ->
      process_line line line_num;
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

  
