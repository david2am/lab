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

let is_inside_parens s pos =
  let rec scan i depth =
    if i < 0 then false
    else match s.[i] with
    | '(' when depth = 0 -> true
    | '(' -> scan (i - 1) (depth - 1)
    | ')' -> scan (i - 1) (depth + 1)
    | _   -> scan (i - 1) depth
  in
  scan (pos - 1) 0

(* ------- Keyword matching and replacement ------- *)

let process_line line line_num =

  let found_keyword line =
    List.exists (fun kw ->
      let pattern = Str.regexp_string_case_fold kw in    
      try
        let _ = Str.search_forward pattern line 0 in
        true
      with Not_found -> false (* Skip quietly if not found *)
    ) keywords
  in

  let update_line line =
    Str.global_substitute mapping_pattern (fun s ->
      let old_value = Str.matched_string s in
      let start = Str.match_beginning () in
      if is_inside_parens s start then
        old_value                              (* leave function args alone *)
      else
        try List.assoc old_value mapping
        with Not_found -> old_value
    ) line
  in
    
  if found_keyword line then begin
    let updated_line = update_line line in
      if updated_line <> line then begin
        Printf.printf "Line %d (+):\n" line_num;
        print_endline ("- " ^ line);
        print_endline ("+ " ^ updated_line)
      end else begin
        print_endline "................................................................................................";
        Printf.printf "Line %d:\n" line_num;
        print_endline ("o" ^ line);
        print_endline "................................................................................................";
      end;
      updated_line
  end else
    line

(* ---------- File processing ---------- *)

let read_file ic =
  print_string "\n";
  let rec loop acc line_num =
    match In_channel.input_line ic with
    | Some line ->
      let new_line = process_line line line_num in
      loop (new_line :: acc) (line_num + 1)
    | None -> List.rev acc (* End of file reached smoothly *)        
  in
    loop [] 1

let write_file path lines =
  let tmp = path ^ ".tmp" in
  Out_channel.with_open_text tmp (fun oc ->
    List.iter (fun l ->
      Out_channel.output_string oc l;
      Out_channel.output_char oc '\n'
    ) lines
  );
  Sys.rename tmp path

let process_file path write =
  let lines =
    try
      In_channel.with_open_text path read_file
    with
    | Sys_error msg ->
       Printf.printf "Could not read file: %s\n" msg;
       exit 1
  in

  if !write then begin
    write_file path lines;
    Printf.printf "\nWrote changes to %s\n" path
  end else
    print_endline "\n(Dry run _ pass -w to apply changes)"

(* ---------- Command-line arguments ---------- *)

let filename = ref ""
let write    = ref false
let version = "0.1.0"

let rules = [
  ("-f", Arg.Set_string filename, " file to read");
  ("-w", Arg.Set write, " write changes back to the file");
  ("-v", Arg.Unit (fun () -> print_endline version; exit 0), " print version and exit");
]

let usage =  "usage: some -f <filename>"

(* ---------- Entry point ---------- *)

let () =
  Arg.parse (Arg.align rules) (fun _ -> ()) usage;

  if !filename = "" then (
    print_endline "Please pass -f <filename>";
    exit 1
  );

  process_file !filename write

  
