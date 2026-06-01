let is_inside_parens s pos =
  let rec scan i depth =
    if i < 0 then false
    else
      match s.[i] with
      | '(' when depth = 0 -> true
      | '(' -> scan (i - 1) (depth - 1)
      | ')' -> scan (i - 1) (depth + 1)
      | _   -> scan (i - 1) depth
  in
  scan (pos - 1) 0

let is_keyword_found line keywords =
  List.exists (fun kw ->
    let pattern = Str.regexp_string_case_fold kw in    
    try
      let _ = Str.search_forward pattern line 0 in
      true
    with Not_found -> false
  ) keywords

let update_line line mapping mapping_pattern =
  Str.global_substitute mapping_pattern (fun s ->
    let old_value = Str.matched_string s in

    let start = Str.match_beginning () in
    let len = String.length old_value in
    let line_len = String.length s in
  
    let followed_by_invalid_unit =
      if start + len < line_len then
        let next_char = s.[start + len] in
        next_char = '.' || (next_char >= 'a' && next_char <= 'z') || next_char = '%'
      else
        false
    in

    if followed_by_invalid_unit then
      old_value (* Skip: it's a decimal fraction or an unsupported unit like em/rem *)
    else if is_inside_parens s start then
      old_value (* Skip: inside function parentheses *)
    else
      try List.assoc old_value mapping
      with Not_found -> old_value
  ) line    

let give_report updated_line line line_num =
  if updated_line <> line then begin
    Printf.printf "Line %d (+):\n" line_num;
    print_endline @@ Color.red ("- " ^ line);
    print_endline @@ Color.green ("+ " ^ updated_line)
  end else
    print_endline "....................................................................................";
    Printf.printf "Line %d:\n" line_num;
    print_endline ("o" ^ line);
    print_endline "...................................................................................."


let process_line line line_num =
  if is_keyword_found line Config.keywords then begin
    let updated_line = update_line line Config.mapping Config.mapping_pattern in
    give_report updated_line line line_num;
    updated_line
  end else
    line
