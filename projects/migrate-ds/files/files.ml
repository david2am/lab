let read_file ic =
  print_string "\n";
  let rec loop acc line_num =
    match In_channel.input_line ic with
    | Some line ->
      let new_line = Transform.process_line line line_num in
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
    with Sys_error msg ->
       Printf.printf "Could not read file: %s\n" msg;
       exit 1
  in

  if write then begin
    write_file path lines;
    Printf.printf "\nWrote changes to %s\n" path
  end else
    print_endline "\n(Dry run _ pass write = true to apply changes)"

(* ---------- *)

let rec find_files dir ext =
  Sys.readdir dir
  |> Array.to_list
  |> List.concat_map (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then find_files path ext
      else if Filename.extension path = ext then [path]
      else [])
