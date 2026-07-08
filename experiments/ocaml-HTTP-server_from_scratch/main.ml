let rec get_lines (acc : Buffer.t) lines buffer ic =
  match In_channel.input ic buffer 0 (Bytes.length buffer) with
  | 0 ->
    (* EOF *)
    let final_lines =
      if Buffer.length acc <> 0 then
        Buffer.contents acc :: lines
      else
        lines
    in

    List.rev final_lines
  | n ->
    let sub_str = BytesLabels.sub_string buffer ~pos:0 ~len:n in

    let rec process_chunk current_lines start_pos =
      match String.index_from_opt sub_str start_pos '\n' with
      | None ->
        Buffer.add_substring acc sub_str start_pos (n - start_pos);
        current_lines
      | Some idx ->
        Buffer.add_substring acc sub_str start_pos (idx - start_pos);
        let line = Buffer.contents acc in
        Buffer.clear acc; (* reset for next line *)

        process_chunk (line :: current_lines) (idx + 1)
    in

    let updated_lines = process_chunk lines 0 in
    get_lines acc updated_lines buffer ic


let read_file file =
  try  Ok (
    let acc = Buffer.create 100 in
    let buffer = Bytes.create 8 in
    In_channel.with_open_bin file (get_lines acc [] buffer)
  )
  with Sys_error msg -> Error msg

(* main function *)
let () =
  match read_file "message.txt" with
  | Ok  lines -> List.iter (Printf.printf "read: %s\n") lines
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
