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
    let index = String.index_opt sub_str '\n' in

    match index with
    | None ->
      Buffer.add_substring acc sub_str 0 n;
      get_lines acc lines buffer ic
    | Some idx ->
      let line =
        Buffer.add_substring acc sub_str 0 idx;
        Buffer.contents acc
      in
      Buffer.clear acc; (* reset for next line *)

      Buffer.add_substring acc sub_str (idx + 1) (n - idx - 1);

      get_lines acc (line::lines) buffer ic


let read_file file =
  let acc = Buffer.create 100 in
  let buffer = Bytes.create 8 in

  try  Ok (
    In_channel.with_open_bin file (get_lines acc [] buffer)
  )
  with Sys_error msg -> Error msg

let () =
  match read_file "message.txt" with
  | Ok  lines -> List.iter (Printf.printf "read: %s\n") lines
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
