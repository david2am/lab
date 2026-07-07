let read_file file =
  let read_channel ic =
    let buffer = Bytes.create 8 in

    let rec get_lines acc lines =
      match In_channel.input ic buffer 0 (Bytes.length buffer) with
      | 0 ->
        let final_lines = if String.length acc <> 0 then acc::lines else lines in
        List.rev final_lines
      | n ->
        let sub_str = BytesLabels.sub_string buffer ~pos:0 ~len:n in
        let index = sub_str
          |> String.find_first ~sub:"\n"
          |> Option.value ~default:(-1)
        in

        if index <> (-1) then
          let len = String.length sub_str in
          let line = acc ^ String.sub sub_str 0 index in
          
          get_lines (StringLabels.sub sub_str ~pos:index ~len:(len - index)) (line::lines)
        else
          get_lines (acc ^ sub_str) lines
    in

    get_lines "" []
  in 

  try  Ok (In_channel.with_open_bin file read_channel)
  with Sys_error msg -> Error msg

let () =
  match read_file "message.txt" with
  | Ok lines  -> List.iter (Printf.printf "read: %s\n") lines
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
