let read_file file =
  let read_channel ic =
    let buffer = Bytes.create 8 in

    let rec loop acc =
      match In_channel.input ic buffer 0 (Bytes.length buffer) with
      | 0 -> ()
      | n ->
        let sub_str = (Bytes.sub_string buffer 0 n) in
        let index = sub_str |> String.find_first ~sub:"\n" |> Option.value ~default:(-1) in

        if index <> (-1) then
          let len = String.length sub_str in
          let line = acc ^ String.sub sub_str 0 index in
          
          Printf.printf "read: %s\n" line;

          loop (String.sub sub_str index (len - index))
        else
          loop (acc ^ sub_str)
    in

    loop ""
  in 

  try Ok (In_channel.with_open_bin file read_channel)
  with Sys_error msg -> Error msg

let () =
  match read_file "message.txt" with
  | Ok () -> ()
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
