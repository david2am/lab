let read_file file =
  let read_channel ic =
    let buffer = Bytes.create 8 in

    let rec loop () =
      match In_channel.input ic buffer 0 (Bytes.length buffer) with
      | 0 -> ()
      | n ->
        print_string (Bytes.sub_string buffer 0 n);
        loop ()
    in

    loop ()
  in 

  try Ok (In_channel.with_open_bin file read_channel)
  with Sys_error msg -> Error msg

let () =
  match read_file "message.txt" with
  | Ok () -> ()
  | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1
