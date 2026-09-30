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


let run_server () =
  let server_socket  = Unix.(socket PF_INET SOCK_STREAM 0) in
  let server_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in

  (* allow rebinding to this port ignoring TIME_WAIT from an earlier run *)
  Unix.(setsockopt server_socket SO_REUSEADDR true);
  Unix.bind server_socket server_address;

  Unix.listen server_socket 10;
  Printf.printf "Server is listening on port 8080...\n%!";
  
  while true do
    let (client_socket, client_address) = Unix.accept server_socket in
    Printf.printf "Client connected!\n%!";

    (try
      let client_ic = Unix.in_channel_of_descr client_socket in
      (try
        let acc = Buffer.create 100 in
        let buffer = Bytes.create 8 in
        let lines = get_lines acc [] buffer client_ic in
        List.iter (Printf.printf "read: %s\n%!") lines;
      with e ->
        (* guarantee the channel closes *)
        close_in_noerr client_ic;
        raise e
      );
    close_in client_ic
    with Unix.Unix_error (err, fn, _) ->
      Printf.printf "Client error during %s: %s\n%!" fn (Unix.error_message err)
    )

  done

let () =
  run_server ()
