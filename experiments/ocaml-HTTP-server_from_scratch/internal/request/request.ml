type request_line = {
  http_version   : string;
  request_target : string;
  request_method : string;
}

type request = {
  request_line : request_line;
}

type request_error = 
  | Invalid_number_parts

let separator = "\r\n"

type error_malformed_request_line =
  | Invalid_request_line
  | Unsupported_http_version
  | Unable_to_read

let is_valid_http r =
  r.http_version = "HTTP/1.1"
 
let parse_request_line line =
  match String.find_first ~sub:separator ~start:0 line with
  | None -> Ok (None, line) (* should not happen *)
  | Some idx ->
    let start_line = String.sub line 0 idx in
    let sep_len = String.length separator in
    let rest_of_msg = String.sub
      line
      (idx + sep_len)
      ((String.length line) - (idx + sep_len))
    in

    match String.split_on_char ' ' start_line with
    | [ request_method; request_target; http_version ] ->
      let req = { request_method; request_target; http_version; } in
      if not @@ is_valid_http req then
        Error (Unsupported_http_version, rest_of_msg)
      else
        Ok (Some req, rest_of_msg)
    | _ ->
      Error (Invalid_request_line, rest_of_msg)
  

let request_from_reader ic : In_channel.t =
  match In_channel.input_all ic with
  | exception _ ->
    Error Unable_to_read
  | data ->
    match parse_request_line data with
    | Error (err, _)   -> Error err
    | Ok (None, _)     -> Error Invalid_request_line
    | Ok (Some req, _) -> Ok req
  
