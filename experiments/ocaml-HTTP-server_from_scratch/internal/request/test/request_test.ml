open Request

let pp_error ppf = function
  | Invalid_number_parts -> Format.fprintf ppf "Invalid_number_parts"

let error_testeable = Alcotest.testable pp_error ( = )

let test_request_line_parser () =
  let actual = request_from_reader (
    "GET / HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: curl/8.4.0\r\nAccept: */*\r\n\r\n"
  ) in
  Alcotest.(check (result pass error_testeable))
    "good GET request line"
    (Ok { request_method = "GET"; request_target = "/"; http_version = "1.1" })
    actual;

  let actual = request_from_reader (
    "GET /coffee HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: curl/8.4.0\r\nAccept: */*\r\n\r\n"
  ) in
  Alcotest.(check (result pass error_testeable))
    "good GET request line with path"
    (Ok { request_method = "GET"; request_target = "/coffee"; http_version = "1.1" })
    actual;

  let actual = request_from_reader (
    "/coffee HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: curl/8.4.0\r\nAccept: */*\r\n\r\n"
  ) in
  Alcotest.(check (result pass error_testeable))
    "should fail"
    (Error Invalid_number_parts)
    actual
  
  
  
let () =
  Alcotest.run "Test case name"
    [
      ("component_name", [Alcotest.test_case "# of test cases" `Quick test_request_line_parser]);
    ]
