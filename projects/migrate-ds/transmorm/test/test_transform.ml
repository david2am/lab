open Transform

let process_line_tests () =
  let open Config in
  Alcotest.(check string)
    "has no keyword"
    "random string" (process_line "random string" 1);
  Alcotest.(check string)
    "has keyword but no old_value"
    "margin: $space-100;" (process_line "margin: $space-100;" 1);
  Alcotest.(check string)
    "has keyword with old_value"
    "margin: $space-200;" (process_line "margin: $spacing-8;" 1);
  Alcotest.(check string)
    "has keyword and has several old_values"
    "padding: $space-1000 $space-0;" (process_line "padding: $spacing-40 0;" 1);
  Alcotest.(check string)
    "has keyword and has several more old_values"
    "padding: $space-0 $space-0 $space-050 $space-050;" (process_line "padding: 0px 0 2 2px;" 1);
  Alcotest.(check string)
    "has keyword but value is not in mapping"
    "margin-top: -10px;" (process_line "margin-top: -10px;" 1)

let () =
  Alcotest.run "transform"
    [
      ("process_line", [Alcotest.test_case "# of test cases" `Quick process_line_tests]);
    ]
