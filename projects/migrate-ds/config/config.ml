let keywords = [
  "margin: "; "padding: "; " top: "; " bottom: "; " right: "; " left: ";
  "margin-top: "; "margin-bottom: "; "margin-right: "; "margin-left: ";
  "padding-top: "; "padding-bottom: "; "padding-right: "; "padding-left: ";
]

let mapping = [
  (* tokens *)
  ("$spacing-2",  "$space-050");
  ("$spacing-4",  "$space-100");
  ("$spacing-8",  "$space-200");
  ("$spacing-12", "$space-300");
  ("$spacing-16", "$space-400");
  ("$spacing-20", "$space-500");
  ("$spacing-24", "$space-600");
  ("$spacing-28", "$space-700");
  ("$spacing-32", "$space-800");
  ("$spacing-36", "$space-900");
  ("$spacing-40", "$space-1000");
  ("$spacing-44", "$space-1100");
  ("$spacing-48", "$space-1200");
  ("$spacing-52", "$space-1300");
  ("$spacing-56", "$space-1400");
  ("$spacing-60", "$space-1500");
  ("$spacing-64", "$space-1600");
  ("$spacing-72", "$space-1800");
  ("$spacing-80", "$space-2000");
  (* numbers *)
  ("0",  "$space-0");
  ("2",  "$space-050");
  ("4",  "$space-100");
  ("8",  "$space-200");
  ("12", "$space-300");
  ("16", "$space-400");
  ("20", "$space-500");
  ("24", "$space-600");
  ("28", "$space-700");
  ("32", "$space-800");
  ("36", "$space-900");
  ("40", "$space-1000");
  ("44", "$space-1100");
  ("48", "$space-1200");
  ("52", "$space-1300");
  ("56", "$space-1400");
  ("60", "$space-1500");
  ("64", "$space-1600");
  ("72", "$space-1800");
  ("80", "$space-2000");
  (* pixels *)
  ("0px", "$space-0");
  ("2px",  "$space-050");
  ("4px",  "$space-100");
  ("8px",  "$space-200");
  ("12px", "$space-300");
  ("16px", "$space-400");
  ("20px", "$space-500");
  ("24px", "$space-600");
  ("28px", "$space-700");
  ("32px", "$space-800");
  ("36px", "$space-900");
  ("40px", "$space-1000");
  ("44px", "$space-1100");
  ("48px", "$space-1200");
  ("52px", "$space-1300");
  ("56px", "$space-1400");
  ("60px", "$space-1500");
  ("64px", "$space-1600");
  ("72px", "$space-1800");
  ("80px", "$space-2000");
]

let mapping_pattern =
  let keys = List.map fst mapping in
  let already_replaced = List.sort_uniq compare (List.map snd mapping) in

  (* Sort longest-first so e.g. "$spacing-16" wins over "16" at the same position *)
  let sort_longest_first lst =
    List.sort (fun a b -> compare (String.length b) (String.length a)) lst
  in

  (* Ignore targets come first so the engine consumes them before trying keys *)
  let all_terms = sort_longest_first already_replaced @ sort_longest_first keys in
  let quoted = List.map Str.quote all_terms in
  let pattern_string = String.concat "\\|" quoted in
  Str.regexp pattern_string
