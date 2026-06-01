(* ---------- Command-line arguments ---------- *)

let filename   = ref ""
let folderpath = ref ""
let ext        = ref ""
let write      = ref false

let version = "0.1.0"

let rules = [
  ("-f", Arg.Set_string filename,   " file to read");
  ("-p", Arg.Set_string folderpath, " folder path to read");
  ("-x", Arg.Set_string ext,        " extension file to filter");
  ("-w", Arg.Set write,             " write changes back to the file");
  ("-v", Arg.Unit (fun () -> print_endline version; exit 0), " print version and exit");
]

let usage =  "usage: some -f <filename>, or some -p <folderpath>"

(* ---------- Entry point ---------- *)

let () =
  Arg.parse (Arg.align rules) (fun _ -> ()) usage;

  if !filename = "" && !folderpath = "" then (
    print_endline "Please pass -f <filename>, or -p <folderpath>";
    exit 1
  );


  let file_list = Files.find_files !folderpath !ext in

  List.iter (fun file ->
    Files.process_file file !write
  ) file_list
