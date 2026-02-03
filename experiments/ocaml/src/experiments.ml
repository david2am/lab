
let is_leap () = 
  (** let year = read_int () in *)
  let year = int_of_string Sys.argv.(1) in
  let leap = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0 in
  let msg = if leap then "is" else "is not" in

  Printf.printf "%d %s a leap year\n" year msg


let approx_pi () =
  let n = read_int () in
  let p = ref 0 in

  for k = 1 to n do
    let x = Random.float 1.0 in
    let y = Random.float 1.0 in
    if x *. x +. y *. y <= 1.0 then
      p := !p + 1
  done;

  let pi = 4.0 *. float !p /. float n in
  Printf.printf "%f\n" pi

let sieve () =
  let max = read_int () in
  let prime = Array.make (max + 1) true in

  prime.(0) <- false;
  prime.(1) <- false;

  let limit = int_of_float @@ sqrt @@ float max in

  for n = 2 to limit do
    if prime.(n) then
      let m = ref (n * n) in

      while !m < max + 1 do
        prime.(!m) <- false;
        m := !m + n
      done
  done;

  for n = 2 to max do
    if prime.(n) then Printf.printf "%d\n" n
  done

(* page 47 *)