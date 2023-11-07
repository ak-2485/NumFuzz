module M = Mlmpfr

let _ =
  let op = M.make_from_float (~-. 1. /. 3.) in
  Printf.printf "%s\n" (Mlmpfr.get_formatted_str ( op))
