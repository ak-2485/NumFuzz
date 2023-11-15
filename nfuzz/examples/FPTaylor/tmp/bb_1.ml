open Interval
open Opt_func


let start_interval = Array.init 4 (function
| 0 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+03}
| 1 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+03}
| 2 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+03}
| 3 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+03}
| _ -> failwith "Out of boundaries"
)

let f_X input_array = 
  let var_x = input_array.(0) in
  let var_a2 = input_array.(1) in
  let var_a1 = input_array.(2) in
  let var_a0 = input_array.(3) in
  ((((var_a2 *$ var_x) +$ var_a1) *$ var_x) +$ var_a0)


let _ =
  let x_tol = size_max_X start_interval *. 0.000000e+00 +. 1.000000e-02 in
  let upper_bound, lower_bound, c = Opt0.opt f_X start_interval x_tol (1.000000e-02) (0.000000e+00) (1000000) in
  let () = Printf.printf "iter_max = %d\n" c in
  let () = Printf.printf "max = %0.20e\n" upper_bound in
  let () = Printf.printf "lower_max = %0.20e\n" lower_bound in
  let upper_bound, lower_bound, c = Opt0.opt (fun x -> ~-$ (f_X x)) start_interval x_tol (1.000000e-02) (0.000000e+00) (1000000) in
  let () = Printf.printf "iter_min = %d\n" c in
  let () = Printf.printf "min = %0.20e\n" (-. upper_bound) in
  let () = Printf.printf "lower_min = %0.20e\n" (-. lower_bound) in
  flush stdout
