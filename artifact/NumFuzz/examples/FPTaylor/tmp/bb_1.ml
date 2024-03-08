open Interval
open Opt_func


let start_interval = Array.init 22 (function
| 0 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 1 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 2 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 3 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 4 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 5 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 6 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 7 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 8 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 9 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 10 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 11 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 12 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 13 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 14 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 15 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 16 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 17 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 18 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 19 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 20 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| 21 -> {low = 9.99999999999999916733e-02; high = 1.00000000000000000000e+00}
| _ -> failwith "Out of boundaries"
)

let f_X input_array = 
  let var_x = input_array.(0) in
  let var_a20 = input_array.(1) in
  let var_a19 = input_array.(2) in
  let var_a18 = input_array.(3) in
  let var_a17 = input_array.(4) in
  let var_a16 = input_array.(5) in
  let var_a15 = input_array.(6) in
  let var_a14 = input_array.(7) in
  let var_a13 = input_array.(8) in
  let var_a12 = input_array.(9) in
  let var_a11 = input_array.(10) in
  let var_a10 = input_array.(11) in
  let var_a9 = input_array.(12) in
  let var_a8 = input_array.(13) in
  let var_a7 = input_array.(14) in
  let var_a6 = input_array.(15) in
  let var_a5 = input_array.(16) in
  let var_a4 = input_array.(17) in
  let var_a3 = input_array.(18) in
  let var_a2 = input_array.(19) in
  let var_a1 = input_array.(20) in
  let var_a0 = input_array.(21) in
  ((((((((((((((((((((((((((((((((((((((((var_a20 *$ var_x) +$ var_a19) *$ var_x) +$ var_a18) *$ var_x) +$ var_a17) *$ var_x) +$ var_a16) *$ var_x) +$ var_a15) *$ var_x) +$ var_a14) *$ var_x) +$ var_a13) *$ var_x) +$ var_a12) *$ var_x) +$ var_a11) *$ var_x) +$ var_a10) *$ var_x) +$ var_a9) *$ var_x) +$ var_a8) *$ var_x) +$ var_a7) *$ var_x) +$ var_a6) *$ var_x) +$ var_a5) *$ var_x) +$ var_a4) *$ var_x) +$ var_a3) *$ var_x) +$ var_a2) *$ var_x) +$ var_a1) *$ var_x) +$ var_a0)


let _ =
  let x_tol = size_max_X start_interval *. 0.000000e+00 +. 1.000000e-02 in
  let upper_bound, lower_bound, c = Opt0.opt f_X start_interval x_tol (1.000000e-02) (1.000000e-02) (1000000) in
  let () = Printf.printf "iter_max = %d\n" c in
  let () = Printf.printf "max = %0.20e\n" upper_bound in
  let () = Printf.printf "lower_max = %0.20e\n" lower_bound in
  let upper_bound, lower_bound, c = Opt0.opt (fun x -> ~-$ (f_X x)) start_interval x_tol (1.000000e-02) (1.000000e-02) (1000000) in
  let () = Printf.printf "iter_min = %d\n" c in
  let () = Printf.printf "min = %0.20e\n" (-. upper_bound) in
  let () = Printf.printf "lower_min = %0.20e\n" (-. lower_bound) in
  flush stdout
