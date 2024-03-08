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
  let ref_0 = (var_a20 *$ var_x) in
  let ref_1 = (ref_0 +$ var_a19) in
  let ref_2 = (ref_1 *$ var_x) in
  let ref_3 = (ref_2 +$ var_a18) in
  let ref_4 = (ref_3 *$ var_x) in
  let ref_5 = (ref_4 +$ var_a17) in
  let ref_6 = (ref_5 *$ var_x) in
  let ref_7 = (ref_6 +$ var_a16) in
  let ref_8 = (ref_7 *$ var_x) in
  let ref_9 = (ref_8 +$ var_a15) in
  let ref_10 = (ref_9 *$ var_x) in
  let ref_11 = (ref_10 +$ var_a14) in
  let ref_12 = (ref_11 *$ var_x) in
  let ref_13 = (ref_12 +$ var_a13) in
  let ref_14 = (ref_13 *$ var_x) in
  let ref_15 = (ref_14 +$ var_a12) in
  let ref_16 = (ref_15 *$ var_x) in
  let ref_17 = (ref_16 +$ var_a11) in
  let ref_18 = (ref_17 *$ var_x) in
  let ref_19 = (ref_18 +$ var_a10) in
  let ref_20 = (ref_19 *$ var_x) in
  let ref_21 = (ref_20 +$ var_a9) in
  let ref_22 = (ref_21 *$ var_x) in
  let ref_23 = (ref_22 +$ var_a8) in
  let ref_24 = (ref_23 *$ var_x) in
  let ref_25 = (ref_24 +$ var_a7) in
  let ref_26 = (ref_25 *$ var_x) in
  let ref_27 = (ref_26 +$ var_a6) in
  let ref_28 = (ref_27 *$ var_x) in
  let ref_29 = (ref_28 +$ var_a5) in
  let ref_30 = (ref_29 *$ var_x) in
  let ref_31 = (ref_30 +$ var_a4) in
  let ref_32 = (ref_31 *$ var_x) in
  let ref_33 = (ref_32 +$ var_a3) in
  let ref_34 = (ref_33 *$ var_x) in
  let ref_35 = (ref_34 +$ var_a2) in
  let ref_36 = (ref_35 *$ var_x) in
  let ref_37 = (ref_36 +$ var_a1) in
  let ref_38 = (ref_37 *$ var_x) in
  let ref_39 = (ref_38 +$ var_a0) in
  ((abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_1 +$ {low = 0.00000000000000000000e+00; high = 0.00000000000000000000e+00}))))))))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_3 +$ {low = -2.22044604925031357389e-16; high = 2.22044604925031357389e-16})))))))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_5 +$ {low = -6.66133814775094022862e-16; high = 6.66133814775094022862e-16}))))))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_7 +$ {low = -1.55431223447521935381e-15; high = 1.55431223447521935381e-15})))))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_9 +$ {low = -2.44249065417534478336e-15; high = 2.44249065417534478336e-15}))))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_11 +$ {low = -3.33066907387547001570e-15; high = 3.33066907387547001570e-15})))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_13 +$ {low = -4.21884749357559564247e-15; high = 4.21884749357559564247e-15}))))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_15 +$ {low = -5.99520433297584610715e-15; high = 5.99520433297584610715e-15})))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_17 +$ {low = -7.77156117237609736069e-15; high = 7.77156117237609736069e-15}))))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_19 +$ {low = -9.54791801177634782537e-15; high = 9.54791801177634782537e-15})))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_21 +$ {low = -1.13242748511765982900e-14; high = 1.13242748511765982900e-14}))))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_23 +$ {low = -1.31006316905768487547e-14; high = 1.31006316905768487547e-14})))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_25 +$ {low = -1.48769885299771007971e-14; high = 1.48769885299771007971e-14}))))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_27 +$ {low = -1.66533453693773512618e-14; high = 1.66533453693773512618e-14})))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_29 +$ {low = -1.84297022087776017265e-14; high = 1.84297022087776017265e-14}))))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_31 +$ {low = -2.19824158875781026558e-14; high = 2.19824158875781026558e-14})))))))) +$ (abs_I((var_x *$ (var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_33 +$ {low = -2.55351295663786035852e-14; high = 2.55351295663786035852e-14}))))))) +$ (abs_I((var_x *$ (var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_35 +$ {low = -2.90878432451791076700e-14; high = 2.90878432451791076700e-14})))))) +$ (abs_I((var_x *$ ({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_37 +$ {low = -3.26405569239796085993e-14; high = 3.26405569239796085993e-14}))))) +$ abs_I(({low = 2.00000000000000000000e+00; high = 2.00000000000000000000e+00} *$ floor_power2_I((ref_39 +$ {low = -3.61932706027801095287e-14; high = 3.61932706027801095287e-14}))))))))))))))))))))))) /$ abs_I(ref_39))


let _ =
  let x_tol = size_max_X start_interval *. 0.000000e+00 +. 1.000000e-02 in
  let upper_bound, lower_bound, c = Opt0.opt f_X start_interval x_tol (1.000000e-02) (1.000000e-02) (1000000) in
  let () = Printf.printf "iter_max = %d\n" c in
  let () = Printf.printf "max = %0.20e\n" upper_bound in
  let () = Printf.printf "lower_max = %0.20e\n" lower_bound in
  let () = Printf.printf "iter_min = 0\n" in
  let () = Printf.printf "min = 0\n" in
  let () = Printf.printf "lower_min = 0\n" in
  flush stdout
