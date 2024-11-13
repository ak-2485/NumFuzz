{(M : ((num, num, num, num, num), (num, num, num, num, num), (num, num, num, num, num), (num, num, num, num, num), (num, num, num, num, num)))}
`{(v : (dnum, dnum, dnum, dnum, dnum))}

let (M_1, Y_1) = M;
let (M_2, Y_2) = Y_1;
let (M_3, Y_3) = Y_2;
let (M_4, M_5) = Y_3;

let (M_1_1, Y_1_1) = M_1;
let (M_1_2, Y_1_2) = Y_1_1;
let (M_1_3, Y_1_3) = Y_1_2;
let (M_1_4, M_1_5) = Y_1_3;
let (M_2_1, Y_2_1) = M_2;
let (M_2_2, Y_2_2) = Y_2_1;
let (M_2_3, Y_2_3) = Y_2_2;
let (M_2_4, M_2_5) = Y_2_3;
let (M_3_1, Y_3_1) = M_3;
let (M_3_2, Y_3_2) = Y_3_1;
let (M_3_3, Y_3_3) = Y_3_2;
let (M_3_4, M_3_5) = Y_3_3;
let (M_4_1, Y_4_1) = M_4;
let (M_4_2, Y_4_2) = Y_4_1;
let (M_4_3, Y_4_3) = Y_4_2;
let (M_4_4, M_4_5) = Y_4_3;
let (M_5_1, Y_5_1) = M_5;
let (M_5_2, Y_5_2) = Y_5_1;
let (M_5_3, Y_5_3) = Y_5_2;
let (M_5_4, M_5_5) = Y_5_3;

dlet (v_1, w_1) = v;
dlet (v_2, w_2) = w_1;
dlet (v_3, w_3) = w_2;
dlet (v_4, v_5) = w_3;

let x_1_1 = dmul v_1 M_1_1;
let x_1_2 = dmul v_2 M_1_2;
let x_1_3 = dmul v_3 M_1_3;
let x_1_4 = dmul v_4 M_1_4;
let x_1_5 = dmul v_5 M_1_5;
let u_1_1 = x_1_1;
let u_1_2 = add u_1_1 x_1_2;
let u_1_3 = add u_1_2 x_1_3;
let u_1_4 = add u_1_3 x_1_4;
let u_1_5 = add u_1_4 x_1_5;

let x_2_1 = dmul v_1 M_2_1;
let x_2_2 = dmul v_2 M_2_2;
let x_2_3 = dmul v_3 M_2_3;
let x_2_4 = dmul v_4 M_2_4;
let x_2_5 = dmul v_5 M_2_5;
let u_2_1 = x_2_1;
let u_2_2 = add u_2_1 x_2_2;
let u_2_3 = add u_2_2 x_2_3;
let u_2_4 = add u_2_3 x_2_4;
let u_2_5 = add u_2_4 x_2_5;

let x_3_1 = dmul v_1 M_3_1;
let x_3_2 = dmul v_2 M_3_2;
let x_3_3 = dmul v_3 M_3_3;
let x_3_4 = dmul v_4 M_3_4;
let x_3_5 = dmul v_5 M_3_5;
let u_3_1 = x_3_1;
let u_3_2 = add u_3_1 x_3_2;
let u_3_3 = add u_3_2 x_3_3;
let u_3_4 = add u_3_3 x_3_4;
let u_3_5 = add u_3_4 x_3_5;

let x_4_1 = dmul v_1 M_4_1;
let x_4_2 = dmul v_2 M_4_2;
let x_4_3 = dmul v_3 M_4_3;
let x_4_4 = dmul v_4 M_4_4;
let x_4_5 = dmul v_5 M_4_5;
let u_4_1 = x_4_1;
let u_4_2 = add u_4_1 x_4_2;
let u_4_3 = add u_4_2 x_4_3;
let u_4_4 = add u_4_3 x_4_4;
let u_4_5 = add u_4_4 x_4_5;

let x_5_1 = dmul v_1 M_5_1;
let x_5_2 = dmul v_2 M_5_2;
let x_5_3 = dmul v_3 M_5_3;
let x_5_4 = dmul v_4 M_5_4;
let x_5_5 = dmul v_5 M_5_5;
let u_5_1 = x_5_1;
let u_5_2 = add u_5_1 x_5_2;
let u_5_3 = add u_5_2 x_5_3;
let u_5_4 = add u_5_3 x_5_4;
let u_5_5 = add u_5_4 x_5_5;

(u_1_5, u_2_5, u_3_5, u_4_5, u_5_5)