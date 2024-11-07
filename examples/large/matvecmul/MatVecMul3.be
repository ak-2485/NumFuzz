{(M : ((num, num, num), (num, num, num), (num, num, num)))}
`{(v : (dnum, dnum, dnum))}

let (M_1, Y_1) = M;
let (M_2, M_3) = Y_1;

let (M_1_1, Y_1_1) = M_1;
let (M_1_2, M_1_3) = Y_1_1;
let (M_2_1, Y_2_1) = M_2;
let (M_2_2, M_2_3) = Y_2_1;
let (M_3_1, Y_3_1) = M_3;
let (M_3_2, M_3_3) = Y_3_1;

dlet (v_1, w_1) = v;
dlet (v_2, v_3) = w_1;

let x_1_1 = dmul v_1 M_1_1;
let x_1_2 = dmul v_2 M_1_2;
let x_1_3 = dmul v_3 M_1_3;
let u_1_1 = x_1_1;
let u_1_2 = add u_1_1 x_1_2;
let u_1_3 = add u_1_2 x_1_3;

let x_2_1 = dmul v_1 M_2_1;
let x_2_2 = dmul v_2 M_2_2;
let x_2_3 = dmul v_3 M_2_3;
let u_2_1 = x_2_1;
let u_2_2 = add u_2_1 x_2_2;
let u_2_3 = add u_2_2 x_2_3;

let x_3_1 = dmul v_1 M_3_1;
let x_3_2 = dmul v_2 M_3_2;
let x_3_3 = dmul v_3 M_3_3;
let u_3_1 = x_3_1;
let u_3_2 = add u_3_1 x_3_2;
let u_3_3 = add u_3_2 x_3_3;

(u_1_3, u_2_3, u_3_3)