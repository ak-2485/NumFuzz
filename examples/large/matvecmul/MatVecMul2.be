{(M : ((num, num), (num, num)))}
`{(v : (dnum, dnum))}

let (M_1, M_2) = M;

let (M_1_1, M_1_2) = M_1;
let (M_2_1, M_2_2) = M_2;

dlet (v_1, v_2) = v;

let x_1_1 = dmul v_1 M_1_1;
let x_1_2 = dmul v_2 M_1_2;
let u_1_1 = x_1_1;
let u_1_2 = add u_1_1 x_1_2;

let x_2_1 = dmul v_1 M_2_1;
let x_2_2 = dmul v_2 M_2_2;
let u_2_1 = x_2_1;
let u_2_2 = add u_2_1 x_2_2;

(u_1_2, u_2_2)