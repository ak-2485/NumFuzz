{(v : (num, num, num, num, num))}
`{(w : (dnum, dnum, dnum, dnum, dnum))}

let (v_1, v'_1) = v;
let (v_2, v'_2) = v'_1;
let (v_3, v'_3) = v'_2;
let (v_4, v_5) = v'_3;

dlet (w_1, w'_1) = w;
dlet (w_2, w'_2) = w'_1;
dlet (w_3, w'_3) = w'_2;
dlet (w_4, w_5) = w'_3;

let x_1 = dmul w_1 v_1;
let x_2 = dmul w_2 v_2;
let x_3 = dmul w_3 v_3;
let x_4 = dmul w_4 v_4;
let x_5 = dmul w_5 v_5;
let y_1 = x_1;
let y_2 = add y_1 x_2;
let y_3 = add y_2 x_3;
let y_4 = add y_3 x_4;
let y_5 = add y_4 x_5;
y_5
