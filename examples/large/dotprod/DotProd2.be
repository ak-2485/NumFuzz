{(v : (num, num))}
`{(w : (dnum, dnum))}

let (v_1, v_2) = v;

dlet (w_1, w_2) = w;

let x_1 = dmul w_1 v_1;
let x_2 = dmul w_2 v_2;
let y_1 = x_1;
let y_2 = add y_1 x_2;
y_2
