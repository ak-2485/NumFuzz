{(a : (num, num, num))}
`{(z : dnum)}

let (a_1, b_1) = a;
let (a_2, a_3) = b_1;

let x_1_1 = a_1;
let x_1 = x_1_1;

let x_2_1 = a_2;
let x_2_2 = dmul z x_2_1;
let x_2 = x_2_2;

let x_3_1 = a_3;
let x_3_2 = dmul z x_3_1;
let x_3_3 = dmul z x_3_2;
let x_3 = x_3_3;

let y_1 = x_1;
let y_2 = add y_1 x_2;
let y_3 = add y_2 x_3;
y_3
