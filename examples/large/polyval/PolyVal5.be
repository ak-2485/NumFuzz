{(a : (num, num, num, num, num, num))}
`{(z : dnum)}

let (a_1, b_1) = a;
let (a_2, b_2) = b_1;
let (a_3, b_3) = b_2;
let (a_4, b_4) = b_3;
let (a_5, a_6) = b_4;

let x_1_1 = a_1;
let x_1 = x_1_1;

let x_2_1 = a_2;
let x_2_2 = dmul z x_2_1;
let x_2 = x_2_2;

let x_3_1 = a_3;
let x_3_2 = dmul z x_3_1;
let x_3_3 = dmul z x_3_2;
let x_3 = x_3_3;

let x_4_1 = a_4;
let x_4_2 = dmul z x_4_1;
let x_4_3 = dmul z x_4_2;
let x_4_4 = dmul z x_4_3;
let x_4 = x_4_4;

let x_5_1 = a_5;
let x_5_2 = dmul z x_5_1;
let x_5_3 = dmul z x_5_2;
let x_5_4 = dmul z x_5_3;
let x_5_5 = dmul z x_5_4;
let x_5 = x_5_5;

let x_6_1 = a_6;
let x_6_2 = dmul z x_6_1;
let x_6_3 = dmul z x_6_2;
let x_6_4 = dmul z x_6_3;
let x_6_5 = dmul z x_6_4;
let x_6_6 = dmul z x_6_5;
let x_6 = x_6_6;

let y_1 = x_1;
let y_2 = add y_1 x_2;
let y_3 = add y_2 x_3;
let y_4 = add y_3 x_4;
let y_5 = add y_4 x_5;
let y_6 = add y_5 x_6;
y_6
