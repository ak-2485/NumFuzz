{(a : (num, num, num, num, num))}
`{(z : dnum)}

let (a_1, b_1) = a;
let (a_2, b_2) = b_1;
let (a_3, b_3) = b_2;
let (a_4, a_5) = b_3;

let y_1 = a_5;
let x_1 = dmul z y_1;
let y_2 = add a_4 x_1;
let x_2 = dmul z y_2;
let y_3 = add a_3 x_2;
let x_3 = dmul z y_3;
let y_4 = add a_2 x_3;
let x_4 = dmul z y_4;
let y_5 = add a_1 x_4;
y_5