{(a : (num, num, num, num))}
`{(z : dnum)}

let (a_1, b_1) = a;
let (a_2, b_2) = b_1;
let (a_3, a_4) = b_2;

let y_1 = a_4;
let x_1 = dmul z y_1;
let y_2 = add a_3 x_1;
let x_2 = dmul z y_2;
let y_3 = add a_2 x_2;
let x_3 = dmul z y_3;
let y_4 = add a_1 x_3;
y_4