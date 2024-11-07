{(a : (num, num))}
`{(z : dnum)}

let (a_1, a_2) = a;

let y_1 = a_2;
let x_1 = dmul z y_1;
let y_2 = add a_1 x_1;
y_2