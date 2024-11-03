{(a0 : num) (a1 : num) (a2 : num) (a3 : num) (a4 : num)}
`{(z : dnum)}

/*
    Computes a0 + a1 * z + a2 * z^2 + a3 * z^3 + a4 * z^4.
*/

let x1 = dmul z a4;
let y1 = add a3 x1;
let x2 = dmul z y1;
let y2 = add a2 x2;
let x3 = dmul z y2;
let y3 = add a1 x3;
let x4 = dmul z y3;
add a0 x4