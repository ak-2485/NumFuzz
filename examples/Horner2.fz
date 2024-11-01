{(a : (num, (num, num)))}
`{(z : dnum)}

/*
    Computes a0 + a1 * z + a2 * z^2.
*/

let (a0, a') = a;
let (a1, a2) = a';

let x = dmul z a2;
let y = add a1 x;
let y1 = dmul z y;
add a0 y1