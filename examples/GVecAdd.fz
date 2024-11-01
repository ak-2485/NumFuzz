{(x : (num, num)) (y : (num, num))}
`{(a : dnum)}

/*
    Computes "axpy", ax + y where x, y are in R^2.
*/

/* let (x1, x2) = ScaleVec a x; */
let (x1', x2') = x;
let x1 = dmul a x1';
let x2 = dmul a x2';

let (y1, y2) = y;
let u = add x1 y1;
let v = add x2 y2;
(u, v)
