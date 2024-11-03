{(a : (num, num))} 
`{(z : dnum)}

/*
    Computes a0 + a1 * z.
*/

let (a0, a1) = a;

let y = dmul z a1;
add a0 y