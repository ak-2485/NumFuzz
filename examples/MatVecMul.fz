{(M : ((num, num), (num, num)))}
`{(v : (dnum, dnum))}

/*
    Computes M * v, where M is a 2x2 matrix and 
    v is a vector in R^2.
*/

let (M1, M2) = M;
let (M11, M12) = M1;
let (M21, M22) = M2;
dlet (v1, v2) = v;

/* let u1 = InnerProduct M1 v */ 
let x1 = dmul v1 M11;
let x2 = dmul v2 M12;
let u1 = add x1 x2;

/* let u2 = InnerProduct M2 v */ 
let y1 = dmul v1 M21;
let y2 = dmul v2 M22;
let u2 = add y1 y2;

(u1, u2)
