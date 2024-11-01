{(A : ((num, num), (num, num))) (u : (num, num))}
`{(a : dnum) (b : dnum) (v : (dnum, dnum))}

/*
    Computes "gemv", aAv + bu where A is a 2x2 matrix,
    u, v are vectors in R^2, and a, b are scalars.
*/

let (A1, A2) = A;
let (A11, A12) = A1;
let (A21, A22) = A2;
dlet (v1, v2) = v;
let (u1, u2) = u;

/* let x = MatVecMul A v */
let v1A11 = dmul v1 A11;
let v2A12 = dmul v2 A12;
let x1 = add v1A11 v2A12;

let v1A21 = dmul v1 A21;
let v2A22 = dmul v2 A22;
let x2 = add v1A21 v2A22;

/* let y = ScaleVec b u */
let y1 = dmul b u1;
let y2 = dmul b u2;

/* GVecAdd a x y */
let x1' = dmul a x1;
let x2' = dmul a x2;

let x = add x1' y1;
let y = add x2' y2;
(x, y)
