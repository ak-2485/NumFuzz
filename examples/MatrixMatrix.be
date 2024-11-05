{(B : ((num, num), (num, num)))}
`{(A : ((dnum, dnum), (dnum, dnum)))}

/* 
    Computes the product AB where A and B are 2x2 matrices
    and A is a diagonal matrix.
*/

let (B1, B2) = B;
let (B11, B12) = B1;
let (B21, B22) = B2;

dlet (A1, A2) = A;
dlet (A11, A12) = A1;
dlet (A21, A22) = A2;

let C11 = dmul A11 B11;
let C12 = dmul A11 B12;
let C21 = dmul A22 B21;
let C22 = dmul A22 B22;
((C11, C12), (C21, C22))
