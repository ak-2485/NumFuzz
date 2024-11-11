{(a : (num,num,num,num,num))} `{(z:dnum)}

let (s1,b1) = a;
let (s2,b2) = b1;
let (s3,b3) = b2;
let (s4,s5) = b3;

let x1 = dmul z s5;
let x2 = dmul z x1;

let x3 = add x2 s4;
let x4 = dmul z x3;
let x5 = dmul z x4;

let x6 = add x5 s3;
let x7 = dmul z x6;
let x8 = dmul z x7;

let x9  = add x8 s2;
let x10 = dmul z x9; 
let x11 = dmul z x10;

let x12 = add x11 s1;
dmul z x12

