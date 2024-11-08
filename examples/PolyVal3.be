{(a : (num, num, num, num))}
`{(z : dnum)}

let (a1, a') = a;
let (a2, a'') = a';
let (a3, a4) = a'';

let x2 = dmul z a2;

let x3' = dmul z a3;
let x3 = dmul z x3';

let x4' = dmul z a4;
let x4'' = dmul z x4';
let x4 = dmul z x4'';

let s1 = add a1 x2;
let s2 = add s1 x3;
add s2 x4
