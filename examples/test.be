{(a0 : num) (a1 : num) (a2 : num) (a : num) (b : num) (c : num)}
`{(z : dnum)}


/* let x1 = dmul z a2 ;
let y1 = add a1 x1 ;
let x2 = dmul z y1;
add a0 x2 */

/* let y1 = dmul z a1 ;

let y2' = dmul z a2 ;
let y2 = dmul z y2' ;

let x = add a0 y1 ;
add x y2 */

let div_or_err = div c a;
case div_or_err of {
     inl (y) =>
        let y' = sub y b;
        inl () y'
    | inr (err) => inr num ()
}