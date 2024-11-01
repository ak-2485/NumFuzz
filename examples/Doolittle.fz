{(A : ((num, num), (num, num)))}
`{}

/* 
    Doolittle's method returns an LU decomposition as follows:
     [ A11 A12 ] = [ 1   0 ] * [ U11 U12 ]
     [ A21 A22 ]   [ L21 1 ]   [ 0   U21 ]
    Thus, U11 = A11, U12 = A12, and div A21 A11. 
    The following program calculates U21. 
*/

let (A1, A2) = A;
let (A11, A12) = A1;
let (A21, A22) = A2;

let L21' = div A21 A11;
case L21' of {
  inl (L21) => 
    let x = mul L21 A12;
    let U21 = sub A22 x;
    inl () U21
| inr (none) => inr num ()
}