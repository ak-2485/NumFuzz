{(A : ((num, num), (num, num))) (b : (num, num))}
`{}

/* 
    Doolittle's method returns an LU decomposition as follows:
     [ A11 A12 ] = [ 1   0 ] * [ U11 U12 ] = [U11         U12            ]
     [ A21 A22 ]   [ L21 1 ]   [ 0   U21 ]   [U11 * L21   U12 * L21 + U21]
    Thus, U11 = A11, U12 = A12, L21 = div A21 A11, and U21 = A22 - A12 * L21.
    We return (L21, U12)
*/

let (A1, A2) = A;
let (A11, A12) = A1;
let (A21, A22) = A2;
let (b1, b2) = b;

let L21_or_err = div A21 A11; 
case L21_or_err of {
  inl (L21) => 
    dlet dL21 = !L21;
    let temp = dmul dL21 A12;
    let U21 = sub A22 temp;
    inl () (dL21, U21)
  | inr (err) => inr (dnum, num) err
}