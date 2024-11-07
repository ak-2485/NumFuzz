{(A : ((num, num, num), (num, (num, num)), (num, (num, num)))) (b : (num, num, num))}
`{}

/* 
    Suppose Ax = b where A is a 3x3 matrix as follows:
    A = [A11  0   0 ]
        [A21 A22  0 ]
        [ 0  A32 A33] 
    and b is a vector in R^3. We solve for x3.
*/

let (A1, A') = A;
let (A2, A3) = A';
let (A11, A1') = A1;
let (A12, A13) = A1';
let (A21, A2') = A2;
let (A22, A23) = A2';
let (A31, A3') = A3;
let (A32, A33) = A3';
let (b1, b') = b;
let (b2, b3) = b';

let x1_or_none = div b1 A11; 
case x1_or_none of {
  inl (x1) => 
    let x2_1 = mul A21 x1;
    let x2_2 = sub b2 x2_1;
    let x2_or_none = div x2_2 A22;
    case x2_or_none of {
      inl (x2) =>
        let x3_1 = mul A32 x2;
        let x3_2 = sub b3 x3_1;
        let x3_or_none = div x3_2 A33;
        x3_or_none
    | inr (none) => inr num none
    }
| inr (none) => inr num none
}
