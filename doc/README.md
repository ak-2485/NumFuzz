# Bean Artifact

This is the artifact for Bean ("Backward Error Analysis").

# Getting Started

### Requirements
Building Bean requires Dune 3.14.0, Ocaml version 4.14.1 with a native compiler, and Menhir version 20220210. 

### Running a Bean program

Run the `InnerProduct` Bean program as follows: `dune exec -- bean examples/InnerProduct.be`. 

In Bean, programs start with a list of input variables which may be *linear* or *discrete*. 
The sole linear input to `InnerProduct` is `v : (num, num)` and the sole discrete input is `u : (dnum, dnum)`.
In other words, `u` and `v` are real vectors in ℝ². 
However, `v` may have backward error while `u` may not.

The output tells us:
```
I  [General] : Type of the program: ℝ
               
I  [General] : Discrete Variables:
               u : (dℝ ⊗ dℝ)
               
I  [General] : Inferred Context:
               v :2. (ℝ ⊗ ℝ)
```
The return type of `InnerProduct` is `ℝ` and the only discrete variable is `u`. 
The inferred context tells us that our input vector `v` has a backward error bound of `2ϵ` where ϵ is machine epsilon.
Note that for vectors and matrices, we report the max element-wise backward error bound produced by Bean. 

# Writing Bean Programs

Bean assumes the interpretation of the numeric type `num` as the set of strictly positive real numbers $\mathbb{R}^{>0}$ with the relative precision (RP) metric given in ________ of the paper. Under this assumption, Bean can generate sound relative error bounds using the analysis described by Olver [44]. 

Soundness of the error bounds inferred by Bean is guaranteed by ________ of the paper and the instantiation of the language described in ________. 

## Syntax

The syntax of Bean, detailed in ________ of the paper, is as follows. 
Some important features are discussed below. 
Note that term constructors and eliminators are restricted to values (including variables).

```
T ::=                                           TYPES
      ()                                        single-valued unit type
      num                                       numeric type; only used for linear variables
      dnum                                      discrete numeric type; only used for discrete variables
      bool                                      boolean
      (T, T)                                    tensor product
      T + T                                     sum

v, w ::=                                        VALUES
      ()                                        value of unit type
      (v, w)                                    tensor pairs
      inl v                                     injection into sum
      inr v                                     injection into sum

e, f ::=                                        EXPRESSIONS
      v                                         values
      let (x, y) = v; e                         linear tensor destructor
      let (x, y) = v; e                         discrete tensor destructor
      case v {inl x => e | inr x => f}          case analysis
      let x = v; e                              monadic sequencing
      op a b                                    op in (add, mul, sub, div, dmul), a and b are variables
```
- **Sequencing**: All computations are explicitly sequenced by let-
bindings using the syntax `let x = v; e`. 

- **Pairs**: The syntax for tensor pairs $− \otimes −$ is `(-, -)` and the syntax for the type is also `(-, -)`. 

- **Linear and discrete inputs**: At the beginning of our programs, we write `{(ID1 : Type1) (ID2 : Type)}` to denote the 
context of linear variables. Next, we write ``{(ID3 : DType) (ID4 : DType)}` to denote the context of discrete variables.
All variable names must be distinct and if one context is empty, you must still include the empty braces `{}` or ``{}`.

- **Primitive Operations**: The type signature and name of each primitive operation is given below. 
    
    1. Addition `add : num -> num -> num`
    2. Multiplication `mult : num -> num -> num`
    3. Division `div : num -> num -> num + err`
    4. Subtraction `sub : num -> num -> num`
    5. Discrete multiplication `dmul : dnum -> num -> num`

As usual, the `bool` type is constructed from the sum of units: `bool = () + ()`.
