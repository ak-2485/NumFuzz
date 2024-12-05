# NumFuzz Artifact

This is the artifact for NumFuzz ("Numerical Fuzz"), a prototype implementation of the type system and floating-point error analysis tool described in the paper *Numerical Fuzz: A Type System for Rounding Error Analysis*.  

This artifact supports the following claim made in the Evaluation section (Section 6.2) of the paper: **compared to state-of-the-art tools that soundly and automatically bound floating-point errors, NumFuzz provides practically useful---and usually better---relative error bounds in at least an order of magnitude less time**.

This artifact supports this claim by automatically generating floating-point error bounds using NumFuzz, FPTaylor, and Gappa for the 17 benchmark problems listed in Table 1 of Section 6.2, and by reporting the timing for each tool on each benchmark.

We can't guarantee that this artifact will, on every machine, produce the *exact* timing values reported in Table 1 of Section 6.2 for each of the tools on all of the benchmarks. However, this artifact should support the claim that **NumFuzz generates floating-point error bounds at least an order of magnitude faster than FPTaylor and Gappa on most benchmarks**.

This artifact can also be used to reproduce the NumFuzz results reported in Tables 2 & 3 of Section 6.2.  

# Getting Started

The artifact can be built manually or using the provided docker image; Docker is required to use the docker image. The requirements for building manually are listed in the description below and have been tested on Ubuntu 20.04, Alpine 3.17, and macOS 14.3.

Both methods require extracting `NumFuzz_source.tar.gz` to the directory `NumFuzz`. This can be done with the command 
```
tar -xzf NumFuzz_source.tar.gz
```

## A. Building the docker image

To build the docker image, first run `docker build -t numfuzz .` in the `NumFuzz` directory.

After the docker image builds you can enter a TTY using the command 
```
docker run --rm --name numfuzz_tty -it numfuzz
``` 
and follow the directions for [running the benchmarks](#running-benchmarks) detailed below.

## B. Building manually

### Requirements
Building NumFuzz manually requires Dune 3.14.0, Ocaml version 4.14.1 with a native compiler, and Menhir version 20220210. There are additional requirements for FPTaylor and Gappa.

#### Installing FPTaylor

FPTaylor requires the [OCaml Num library](https://github.com/ocaml/num) which can be installed via opam using the command 

```
opam install num
```
Now, in the directory `NumFuzz/examples/FPTaylor`, extract FPTaylor to the directory `FPTaylor-0.9.4` by running the command  
```
tar -xzf v0.9.4.tar.gz
```
In the directory `FPTaylor-0.9.4` run
```
make all
```
That's it! 

Further details and instructions for FPTaylor can be found [in the GitHub repository](https://github.com/soarlab/FPTaylor).

#### Installing Gappa 

Gappa requires

- [GMP](https://gmplib.org/)--Install via  `sudo apt-get install libgmp3-dev`.
- [Boost](https://www.boost.org/)---Install via `sudo apt-get install libboost-all-dev`.
- [MPFR](https://www.mpfr.org/) version 4.1.1---Install with the steps below.

In the directory `NumFuzz/examples/Gappa` extract `mpfr-4.1.1.tar.gz` to the directory `mpfr-4.1.1` via the command

``` tar -xzf mpfr-4.1.1.tar.gz ```

Then, in the MPFR directory `mpfr-4.1.1` install MPFR via

```
./configure --prefix=/app/local/ && ./remake && ./remake install 
```

You should now be ready to install Gappa. In the directory `NumFuzz/examples/Gappa`, extract Gappa to the directory `gappa-1.4.2` by running the command  

``` tar -xzf gappa-1.4.2.tar.gz ```
Then, in the directory `gappa-1.4.2` run 

```
./configure --prefix=/app/local/ && ./remake && ./remake install
```
That's it!

More details about Gappa can be found in the [Gappa GitLab](https://gappa.gitlabpages.inria.fr/).

# Running Benchmarks

## Small Benchmarks (Table 1)

To verify the reported relative error bounds and check the timings listed in Table 1 of Section 6.2, simply run `make tests` in the top-level `NumFuzz` directory. This will generate the files `tests.txt` and `table.txt`. The file
`tests.txt` contains all of the output from each tool on each benchmark. The file `table.txt` contains a table generated from the `tests.txt` file; the script for generating the table is `out_table.sh`. The data in the table in `table.txt` should match the data in Table 1 of Section 6.2 of the paper.

To run all benchmarks for each tool individually, you can run `make tests` in the tool directory `examples/TOOLNAME` (e.g., `examples/numfuzz`). This will generate a file `examples/TOOLNAME/TOOLNAME_tests.txt` (e.g., `examples/NumFuzz/numfuzz_tests.txt`).

To run individual benchmarks, use the following commands.
- **FPTaylor**: In the directory `NumFuzz/examples/FPTaylor` run `FPTaylor-0.9.4/fptaylor -c config.cfg BENCHMARK.txt`
- **Gappa**:  In the directory `NumFuzz/examples/Gappa` run `time gappa BENCHMARK.g`
- **NumFuzz**: 	In the directory `NumFuzz/examples/NumFuzz`	run `dune exec -- nfuzz BENCHMARK.fz`

## Reading the output

The easiest way to read the output is using the generated table in the `table.txt` file. Details for reading the raw output from each tool follow. 

When you run benchmarks using the methods described above, you'll get output like the following.

```
*** START BENCHMARK: hypot *** 
*** TOOL: NumFuzz *** 
I  [General] : Type of the program: ((![0.5] ℝ) ⊸
                                     ((![0.5] ℝ) ⊸ (M[5.55111512313e-16] ℝ)))
Execution time: 0.000642s
*** END NumFuzz *** 
 
*** BENCHMARK: hypot *** 
*** TOOL: FPTaylor *** 
FPTaylor, version 0.9.3+dev
Loading configuration file: /home/ak2485/Documents/NumFuzz/artifact/NumFuzz/examples/FPTaylor/FPTaylor-0.9.3/default.cfg
Loading configuration file: /home/ak2485/Documents/NumFuzz/artifact/NumFuzz/examples/FPTaylor/config.cfg

Loading: /home/ak2485/Documents/NumFuzz/artifact/NumFuzz/examples/FPTaylor/hypot.txt
Processing: hypot
**WARNING**: Large second-order error: 6.776264e-17 (first-order = 4.492432e-16)
**WARNING**: Try intermediate-opt = true or manually split intervals of input variables.
-------------------------------------------------------------------------------
Problem: hypot

Optimization lower bounds for error models:
The relative error model (exact): 4.439495e-16 (0x1.ffd6c80e3faep-52) (suboptimality = 14.1%)

Bounds (without rounding): [1.414213e-1, 1.414214e+3]

Relative error (exact): 5.170059e-16 (0x1.2a089914b604dp-51)

Elapsed time: 0.75


*** END FPTAYLOR *** 
 
*** BENCHMARK: hypot *** 
*** TOOL: Gappa *** 
Results:
  |(r - z) / r| in [0, 609354566858790905b-97 {3.84557e-12, 2^(-37.9199)}]

real	0m0.005s
user	0m0.005s
sys	0m0.000s
*** END GAPPA *** 
```

Above, we see that the benchmark under consideration is named `hypot`.

### NumFuzz output

In NumFuzz, programs with return type `M[u] ℝ` are guaranteed to have a relative precision of at most `u`. Above, the type of the program is 

``` ((![0.5] ℝ) ⊸ ((![0.5] ℝ) ⊸ (M[5.55111512313e-16] ℝ))) ```

The return type of the program is `M[5.55111512313e-16] ℝ` and
the relative error bound is printed as `I  [General] : Relative error: 5.55111512313e-16`. 

The execution time is printed in seconds as `0.000642s`.

### FPTaylor output

FPTaylor outputs the computed relative error bounds directly. Above, the relative error is 

``` Relative error (exact): 5.170059e-16 (0x1.2a089914b604dp-51)```

The execution time is printed in seconds as `Elapsed time: 0.75`. 

### Gappa output

Gappa's output includes the relative error formula `|(r - z) / r|`   and states the computed interval in which the value of the relative error lies. Above, Gappa produces the output
```
Results:
  |(r - z) / r| in [0, 609354566858790905b-97 {3.84557e-12, 2^(-37.9199)}]
```
which says that the relative error is at most `3.84557e-12`. 

The timing in seconds is `real 0m0.005s` and is produced using the UNIX `time` command.

## Large Benchmarks (Table 2)

To verify the reported error bounds and check the timings listed in Table 2 of Section 6.2, simply run `make tests` in the directory `examples/NumFuzz/arge_benchmarks`. This will generate the file `large_tests.txt`, which contains the NumFuzz output for each large benchmark listed in Table 2 **except** `mat_mul_128`. The benchmark `mat_mul_128` takes approximately 18 minutes on a MacBook with a 1.4 GHz processor and 8 GB of memory. In Linux environments, RAM compression must be enabled using something like zram. The Docker instance does not support this and the process might be killed unless you manually configure it to use an appropriate amount of the host machine's memory.

### Reading the output 
For matrix multiplication, we report
the max element-wise relative error bound produced by NumFuzz. For example, we see below that the return type of the `mat_mul_4` benchmark is a tuple of monadic values of type `(M[1.55431223448e-15] ℝ)`; this means that the error of each element of the tuple (matrix) is at most `1.55431223448e-15`.

```
I  [General] : Type of the program: ((![4.] ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ (ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))))))) ⊸
                                     ((![4.] ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ ((ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))) ⊗ (ℝ ⊗ (ℝ ⊗ (ℝ ⊗ ℝ))))))) ⊸
                                      (((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ (M[1.55431223448e-15] ℝ)))) ⊗ (((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ (M[1.55431223448e-15] ℝ)))) ⊗ (((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ (M[1.55431223448e-15] ℝ)))) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ ((M[1.55431223448e-15] ℝ) ⊗ (M[1.55431223448e-15] ℝ)))))))))

Execution time: 0.001587s
```
Large benchmarks other that do not model matrix multiplication follow the same output format as for small benchmarks.

## Conditional Benchmarks (Table 3)

To verify the reported error bounds and check the timings listed in Table 3 of Section 6.2, simply run `make tests` in the directory `examples/NumFuzz/conditionals`. This will generate the file `conditional_tests.txt`, which contains the NumFuzz output for each conditional benchmark listed in Table 3 of Section 6.2.

## Mixed Precision Benchmarks

The test suite also includes several mixed precision benchmarks, but which
is executed separately from the tests outlined above. 
To run the entire mixed precision suite, type the command `make tests_mixed` in the toplevel directory.
Doing this will produce the file `mixed_test.txt` with the same information as the benchmarks in the previous sections. 
Alternatively, you can use the same command in one of FPTaylor, Gappa, or NumFuzz
directories in the `/examples` directory. This will produce a file `TOOL_mixed_test.txt`
with results only for that particular tool.

# Writing NumFuzz Programs

NumFuzz assumes the interpretation of the numeric type `num` as the set of strictly positive real numbers $\mathbb{R}^{>0}$ with the relative precision (RP) metric given in Definition 2.2 of the paper. Under this assumption, NumFuzz can generate sound relative error bounds using the analysis described by Olver [44]. 

Soundness of the error bounds inferred by NumFuzz is guaranteed by Corollary 4.12 of the paper and the instantiation of the language described in Section 5. This instantiation imposes the following limitations on the programs that can be written in NumFuzz.

1. The only primitive arithmetic operations currently supported in NumFuzz are `+`, `∗`, `/`, and `sqrt`; subtraction and transcendental functions are not supported. The comparison operators `>` and `==` on numeric values are also supported.

2. All constants and variables must be strictly
positive numbers.

3. The precision can be one of binary16, binary32, or binary64 and must be indicated
when using the rounding operator (i.e. `rnd64`). 
The rounding mode is fixed as round towards $+\infty$. 


## Syntax

The syntax of NumFuzz, detailed in Sections 3 and 5 of the paper, is as follows. Some important features are discussed below. Note that term constructors and eliminators are restricted to values (including variables).

```
r ::=                                           SENSITIVITY ANNOTATIONS
      R                                         real-valued sensitivity 
      inf                                       infinite sensitivity

T ::=                                           TYPES
      ()                                        single-valued unit type
      num                                       numeric type
      (T,T)                                     tensor product
      <T,T>                                     cartesian product
      T + T                                     sum
      T -o T                                    1-sensitive function
      MuT                                       monadic type
      !sT                                       co-monadic type

v, w ::=                                        VALUES
      ()                                        value of unit type
      k in R                                    constants (reals > 0)
      (v,w)                                     tensor pairs
      <v,w>                                     cartesian pairs
      inl v                                     injection into sum
      inr v                                     injection into sum
      fun (x:T) {e}                             ordinary function
      [v{r}]                                    co-monadic scaling
      rnd16 v                                   round toward +inf in 16-bit precision
      rnd32 v                                   round toward +inf in 32-bit precision
      rnd64 v                                   round toward +inf in 64-bit precision
      ret v                                     monadic return 
      let x = (rnd v); e                        base monadic sequence  

e, f ::=                                        EXPRESSIONS
      v                                         values
      vw                                        application
      fst v                                     cartesian pair destructor
      snd v                                     cartesian pair destructor
      let (x,y) = v; e                          tensor destructor
      case v {inl x => e | inr x => f}          case analysis
      if v then {e} else {f}			      conditional (case sugar)
      let [x] = v; e                            co-monadic sequencing
      let x = v; e                              monadic sequencing
      x = e; f                                  pure sequencing
      op v                                      op in (+,*,/,sqrt,==,<)
```

- **Sequencing**: All computations are explicitly sequenced by let-
bindings using the syntax `x = v; e`. 

- **Monadic and Metric Destructors**: The destructors for monadic and metric (co-monadic) scaled types are `let x = v ; e` and `let [x] = v ; e`, respectively. 

- **Monadic Constructors**: The constructs `rnd v` and `ret v` lift values of plain type to monadic
type. The rounding mode is fixed as round towards $+\infty$ and the precision is fixed as binary64, so `rnd v` produces an error of at most $2^{-52}$. This value can be used in NumFuzz programs as `eps64_up`; e.g., 
as in `M[eps64_up]num`.

-  **Metric Constructor**: For metric types, the box constructor `[v{r}]` indicates scaling the metric of the type by the sensitivity value `r`; sensitivities are either a positive floating-point number or `inf`, for infinite sensitivity. 

- **Lambda Terms**: The argument of a lambda term requires a type annotation; e.g., the type of the argument `x` in `fun (x:num) {e}` is required. 

- **Pairs**: The syntax for tensor pairs $− \otimes −$ is `(-,-)` and the syntax for the type is also `(-,-)`. For cartesian pairs $− \times −$, the syntax is `(|-,-|)` and the syntax for the type is `<-,->` .

- **Top Level Programs**: For top level programs, we
write `function ID args {v} e` to denote the let-binding `ID = v ; e`, where `v` is a lambda term with arguments `args`.

- **Primitive Operations**: The type signature and name of each primitive operation is given below. 
    
    1. Addition `add: num × num -o num`
    2. Multiplication `mul: num ⊗ num -o num`
    3. Division `div: num ⊗ num -o num`
    4. Square root `sqrt: ![0.5]num -o num`
    5. Greater than `gt: ![inf]num ⊗ ![inf]num -o bool`
    6. Equal `eq: ![inf]num ⊗ ![inf]num -o bool`

As usual, the `bool` type is constructed from the sum of units: `bool = () + ()`.

## FPCore Translation

The NumFuzz artifact provides a tool for transpiling a subset of NumFuzz programs
to [FPCore](https://fpbench.org/spec/fpcore-2.0.html) programs. To run the 
tool on a program, run the command `dune exec -- nfuzz BENCHMARK.fz --translate OUTPUT.fpcore`.
Here, `--translate` is the default flag for program translation. 

- **Transpilation Flags**: 

    1. `--translate`. The default translation mode -- it produces a file with a single FPCore
    and uses of floating point operations in NumFuzz inlined with the usual FPCore
    operations but with a specific rounding context that matches that of NumFuzz.
    2. `--translate-inline`. This translation mode is the same as the default
    but it does not inline calls to floating point operations, meaning that they
    include construction of arrays to match the syntax of NumFuzz floating operators,
    which take pairs as inputs. 
    3. `--translate-literal`. This translation mode produces multiple FPCores
    in a single file. It matches the grammar of FPCore, since FPCores *are* 
    function definitions in this language. More details about this translation mode
    can be found in the transpiler report.
    4. `--translate-binary64`. This translation mode produces programs
    with all rounding annotations removed and with the FPCore toplevel rounding context
    having binary64 precision and rounding towards positive infinity.

- **Limitations**: Currently, the transpiler
is limited to translating programs that use higher order functions only as inputs to fold or map 
functions, such as those in `NumFuzz/examples/NumFuzz/folds`. When definining
other fold or map functions, their signature must be that of a product type
followed by the input function to be applied to each element in the input tuple.

## Examples

### Arithmetic Operations with Rounding

Arithmetic operations that perform rounding can be built from primitive operations. One example is `addfp64`, which adds two numbers and rounds the result to the nearest 64-bit floating point number: 

```ocaml
function addfp_64 (xy: <num, num>) 
{
  s = add xy;
  rnd_64 s
} 
```

Several operations that perform rounding are included in the directory  `NumFuzz/examples/NumFuzz/float_ops`; we list the
64-bit ones here with their type signatures.

1. Addition `addfp64: (num × num) -o M[eps64_up]num`
2. Multiplication `mulfp64: (num ⊗ num) -o M[eps64_up]num`
3. Division `divfp64: (num ⊗ num) -o M[eps64_up]num`
4. Square root `sqrtfp64: ![0.5]num -o M[eps64_up]num`
5. Fused multiply-add `FMA64: num -o num -o num -o M[eps64_up]num`
6. Multiply-add `MA64: num -o num -o num -o M[eps64_up]num`

These operations can be used in NumFuzz programs using an include statement; e.g., 
```
#include PATH/float_ops/addfp64.fz
``` 
where `PATH` is the path relative to the directory of your program. Operations
using 16, and 32 bit floating-point numbers are also available. For example, `addfp32`
is the 32-bit addition operator.  

### A Simple Benchmark

As an example, let us consider the benchmark `one_by_sqrtxx`, where we are tasked with computing a relative error bound for the expression $1/\sqrt{x^2}$. We can use NumFuzz to derive a bound using the following program.

```ocaml
1.  #include "float_ops/mulfp64.fz"
2.  #include "float_ops/divfp64.fz"
3.  #include "float_ops/sqrtfp64.fz"
4.
5.  function one_by_sqrtxx (x : num)
6.  {
7.   sz = mulfp64 (x,x);
8.   let z = sz;
9.   sy = sqrtfp64 ([z{0.5}]);
10.  let y = sy;
11.  divfp64 (1.0,y)
12. }
13.
14. one_by_sqrtxx
```

Let us first look at the overall structure of the program. On lines 1-3, we include the arithmetic operations that we'll be using in our program. On lines 5-14, we recall from the Sytnax description above that the syntax of top-level programs in NumFuzz is `function ID args {v} e`, which is just sugar for the let-binding `ID = v ; e` where `v` is a lambda term with arguments `args`. In the above program, `one_by_sqrtxx` is the `ID` and the only argument `args` is `x` (which *must* be given with its type because it is the argument of a lambda term). The body `v` of the lambda term is given on lines 7-11. Finally, the body `e` of the let-binding is given on line 14.

Now, let us consider the body of the function, given in lines 7-11. First, notice the pattern of sequencing that appears on lines 7 & 8, and then again on lines 9 & 10: the terms `mulfp (x,x)` and `sqrtfp ([z{0.5}])` are *computations* that must be explicitly sequenced using the let-bindings on lines 7 & 9. These let-bindings bind the variables `sz` and `sy` to terms of *monadic* type: `mulfp: (num ⊗ num) -o M[eps64_up]num` and `sqrtfp: ![0.5]num -o M[eps64_up]num` perform a single rounding. To use these values in subsequent computations that take non-monadic numeric types as arguments to (i.e., in `sqrtfp` on line 9 and `divfp` on line 11), we use the monadic destructor on lines 8 & 10.

The last detail that needs clarification is the argument to `sqrtfp` on line 9. Recall the type signature of the operation: `sqrtfp: ![0.5]num -o M[eps64_up]num`. To construct a value of type `![0.5]num` from the variable `z` of plain numeric type, we use the metric (box) constructor `[z{0.5}]`. We know that the `sqrtfp` is half-sensitive in its argument, so we annotate the constructor appropriately; the type-checker uses this annotation to ensure that the computation `sqrtfp ([z{0.5}])` has permission to be half-sensitive in `z`, and that the type of the value `[z{0.5}]` matches the type of the argument to `sqrtfp`.  If we use an incorrect annotation, such as `[z{1.0}]`, the type-checker will produce an error message like the following: `Cannot satisfy constraint 1. <= 0.5`.


