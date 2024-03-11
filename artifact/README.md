# NumFuzz Artifact

This is the artifact for NumFuzz ("Numerical Fuzz"), a prototype implementation of the type system and floating-point error analysis tool described in the paper *A Type System for Numerical Error Analysis*.  

This artifact supports the following claim made in the Evaluation section (Section 6.2) of the paper: **compared to state-of-the-art tools that soundly and automatically bound floating-point errors, NumFuzz provides practically useful---and often better---relative error bounds in at least an order of magnitude less time**.

This artifact supports this claim by automatically generating floating-point error bounds using NumFuzz, FPTaylor, and Gappa for the 10 benchmark problems listed in Table 3 of Section 6.2, and by reporting the timing for each tool on each benchmark.

We can't guarantee that this artifact will produce the *exact* timing values reported in Table 3 of Section 6.2 for each of the tools on all of the benchmarks on every machine. However, this artifact should support the claim that **NumFuzz generates floating-point error bounds at least an order of magnitude faster than both FPTaylor and Gappa on all of the benchmarks**.

### Revisions

There are a few small errors in Table 3 of Section 6.2 that will be revised in the final version of the paper. We list them here by benchmark name. **These small errors do not impact the claims made above**.
- **x_by_xy**: The relative error bound for Gappa should be `1.00e-04`, which is worse than the bound originally reported of `2.22e-12`. Observe the NumFuzz remains the best performer.
- **sqrt_add**: The relative error bound for NumFuzz should be `9.99e-16`. Observe that FPTaylor still outperforms NumFuzz on this benchmark.
- **test02_sum8**: The relative error bound for FPTaylor should be `9.32e-14`, which is worse than the bound originally reported of `4.66e-14`. Observe the NumFuzz remains the best performer.
- **horner5**: The relative error bound for FPTaylor should be `1.61e-01`, which is slightly worse than the bound originally reported of `1.11e-01`. The relative error bound for NumFuzz should be `1.11e-15`, which is slightly *better* than the bound originally reported of `1.33e-01`. Observe the NumFuzz remains the best performer.

# Getting Started

The artifact can be built manually or using the provided docker image; Docker is required in order to use the docker image. The requirements for building manually are listed in the description below.

Both methods require extracting `NumFuzz_source.tar.gz` to the directory `NumFuzz`. This can be done with the command 
```
tar -xzf NumFuzz_source.tar.gz
```

## A. Building the docker image

To build the docker image, first run `docker build -t numfuzz .` in the `NumFuzz` directory

Now, you can enter a TTY using the command `docker run --rm --name numfuzz_tty -it numfuzz` and follow the directions 
for [running the benchmarks](#running-benchmarks).

## B. Building manually

### Requirements
Building NumFuzz manually requires Dune 3.14.0, Ocaml version 4.14.1 with a native compiler, and Menhir version 20220210. There are additional requirements for FPTaylor and Gappa.

#### Installing FPTaylor

FPTaylor requires the [OCaml Num library](https://github.com/ocaml/num) which can be installed via opam using the command 

```
opam install num
```
Now, in the directory `NumFuzz/examples/FPTaylor`, extract FPTaylor to the directory `FPTaylor-9.3.0` by running the command  
```
tar -xzf v0.9.3.tar.gz
```
In the directory `FPTaylor-9.3.0` run
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

To verify the reported relative error bounds and check the timings listed in Table 3 of Section 6.2 (subject to the [revisions](#revisions) mentioned at the beginning of this document) simply run `make tests` in the top-level `NumFuzz` directory. This will generate the file `tests.txt`. 

To run all benchmarks for each tool individually, you can run `make tests` in the tool directory `examples/TOOLNAME` (e.g., `examples/numfuzz`). This will generate a file `examples/TOOLNAME/TOOLNAME_tests.txt` (e.g., `examples/NumFuzz/numfuzz_tests.txt`).

To run individual benchmarks, use the following commands.
- **FPTaylor**: In the directory `NumFuzz/examples/FPTaylor` run `FPTaylor-0.9.3/fptaylor -c config.cfg BENCHMARK.txt`
- **Gappa**:  In the directory `NumFuzz/examples/Gappa` run `time gappa BENCHMARK.g`
- **NumFuzz**: 	In the directory `NumFuzz/examples/NumFuzz`	run `dune exec -- nfuzz BENCHMARK.fz`

## Reading the output

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

In NumFuzz, programs with return type `M[u] ℝ` are guaranteed to have a relative error bound of at most `u`. Above, the type of the program is 

``` ((![0.5] ℝ) ⊸ ((![0.5] ℝ) ⊸ (M[5.55111512313e-16] ℝ))) ```

The return type of the program is `M[5.55111512313e-16] ℝ`, and
so the relative error bound is `5.55111512313e-16`. 

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