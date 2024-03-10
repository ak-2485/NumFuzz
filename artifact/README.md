# NumFuzz Artifact

This is the artifact for NumFuzz ("Numerical Fuzz"), a prototype implementation of the type system and floating-point error analysis tool described in the paper *A Type System for Numerical Error Analysis*.  

This artifact supports the following claim made in the Evaluation section (Section 6.2) of the paper: *compared to state-of-the-art tools that soundly and automatically bound floating-point errors, NumFuzz provides practically useful---and often better---error bounds in significantly less time*.

This artifact supports this claim by automatically generating floating-point error bounds using NumFuzz, FPTaylor, and Gappa for the 10 benchmark problems listed in Table 3 of Section 6.2, and by reporting the timing for each tool on each benchmark.

We can't guarantee that this artifact will produce the *exact* timing values reported in Table 3 of Section 6.2 for each of the tools on all of the benchmarks. However, this artifact should support the claim that NumFuzz generates floating-point error bounds in at least an order of magnitude less time than both FPTaylor and Gappa on all of the benchmarks.

There is a small error in Table 3 of Section 6.2 that will be revised in the final version of the paper: the error bound on the benchmark `sqrt_add` for NumFuzz should be `9.99200722163e-16`. Observe that FPTaylor still outperforms NumFuzz on this example.

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

```
tar -xzf mpfr-4.1.1.tar.gz
```
Then, in the MPFR directory `mpfr-4.1.1` install MPFR via
```
./configure --prefix=/app/local/ && ./remake && ./remake install
```

You should now be ready to install Gappa. In the directory `NumFuzz/examples/Gappa`, extract Gappa to the directory `gappa-1.4.2` by running the command  
```
tar -xzf gappa-1.4.2.tar.gz
```
Then, in the directory `gappa-1.4.2` run 
```
./configure --prefix=/app/local/ && ./remake && ./remake install
```
That's it!

More details about Gappa can be found in the [Gappa GitLab](https://gappa.gitlabpages.inria.fr/).

# Running Benchmarks

To verify the error bounds and check the timings listed in Table 3 of Section 6.2 simply run `make tests` in the top-level `NumFuzz` directory. This will generate the file `tests.txt`. 

To run all benchmarks for each tool individually, you can run `make tests` in the tool directory `examples/TOOLNAME` (e.g., `examples/numfuzz`). This will generate a file `examples/TOOLNAME_tests.txt` (e.g., `examples/numfuzz_tests.txt`).

To run individual benchmarks, use the following commands.
- **FPTaylor**: In the directory `NumFuzz/examples/FPTaylor` run `FPTaylor-0.9.3/fptaylor -c config.cfg BENCHMARK.txt`
- **Gappa**:  In the directory `NumFuzz/examples/Gappa` run `time gappa BENCHMARK.g`
- **NumFuzz**: 	In the directory `NumFuzz/examples/NumFuzz`	run `dune exec -- nfuzz BENCHMARK.fz`

## Reading the output

When you run benchmarks using the methods described above, you'll get output like the following 

```
*** START BENCHMARK: hypot *** 
*** TOOL: NumFuzz *** 
I  [General] : Type of the program: ((![0.5] ℝ) ⊸ ((![0.5] ℝ) ⊸ (M[5.55111512313e-16] ℝ)))
Execution time: 0.000508s
*** END NumFuzz *** 
 
*** BENCHMARK: hypot *** 
*** TOOL: FPTaylor *** 
FPTaylor, version 0.9.3+dev
...
...
...
Processing: hypot
...
-------------------------------------------------------------------------------
Problem: hypot

Optimization lower bounds for error models:
The relative error model (exact): 4.439495e-16 (0x1.ffd6c80e3faep-52) (suboptimality = 14.1%)

Bounds (without rounding): [1.414213e-1, 1.414214e+3]

Relative error (exact): 5.170059e-16 (0x1.2a089914b604dp-51)

Elapsed time: 0.87


*** END FPTAYLOR *** 
 
*** BENCHMARK: hypot *** 
*** TOOL: Gappa *** 
Results:
  |(r - z) / r| in [0, 609354566858790905b-97 {3.84557e-12, 2^(-37.9199)}]
Real time (s): 0.006
*** END GAPPA *** 
*** END BENCHMARK: hypot *** 
```

