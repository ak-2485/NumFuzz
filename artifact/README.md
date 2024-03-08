# NumFuzz Artifact

This is the artifact for NumFuzz ("Numerical Fuzz"), a prototype implementation of the type system and floating-point error analysis tool described in the paper *A Type System for Numerical Error Analysis*.  

This artifact supports the following claim made in the Evaluation section (Section 6.2) of the paper: *compared to state-of-the-art tools that soundly and automatically bound floating-point errors, NumFuzz provides practically useful---and often better---error bounds in significantly less time*.

This artifact supports this claim by automatically generating floating-point error bounds using NumFuzz, FPTaylor, and Gappa for the 10 benchmark problems listed in Table 3 of Section 6.2, and by reporting the timing for each tool on each benchmark.

We can't guarantee that this artifact will produce the *exact* timing values reported in Table 3 of Section 6.2 for each of the tools on all of the benchmarks. However, this artifact should support the claim that NumFuzz generates floating-point error bounds in at least an order of magnitude less time than both FPTaylor and Gappa on all of the benchmarks.

There is a small error in Table 3 of Section 6.2 that will be revised in the final version of the paper: the error bound on the benchmark `sqrt_add` for NumFuzz should be `9.99200722163e-16`. Observe that FPTaylor still outperforms NumFuzz on this example.

# Getting Started

The artifact can be built manually or using the provided docker image; Docker is required in order to use the docker image. The requirements for building manually are listed in the description below.

## A. Building the docker image

Build the image following these steps:

1. Extract `NumFuzz_source.tar.gz` to the directory `NumFuzz` by running `tar -xzf NumFuzz_source.tar.gz`.
2. Run `docker build -t numfuzz .` in the `NumFuzz` directory

Now, you can enter a TTY using the command `docker run --rm --name numfuzz_tty -it numfuzz` and follow the directions 
for [running the benchmarks](#running-the-benchmarks).

## B. Building manually

### Requirements
Building manually requires Dune and Ocaml version 4.14.1 with a native compiler. The additional requirements for FPTaylor and Gappa are as follows.
- For FPTaylor: [OCaml Num library](https://github.com/ocaml/num). This can be insalled via the command `opam install num`.
- For Gappa:
  1. [GMP](https://gmplib.org/).  Install via  `sudo apt-get install libgmp3-dev`.
  2. [MPFR](https://www.mpfr.org/) you can obtain the required version from
https://www.mpfr.org/mpfr-4.1.1/ and build as follows.

To build MPFR, you first have to install GNU MP (version 5.0.0 or higher). Then, in the MPFR build directory, type the following commands.
```
$ ./configure
```
This will prepare the build and set up the options according to your system.

```
$ make
```
This will compile MPFR, and create a library archive file libmpfr.a. On most platforms, a dynamic library will be produced too.

```
$ make check
```
This will make sure that MPFR was built correctly. If any test fails, information about this failure can be found in the tests/test-suite.log file.

```
$ make install
```
This will copy the files mpfr.h and mpf2mpfr.h to the directory /usr/local/include, the library files (libmpfr.a and possibly others) to the directory /usr/local/lib, the file mpfr.info to the directory /usr/local/share/info, and some other documentation files to the directory /usr/local/share/doc/mpfr (or if you passed the ‘--prefix’ option to configure, using the prefix directory given as argument to ‘--prefix’ instead of /usr/local).
  3. [Boost](https://www.boost.org/). Install via `sudo apt-get install libboost-all-dev`.

# Running the Benchmarks

In order to verify the error bounds and check the timings listed in Table 3 of Section 6.2 simply run `make tests` in the top-level `NumFuzz` directory. This will generate the file `NumFuzz/tests.txt`. 

To run all benchmarks for each tool individually, you can run `make tests` in the tool directory `examples/TOOLNAME` (e.g., `examples/numfuzz`). This will generate a file `examples/toolname_tests.txt` (e.g., `examples/numfuzz_tests.txt`).

To run individual benchmarks, use the following commands.
- FPTaylor: 
- Gappa: 
- NumFuzz: 