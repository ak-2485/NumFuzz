A Type System for Numerical Error Analysis
=====

This repository implements a type checker for a type system
that tracks function sensitivity and roundoff error. The type
checker is based on the implementation due to Arthur Azevedo de Amorim
and co-authors [1].
[1] Arthur Azevedo de Amorim, Marco Gaboardi, Emilio Jesús Gallego Arias, and Justin Hsu. 2014. Really Natural Linear Indexed Type Checking. In Proceedings of the 26nd 2014 International Symposium on Implementation and Application of Functional Languages (IFL '14). Association for Computing Machinery, New York, NY, USA, Article 5, 1–12. https://doi.org/10.1145/2746325.2746335

## Install

You need ocaml >= 4.14.1 plus dune, the mlmpfr
dependencies, and standard gnu tools like gcc and make.

You can obtain everything other than mlmpfr through the
command :
```
$ opam install --deps-only -d -t .
```

### Install mlmpfr
Building mlmpfr (Ocaml bindings for MPFR) depends on 
MPFR library version 4.1.1. Your machine might already have the appropriate MPFR version. You can check this by typing the 
following command.

```
$ opam install mlmpfr
```

If the install fails due to the MPFR version, you can obtain 
the required version from
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

Once the correct version of MPFR has been installed, run
```
$ opam install mlmpfr
```
### Build the type-checker

Once mlmpfr has been built and installed, run

```
$ dune build
```

to compile the tool

## How to compile and run programs

To typecheck a program type:

```
$ dune exec -- nfuzz examples/filename.fz
```


