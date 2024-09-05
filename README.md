Numerical Fuzz: A Type System for Rounding Error Analysis
=====
This is the artifact for NumFuzz ("Numerical Fuzz"), a prototype implementation of the type system and floating-point error analysis tool described in the paper *Numerical Fuzz: A Type System for Rounding Error Analysis*. The type
checker is based on the implementation due to Arthur Azevedo de Amorim
and co-authors [1].

[1] Arthur Azevedo de Amorim, Marco Gaboardi, Emilio Jesús Gallego Arias, and Justin Hsu. 2014. Really Natural Linear Indexed Type Checking. In Proceedings of the 26nd 2014 International Symposium on Implementation and Application of Functional Languages (IFL '14). Association for Computing Machinery, New York, NY, USA, Article 5, 1–12. https://doi.org/10.1145/2746325.2746335

## Install

You need ocaml >= 4.14.1 plus dune and menhir. 

You can obtain everything using the command
```
opam install --deps-only -d -t .
```

Build via dune:
```
dune build
```

Type check a benchmark in the examples directory with the following command.
```
dune exec -- nfuzz examples/BENCHMARK.fz
```


