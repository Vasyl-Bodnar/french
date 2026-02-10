# french
A SAT solver, currently using simulated annealing in OxCaml.

## Build
You need `dune` to build this project.
I use OxCaml toolkit, however it should be buildable on usual 5.0+ OCaml as I do not currently utilize any OxCaml features.

You can build the executable with `dune build`.

## Use
You can run the executable with `dune exec french`. 
You may also want to install it on your system with `dune install`, assuming your PATH is correct.

You can use `french --help` to see available options. 
But, typical use could be:
``` sh
french -s "p cnf 1 1  1 0"
```
or if you want to tweak options:
``` sh
french -s "p cnf 1 1  1 0" --temp 0.89 --iters 200
```
or if you want to use files (with `-f` or without). Short options are also available:
``` sh
french example.dim -t 0.89 -i 200
```

Note that the supported format for strings and files is DIMACS CNF.

Currently the library API is in progress for use in other OCaml programs.

Note that this is simulated annealing only at the moment. 
It can find SAT, but it is unable to give UNSAT, only UNKNOWN with a minimum number of unsatisfied clauses it could find.
This can be useful for Max-SAT however, and is essentially timeout.

## Benchmark
This repo includes some lighter SAT benchmarks of Holger H. Hoos for SATLIB.
From my testing, `french` generally can do well on these, but there are plenty of times where it times out too fast.
More optimization and improvements to be seen, especially if CLDL or other SAT solving algorithm are implemented.

## License
This project is licensed under MPL Version 2.0.
