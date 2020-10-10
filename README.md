# 6120
CS 6120 things!

# Required Libraries

You will need to run the following to first switch to OCaml 4.11.0 (if you don't already have it)

```
opam switch create 4.11.0
```

otherwise you can simply switch by doing

```
opam switch 4.11.0
```

Then you should install the relevant libraries using opam

```
opam install ocamlfind merlin yojson ppx_deriving core ppx_jane
```

# Compiling the Program

```
make rebuild
```
If prompted to delete brilc, enter `y`

# Running DCE/LVN/LVN+DCE

To run just DCE or just LVN or LVN followed by DCE execute the following command.

```
bril2json < <filepath> | ./brilc [dce|lvn|lvn-dce] | bril2txt | bril2json
```

This will produce a new optimized program from the original program specified in `<filepath>` and print it to `stdout`.

Brench outputs are in the `out/` directory. We add some extraneous labels on occasion, because they're needed for from-SSA conversion (which uses LVN). 

# Running Dataflow Analyses

We have implemented a generic dataflow analyses solver which is a functor over an underlying dataflow domain. The solver works both in the forward and backward fashion. The available domains are

* reaching definitions - `reach` (forward)
* live variables - `live-vars` (backward)
* constant propagation - `const-prop` (forward)

To run any of these analyses execute the following command.

```
bril2json < <filepath> | ./brilc [reach|live-vars|const-prop]
```

This will produce the results of the analysis for each basic block from the original program specified in `<filepath>` and print it to `stdout`. Technically, we associate with each instruction an element of the domain so we could print more fine-grained instruction level information as well.

# Running Dominance Utilities 

To find dominators for a functions, get the dominance tree, compute the dominance frontier or all three (dominators, tree, frontiers) for each node, run 
```
bril2json < <filepath> | ./brilc [doms|dom-tree|dom-frontiers|dom-all]
```
respectively.

# Converting to and from SSA

To convert a program to SSA form, run

```
bril2json < <filepath> | ./brilc to-ssa
```

To convert a program from SSA form, replace `to-ssa` with `from-ssa`. To convert a program from SSA form with some optimizations (LVN+DCE), run with `from-ssa-opt`.

The file `out/tossa.csv` and `out/fromssa.csv` show the results of running the code on the benchmarks. We only fail with `incorrect` for benchmarks using floats and pointers, which we do not support yet.

# Running a Bril Program

```
make test-[dce|lvn|lvn-dce|reach|live-vars|const-prop] F=<filepath>
```
If prompted to delete brilc, enter `y`.