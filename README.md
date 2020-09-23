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
opam install ocamlfind merlin yojson ppx_deriving
```

# Compiling the Program

```
make rebuild
```
If prompted to delete brilc, enter `y`

# Running the Program

```
bril2json < <filepath> | ./brilc [lvn|dce|lvn-dce] | bril2txt | bril2json | brili -p
```
Brench outputs are in the `out/` directory. We add some extraneous jumps sometimes, so we don't outperform the baseline in terms of dynamic instructions with DCE. But LVN or LVN + DCE works great! 

# Running a Bril Program

```
make test-[dce|lvn|lvn-dce] F=<filepath>
```
If prompted to delete brilc, enter `y`