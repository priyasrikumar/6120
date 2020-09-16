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
make build
```

# Running the Program

```
make file=FILEPATH run
```
Currently prints out out the number of memory instructions in the program.