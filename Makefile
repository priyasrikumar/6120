all: build

build: 
	dune build

run: build 
	mv _build/default/src/main/main.exe 6120

brench: run
	brench brench.toml > results.csv

clean: 
		dune clean