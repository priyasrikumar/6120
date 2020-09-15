all: build

build: 
		dune build 

run: build
		./_build/default/src/cli.exe $(file)

clean: 
		dune clean