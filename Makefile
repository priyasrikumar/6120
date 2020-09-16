all: build

build: 
		dune build 

run: build
		./_build/default/src/main/main.exe $(file)

clean: 
		dune clean