_build/default/src/bin/brilc.exe: 
		dune build src/bin/brilc.exe

brilc: _build/default/src/bin/brilc.exe
	sudo cp $< $@

clean: 
		rm -fr _build

build:
	dune build src/bin/brilc.exe
rebuild: clean brilc

test-dce: rebuild
		bril2json < $F | ./brilc dce | bril2txt | bril2json | brili -p

test-lvn: rebuild
		bril2json < $F | ./brilc lvn | bril2txt | bril2json | brili -p

test-lvn-dce: rebuild
		bril2json < $F | ./brilc lvn-dce | bril2txt | bril2json | brili -p

test-reach: rebuild
		bril2json < $F | ./brilc reach | bril2txt | bril2json | brili -p

test-live-vars: rebuild
		bril2json < $F | ./brilc live-vars

test-const-prop: rebuild
		bril2json < $F | ./brilc const-prop