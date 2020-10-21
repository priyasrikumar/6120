_build/default/src/bin/brilc.exe: 
		dune build src/bin/brilc.exe

brilc: _build/default/src/bin/brilc.exe
	sudo cp $< $@

clean: 
		rm -fr _build

build:
	dune build src/bin/brilc.exe

rebuild: clean brilc

test: rebuild 
	brench dce.toml > out/dce.csv
	brench lvn.toml > out/lvn.csv
	brench lvndce.toml > out/lvndce.csv
	brench tossa.toml > out/tossa.csv
	brench fromssa.toml > out/fromssa.csv

test-dce: rebuild
		bril2json < $F | ./brilc dce | bril2txt 

test-lvn: rebuild
		bril2json < $F | ./brilc lvn | bril2txt

test-lvn-dce: rebuild
		bril2json < $F | ./brilc lvn-dce | bril2txt 

test-reach: rebuild
		bril2json < $F | ./brilc reach | bril2txt 

test-live-vars: rebuild
		bril2json < $F | ./brilc live-vars | bril2txt

test-const-prop: rebuild
		bril2json < $F | ./brilc const-prop | bril2txt 