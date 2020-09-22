_build/default/src/bin/brilc.exe: 
		dune build src/bin/brilc.exe

brilc: _build/default/src/bin/brilc.exe
	cp $< $@

clean: 
		rm -fr _build
		rm brilc

rebuild: clean brilc