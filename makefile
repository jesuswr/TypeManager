all: test typemanager

test:
	ghc -fhpc Tests.hs TypeManager.hs

typemanager:
	ghc main.hs -o TypeManager

clean:
	rm TypeManager Tests *.o *.hi 
	if test -s "Tests.tix"; then rm Tests.tix; fi
	if test -s ".hpc"; then rm -r .hpc; fi
