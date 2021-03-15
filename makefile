all: test typemanager

test:
	ghc -fhpc Tests.hs TypeManager.hs

typemanager:
	ghc main.hs -o TypeManager

clean:
	rm TypeManager Tests *.o *.hi *.tix 
	rm -r .hpc