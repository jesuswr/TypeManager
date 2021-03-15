all: test typemanager

test:
	ghc Tests.hs

typemanager:
	ghc main.hs -o TypeManager

clean:
	rm TypeManager Tests *.o *.hi