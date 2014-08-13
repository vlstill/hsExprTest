all : build

update :
	cabal update
	make build

build: 
	cabal install
	ghc Main.hs -o exprTest

clean:
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	rm exprTest
