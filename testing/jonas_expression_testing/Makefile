all : build

update :
	cabal update
	make build

build:
	cabal configure
	cabal build
	cp dist/build/expressionTesting/expressionTesting .

install: 
	cabal install

clean:
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	rm -rf dist
	rm expressionTesting
