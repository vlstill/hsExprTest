all : build

build: sandbox
	cabal install
	chmod +x wrap/expressionTesting.sh
	ln -s wrap/expressionTesting.sh expressionTesting || true

sandbox : .cabal-sandbox

.cabal-sandbox :
	cabal sandbox init

install:
	cabal install

clean:
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	rm -rf dist || true
	rm expressionTesting || true

clean-all: clean
	cabal sandbox delete

env : build
	chmod +x wrap/env.sh
	wrap/env.sh
