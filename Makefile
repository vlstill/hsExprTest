all : build

build: .cabal-sandbox
	cabal install
	chmod +x wrap/hsExprTest.sh
	ln -s wrap/hsExprTest.sh hsExprTest || true

sandbox : .cabal-sandbox

.cabal-sandbox :
	cabal sandbox init

install:
	cabal install

clean:
	cabal clean
	rm -rf dist || true
	rm hsExprTest || true

clean-all: clean
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	cabal sandbox delete

env : build
	chmod +x wrap/env.sh
	wrap/env.sh

sdist :
	cabal sdist

nixcheck : sdist
	chmod +x nixcheck.sh
	./nixcheck.sh

nixbuild : sdist
	nix-build -A current --arg hsExprTestSrc $$(ls dist/hsExprTest-*.tar.gz | tail -n1)

test : .cabal-sandbox
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests
	cabal install
	chmod +x wrap/env.sh
	./wrap/env.sh -c "cabal test --show-details=always"

.cabal-sandbox/bin/haddock : .cabal-sandbox
	cabal install haddock
	touch $@
		
haddock : .cabal-sandbox/bin/haddock
	cabal haddock --html --with-haddock=./.cabal-sandbox/bin/haddock
	@echo "file://$$PWD/dist/doc/html/hsExprTest/index.html"
