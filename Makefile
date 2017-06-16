all : build

build: .cabal-sandbox serviceProxy
	cabal install
	chmod +x wrap/hsExprTest.sh wrap/hsExprTestService.sh
	ln -s wrap/hsExprTest.sh hsExprTest || true
	ln -s wrap/hsExprTestService.sh hsExprTestService || true

serviceProxy : exec/serviceProxy.cpp
	g++ -std=c++11 -Wall -Wextra -Wold-style-cast -pthread $< -o $@

sandbox : .cabal-sandbox

.cabal-sandbox :
	cabal sandbox init

install:
	cabal install

clean:
	-cabal clean
	rm -rf dist
	rm -f hsExprTest hsExprTestService

clean-all: clean
	find -name '*.hi' -exec rm {} \;
	find -name '*.o' -exec rm {} \;
	-cabal sandbox delete

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

deploy-haddock :
	rm -rf _deploy_haddock
	mkdir _deploy_haddock
	cd _deploy_haddock
	[[ -x ./.cabal-sandbox/bin/standalone-haddock ]] || cabal install standalone-haddock
	./.cabal-sandbox/bin/standalone-haddock --package-db .cabal-sandbox/*.conf.d/ . -o _deploy_haddock
	rsync -a --del _deploy_haddock/hsExprTest/ antea:public_html/doc/hsExprTest


HINT_OPTS=--hint support/HLint.hs --cpp-define="MIN_VERSION_base(x,y,z) 1" \
		  --cpp-define="MIN_VERSION_template_haskell(x,y,z) 1"

hlint : $(shell if which hlint 2>/dev/null; then echo hlint-global; else echo hlint-local; fi)

hlint-global :
	hlint $(HINT_OPTS) src exec tests examples

hlint-local : .cabal-sandbox
	[[ -x ./.cabal-sandbox/bin/hlint ]] || cabal install hlint
	./.cabal-sandbox/bin/hlint $(HINT_OPTS) src exec tests examples
