# vim: noexpandtab

PWD != pwd
BUILD_DIR=${PWD}/_build
SRC=${PWD}/src
HS_ROOT=${SRC}/hs
HS_CABAL=${HS_ROOT}/hsExprTest.cabal
CABAL_OPTS_BUILD=--builddir ${BUILD_DIR}
CABAL_OPTS=${CABAL_OPTS_BUILD} --bindir ${BUILD_DIR}/bin --datasubdir ${BUILD_DIR}/data
GHC ?= ghc-8.4

PYSRC_PY != find src -type f -name '*.py'
PYSRC_HASHBANG != find src -type f -executable -exec sh -c 'file {} | grep -iqF python' \; -print
PYSRC=$(PYSRC_PY) $(PYSRC_HASHBANG)

-include local.make

MYPY ?= mypy

all : submodules build test

CXXFLAGS += -D_POSIX_C_SOURCE

${BUILD_DIR} :
	mkdir -p $@
	touch $@

${BUILD_DIR}/obj :
	mkdir -p $@
	touch $@

builddir :
	mkdir -p ${BUILD_DIR}/data
	mkdir -p ${BUILD_DIR}/bin

configure : builddir ${BUILD_DIR}/_confstamp

${BUILD_DIR}/_confstamp :
	cabal install ${HS_CABAL} --only-dependencies --enable-tests
	cd ${HS_ROOT} && cabal configure --enable-tests ${CABAL_OPTS}

build : configure service pycheck
	cd ${HS_ROOT} && cabal build ${CABAL_OPTS_BUILD}
	cabal install --enable-tests ${HS_CABAL} ${CABAL_OPTS}

doc :
	cd ${HS_ROOT} && cabal haddock --builddir=${BUILD_DIR}

build-stack : builddir service pycheck
	stack --compiler $(GHC) build

service : ${BUILD_DIR}/hsExprTest-service

${BUILD_DIR}/obj/service.o : ${SRC}/core/service.cpp $(wildcard ${SRC}/core/*.hpp) $(wildcard ext/bricks/*) ${BUILD_DIR}/obj
	$(CXX) $(CXXFLAGS) -c -Wall -Wextra -Wold-style-cast -std=c++1z -Iext/bricks -pthread $< -o $@

${BUILD_DIR}/hsExprTest-service : ${BUILD_DIR}/obj/service.o
	-rm -f ${BUILD_DIR}/service
	$(CXX) $(LDFLAGS) $< -o $@ -pthread -lacl

test : configure pycheck build
	cd ${HS_ROOT} && cabal test --show-details=always ${CABAL_OPTS_BUILD}
	./test/driver examples $T
	./test/driver test $T

test-stack : builddir pycheck
	stack --compiler $(GHC) test

pycheck : $(PYSRC:%=%-mypy)

$(PYSRC:%=%-mypy) :
	$(MYPY) $(@:%-mypy=%)

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean test configure build builddir submodules pycheck $(PYSRC:%=%-mypy)

#
# haddock : .cabal-sandbox/bin/haddock
# 	cabal haddock --html --with-haddock=./.cabal-sandbox/bin/haddock
# 	@echo "file://$$PWD/dist/doc/html/hsExprTest/index.html"
#
# HINT_OPTS=--hint support/HLint.hs --cpp-define="MIN_VERSION_base(x,y,z) 1" \
# 		  --cpp-define="MIN_VERSION_template_haskell(x,y,z) 1"
#
# hlint : $(shell if which hlint 2>/dev/null; then echo hlint-global; else echo hlint-local; fi)
#
# hlint-global :
# 	hlint $(HINT_OPTS) src exec tests examples
#
# hlint-local : .cabal-sandbox
# 	[[ -x ./.cabal-sandbox/bin/hlint ]] || cabal install hlint
# 	./.cabal-sandbox/bin/hlint $(HINT_OPTS) src exec tests examples
