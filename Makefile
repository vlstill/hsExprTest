# vim: noexpandtab

PWD != pwd
BUILD_DIR=${PWD}/_build
SRC=${PWD}/src
HS_ROOT=${SRC}/hs
HS_CABAL=${HS_ROOT}/hsExprTest.cabal
CABAL_OPTS_BUILD=--builddir ${BUILD_DIR}
CABAL_OPTS=${CABAL_OPTS_BUILD} --bindir ${BUILD_DIR}/bin --datasubdir ${BUILD_DIR}/data

PYSRC_PY != find src -type f -name '*.py'
PYSRC_HASHBANG != find src -type f -executable -exec sh -c 'file {} | grep -iqF python' \; -print
PYSRC=$(PYSRC_PY) $(PYSRC_HASHBANG)

-include local.make

all : submodules build

CXXFLAGS += -D_POSIX_C_SOURCE

${BUILD_DIR} :
	mkdir -p $@
	touch $@

${BUILD_DIR}/obj :
	mkdir -p $@
	touch $@

build : service pycheck
	mkdir -p ${BUILD_DIR}/data
	mkdir -p ${BUILD_DIR}/bin
	cabal install ${HS_CABAL} --dependencies-only ${CABAL_OPTS}
	cd ${HS_ROOT} && cabal configure ${CABAL_OPTS}
	cd ${HS_ROOT} && cabal build ${CABAL_OPTS_BUILD}
	cabal install ${HS_CABAL} ${CABAL_OPTS}

service : ${BUILD_DIR}/hsExprTest-service

${BUILD_DIR}/obj/service.o : ${SRC}/core/service.cpp $(wildcard ${SRC}/core/*.hpp) $(wildcard ext/bricks/*) ${BUILD_DIR}/obj
	$(CXX) $(CXXFLAGS) -c -Wall -Wextra -Wold-style-cast -std=c++1z -Iext/bricks -pthread $< -o $@

${BUILD_DIR}/hsExprTest-service : ${BUILD_DIR}/obj/service.o
	-rm -f ${BUILD_DIR}/service
	$(CXX) $(LDFLAGS) $< -o $@ -pthread -lacl

test : pycheck
	cabal install ${HS_CABAL} --only-dependencies --enable-tests
	cd ${HS_ROOT} && cabal configure --enable-tests ${CABAL_OPTS}
	cd ${HS_ROOT} && cabal test --show-details=always ${CABAL_OPTS_BUILD}

pycheck : $(PYSRC:%=%-mypy)

$(PYSRC:%=%-mypy) :
	mypy $(@:%-mypy=%)

# test : build
#	./test/driver examples $T
#	./test/driver test $T

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean test build submodules pycheck $(PYSRC:%=%-mypy)

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
