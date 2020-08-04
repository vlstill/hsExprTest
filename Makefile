# vim: noexpandtab

PWD != pwd
BUILD_DIR=${PWD}/_build
SRC=${PWD}/src
HS_ROOT=${SRC}/hs
HS_CABAL=${HS_ROOT}/hsExprTest.cabal
CABAL_OPTS_BUILD=--builddir ${BUILD_DIR}
CABAL_OPTS=${CABAL_OPTS_BUILD} --bindir ${BUILD_DIR}/bin --datasubdir ${BUILD_DIR}/data
GHC ?= ghc
HADDOCKDYN != if grep -q ID=arch /etc/os-release; then echo " --ghc-options=-dynamic"; else echo ""; fi

PYSRC_PY != find src -type f -name '*.py'
PYSRC_HASHBANG != find src -type f -executable -exec sh -c 'file {} | grep -iqF python' \; -print
PYSRC != echo $(PYSRC_PY) $(PYSRC_HASHBANG) | tr ' ' '\n' | awk '!a[$$0]++'

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
	cabal v1-install ${HS_CABAL} --only-dependencies --enable-tests
	cd ${HS_ROOT} && cabal v1-configure --enable-tests ${CABAL_OPTS}

build : build-hs pycheck

build-hs : configure
	cd ${HS_ROOT} && cabal v1-build ${CABAL_OPTS_BUILD}
	cabal v1-install --enable-tests ${HS_CABAL} ${CABAL_OPTS}

doc :
	cd ${HS_ROOT} && cabal v1-haddock $(HADDOCKDYN) --builddir=${BUILD_DIR}
	find ${BUILD_DIR}/doc/html -name '*.html' -exec sed -i 's|<a href="file:///[^"]*/html/libraries/\([^"/]*\)/|<a href="https://hackage.haskell.org/package/\1/docs/|g' {} \;

test : configure pycheck build
	cd ${HS_ROOT} && cabal v1-test --show-details=always ${CABAL_OPTS_BUILD}
	./test/driver examples $T
	./test/driver test $T

pycheck : $(PYSRC:%=%-mypy)

$(PYSRC:%=%-mypy) :
	env MYPYPATH=$(dir $@) $(MYPY) --check-untyped-defs --warn-redundant-casts --warn-unused-ignores --warn-return-any $(@:%-mypy=%)

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean test configure build builddir submodules pycheck doc $(PYSRC:%=%-mypy)
