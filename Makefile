# vim: noexpandtab

PWD != pwd
BUILD_DIR=${PWD}/_build
HS_ROOT=${PWD}
HS_CABAL=${HS_ROOT}/hsExprTest.cabal
CABAL_OPTS=--builddir ${BUILD_DIR}
GHC ?= ghc
HADDOCKDYN != if grep -q ID=arch /etc/os-release; then echo " --ghc-options=-dynamic"; else echo ""; fi

PYSRC_PY != find ${PWD} -type f -name '*.py'
PYSRC_HASHBANG != find ${PWD} -type f -executable -exec sh -c 'file {} | grep -iqF python' \; -print
PYSRC != echo $(PYSRC_PY) $(PYSRC_HASHBANG) | tr ' ' '\n' | awk '!a[$$0]++'

-include local.make

MYPY ?= mypy

all : submodules build test

${BUILD_DIR} :
	mkdir -p $@
	touch $@

${BUILD_DIR}/obj :
	mkdir -p $@
	touch $@

builddir :
	mkdir -p ${BUILD_DIR}

configure :

build : build-hs pycheck

prerequisites :
	cabal v2-install --lib QuickCheck

build-hs : builddir prerequisites
	cd ${HS_ROOT} && cabal v2-build ${CABAL_OPTS}
	cabal v2-install --lib ${HS_CABAL} ${CABAL_OPTS}

doc : builddir
	cd ${HS_ROOT} && cabal v2-haddock $(HADDOCKDYN) --builddir=${BUILD_DIR}
	find ${BUILD_DIR}/doc/html -name '*.html' -exec sed -i 's|<a href="file:///[^"]*/html/libraries/\([^"/]*\)/|<a href="https://hackage.haskell.org/package/\1/docs/|g' {} \;

test : configure build pycheck
	cd ${HS_ROOT} && cabal v2-test ${CABAL_OPTS}
	./test/driver examples $T
	./test/driver test $T

pycheck : $(PYSRC:%=%-mypy)

$(PYSRC:%=%-mypy) :
	env MYPYPATH=$(dir $@) $(MYPY) --check-untyped-defs --warn-redundant-casts --warn-return-any $(@:%-mypy=%)

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean test configure build builddir submodules pycheck doc $(PYSRC:%=%-mypy)
