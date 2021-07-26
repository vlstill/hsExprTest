# vim: noexpandtab

PWD != pwd
BUILD_DIR=${PWD}/_build
HS_ROOT=${PWD}
HS_CABAL=${HS_ROOT}/hsExprTest.cabal
CABAL_OPTS_BLD=--builddir ${BUILD_DIR}
CABAL_OPTS_LOCAL=${CABAL_OPTS_BLD} --package-env=$(BUILD_DIR)
GHC ?= ghc

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
	rm -f ${BUILD_DIR}/.ghc.environment.*

configure :

build : build-hs pycheck

prerequisites : builddir
	cabal v2-install --lib QuickCheck ${CABAL_OPTS_LOCAL}

build-hs : prerequisites
	cd ${HS_ROOT} && cabal v2-build ${CABAL_OPTS_BLD}
	cabal v2-install --lib ${HS_CABAL} ${CABAL_OPTS_LOCAL}
	@echo "set your GHC_ENVIRONMENT to $$(readlink -f ${BUILD_DIR}/.ghc.environment.*) to use hsExprTest"

doc : builddir
	cd ${HS_ROOT} && cabal v2-haddock $(CABAL_OPTS_BLD) $(HADDOCKDYN) \
	    | tee ${BUILD_DIR}/_doclog
	rm -rf ${BUILD_DIR}/doc
	cp -r $$(dirname $$(tail -n1 ${BUILD_DIR}/_doclog))/../.. ${BUILD_DIR}/doc
	find ${BUILD_DIR}/doc/html -name '*.html' \
	    -exec sed -i 's|<a href="file:///[^"]*/html/libraries/\([^"/]*\)/|<a href="https://hackage.haskell.org/package/\1/docs/|g' {} \;

test : configure build pycheck
	cd ${HS_ROOT} && cabal v2-test ${CABAL_OPTS_BLD}
	./test/driver examples $T
	./test/driver test $T

pycheck : $(PYSRC:%=%-mypy)

$(PYSRC:%=%-mypy) :
	env MYPYPATH=$(dir $@) $(MYPY) --check-untyped-defs --warn-redundant-casts --warn-return-any $(@:%-mypy=%)

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean test configure build builddir submodules pycheck doc install $(PYSRC:%=%-mypy)
