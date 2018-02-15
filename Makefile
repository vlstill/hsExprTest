
BUILD_DIR=_build
SANDBOX_DIR=_cabal_sandbox
PWD=$(shell pwd)
SRC=${PWD}/src
WRAP=${PWD}/wrap
CONFIG_STAMP=${BUILD_DIR}/.config-stamp
HS_CABAL=${SRC}/hs/hsExprTest.cabal
GHC_PACKAGE_PATH_VAR=GHC_PACKAGE_PATH="$(shell cd ${BUILD_DIR} && cabal sandbox hc-pkg list 2>/dev/null | grep '^/' | tac | sed 's/://' | paste -d: - -)"

-include local.make

all : submodules build

CXXFLAGS += -D_POSIX_C_SOURCE

${BUILD_DIR} :
	mkdir -p $@
	touch $@

${BUILD_DIR}/obj :
	mkdir -p $@
	touch $@

${BUILD_DIR}/${SANDBOX_DIR} : ${BUILD_DIR}
	cd ${BUILD_DIR} && cabal sandbox init --sandbox=${SANDBOX_DIR}

configure : ${CONFIG_STAMP}

${CONFIG_STAMP} :
	make ${BUILD_DIR}/${SANDBOX_DIR}
	touch $@

build : ${CONFIG_STAMP} service ${BUILD_DIR}/hsExprTest
	cd ${BUILD_DIR} && cabal install ${HS_CABAL}

service : ${BUILD_DIR}/hsExprTest-service

${BUILD_DIR}/obj/service.o : ${SRC}/core/service.cpp $(wildcard ${SRC}/core/*.hpp) ${BUILD_DIR}/obj
	$(CXX) $(CXXFLAGS) -c -Wall -Wextra -Wold-style-cast -std=c++1z -Iext/bricks/bricks -pthread $< -o $@

${BUILD_DIR}/hsExprTest-service :	${BUILD_DIR}/obj/service.o
	-rm -f ${BUILD_DIR}/service
	$(CXX) $(LDFLAGS) $< -o $@ -pthread -lacl

${BUILD_DIR}/hsExprTest :
	echo "#!/usr/bin/env bash" > $@
	echo 'export ${GHC_PACKAGE_PATH_VAR}' >> $@
	echo 'export PATH="$(shell cd ${BUILD_DIR} && pwd)/${SANDBOX_DIR}/bin:$$PATH"' >> $@
	echo 'exec $(notdir $@) "$$@"' >> $@
	chmod +x $@

test : build
	./test/driver examples $T
	./test/driver test $T

clean :
	rm -rf ${BUILD_DIR}

submodules :
	git submodule update -i

.PHONY: all clean configure build ${BUILD_DIR}/hsExprTest submodules

# test : .cabal-sandbox
# 	cabal install --only-dependencies --enable-tests
# 	cabal configure --enable-tests
# 	cabal install
# 	chmod +x wrap/env.sh
# 	./wrap/env.sh -c "cabal test --show-details=always"
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
