#!/usr/bin/env bash

. wrap/lib
exec ./.cabal-sandbox/bin/hsExprTest "$@"
