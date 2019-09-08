#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
source $DIR/common

set -e

reqev true | check ok
reqev false | check nok
reqev 'echo $ARG' "FOO" | check ok 'FOO'
