#!/usr/bin/env bash

set -e

usage() {
    echo "USAGE $0 EXPRTEST_WORKDIR EXPRTEST_CONFIG" >&$1
    if [[ $1 = 1 ]]; then exit 0; else exit 1; fi
}
trap "usage 2" EXIT

WORKDIR=$(readlink -f $1 2>/dev/null)
CONFIG=$(readlink -f $2 2>/dev/null)
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

test -d "$WORKDIR"
test -f "$CONFIG"

for i in $SCRIPTDIR/exprtest.{service,socket}; do
    sed -e "s|\$WORKDIR|$WORKDIR|g" -e "s|\$CONFIG|$CONFIG|g" $i > /etc/systemd/system/$(basename $i)
done

trap - EXIT
