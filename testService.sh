#!/usr/bin/env bash

QDIR=$HOME/repo/ib015_questions

[[ -n $1 ]] && QDIR=$1

echo "QDIR = $QDIR" >&2

if ! echo $TERM | grep -iq screen; then
    echo "This test must run in screen to execute multiple terminals" >&2
    exit
fi

echo | socat - UNIX-CONNECT:sock >&/dev/null || screen -t service bash -c "./hsExprTestService sock $QDIR 2>&1 | tee test.log"

echo "enter question id:"
read id

echo "enter the question:"
data=`cat`

Q="I0Q${id}S${data}"
echo -en "$Q"
echo -en "$Q" | socat -t 70 - UNIX-CONNECT:sock
