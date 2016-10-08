#!/usr/bin/env bash

QDIR=$HOME/repo/ib015_questions

[[ -n $1 ]] && QDIR=$1

echo "QDIR = $QDIR" >&2

if ! echo $TERM | grep -iq screen; then
    echo "This test must run in screen to execute multiple terminals" >&2
    exit
fi

echo | socat - UNIX-CONNECT:sock >&/dev/null || screen ./hsExprTestService sock $QDIR

echo "enter question id:"
read id

echo "enter the question:"
data=""
while read line; do
    data="$data$line\n"
done

echo -en "I0Q${id}S${data}" | socat -t 10 - UNIX-CONNECT:sock
