#/usr/bin/env bash

working=""
failed=""

src=$(ls dist/hsExprTest-* | tail -n1)

for i in current ghc784 ghc7101; do
    echo "Making $i"

    if nix-build -A $i --arg hsExprTestSrc $src; then
        working="$working $i"
        mv result $i
    else
        failed="$failed $i"
    fi
done

echo "Successfull builds: $working"
echo "Failed build: $failed"

if [[ -z $failed ]]; then
    exit 0
else
    exit 1
fi
