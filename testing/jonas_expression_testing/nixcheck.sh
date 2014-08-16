#/usr/bin/env bash

working=""
failed=""


for i in current ghc783 ghc763 ghc742 ghc722 ghc704; do
    echo "Making $i"

    if nix-build -A $i; then
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
