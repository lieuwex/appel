#!/bin/sh

cargo build --release
prog="$PWD/target/release/appel"

for n in $(appel "2 ** (iota $1)"); do
	printf '%s ' $n; command time -f '%U' $prog "*//(iota $n)" >/dev/null
done
