#!/usr/bin/env bash
# End-to-end benchmark for the appel interpreter.
#
# Usage: benches/bench.sh [-n RUNS] [-b BINARY] [FILTER]
#   -n RUNS    runs per case (default 5); min and median wall time are reported
#   -b BINARY  binary to benchmark (default: target/release/appel, built first)
#   FILTER     only run cases whose name contains FILTER
#
# Every case is run as `appel [--pre PRELUDE] EXPR` with stdout discarded, so
# timings include startup, parsing, evaluation and formatting the output.

set -euo pipefail

runs=5
bin=""
while getopts "n:b:" opt; do
	case $opt in
		n) runs=$OPTARG ;;
		b) bin=$OPTARG ;;
		*) exit 2 ;;
	esac
done
shift $((OPTIND - 1))
filter=${1:-}

root=$(cd "$(dirname "$0")/.." && pwd)
if [ -z "$bin" ]; then
	cargo build --release --manifest-path "$root/Cargo.toml" >/dev/null 2>&1
	bin="$root/target/release/appel"
fi

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# preludes
: >"$tmp/empty.appel"
echo 'fn f a = a + 1' >"$tmp/named.appel"
for i in $(seq 1 300); do echo "v$i = $i"; done >"$tmp/globals.appel"
cat >"$tmp/fact.appel" <<'EOF'
fn head n xs = (n rho xs)
fn fact n = head 1 ((((n != 0) rho n) * (fact . ((n != 0) rho (n - 1)))) , 1)
EOF
long_sum=$(printf '1 + %.0s' $(seq 1 2000))1

# name | prelude | expression
cases=(
	"startup|empty|1"
	"elementwise_1M|empty|+//((iota 1000000) * 2 + 1)"
	"rho_repeat_1M|empty|+//(1000000 rho (1 2 3))"
	"bignum_fact_20k|empty|*//iota 20000"
	"scan_200k|empty|+\\\\iota 200000"
	"sort_200k|empty|+//(up ((iota 200000) % 977))"
	"index_100k|empty|+//((iota 1000000)[(iota 100000) * 7])"
	"print_300k|empty|iota 300000"
	"parse_long_expr|empty|$long_sum"
	"named_map_200k|named|+//(f . (iota 200000))"
	"lambda_map_200k|empty|+//((\\a -> a + 1) . (iota 200000))"
	"lambda_map_20k_300globals|globals|+//((\\a -> a + 1) . (iota 20000))"
	"lambda_fold_200k|empty|(\\a b -> a + b) // (iota 200000)"
	"let_in_lambda_20k_300globals|globals|+//((\\a -> let b = a in b + 1) . (iota 20000))"
	"recursive_fact_200|fact|+//(fact . (iota 200))"
)

now_ns() { date +%s%N; }

printf '%-32s %10s %10s  %s\n' case "min(ms)" "median(ms)" status
for c in "${cases[@]}"; do
	IFS='|' read -r name prelude expr <<<"$c"
	[[ -n $filter && $name != *"$filter"* ]] && continue

	times=()
	status=ok
	for _ in $(seq 1 "$runs"); do
		start=$(now_ns)
		if ! "$bin" --pre "$tmp/$prelude.appel" "$expr" >/dev/null 2>"$tmp/stderr"; then
			status="FAIL: $(head -c 60 "$tmp/stderr" | tr '\n' ' ')"
		fi
		end=$(now_ns)
		times+=($(((end - start) / 1000)))
	done

	sorted=($(printf '%s\n' "${times[@]}" | sort -n))
	min=${sorted[0]}
	med=${sorted[$((runs / 2))]}
	LC_ALL=C awk -v n="$name" -v a="$min" -v b="$med" -v s="$status" \
		'BEGIN { printf "%-32s %10.1f %10.1f  %s\n", n, a / 1000, b / 1000, s }'
done
