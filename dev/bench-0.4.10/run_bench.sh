#!/usr/bin/env bash
# Driver du banc CRAN 0.5.0 vs PR #58. Chaque mesure dans un processus R
# frais, versions alternees a chaque repetition pour lisser la derive machine.
# Usage : run_bench.sh <scratchpad_dir> <out_jsonl>
set -eu
SP="$1"
OUT="$2"
BENCH="$SP/datadiff-pr/dev/bench-0.4.10/bench_one.R"
DATA="$SP/benchdata"

for rep in 1 2 3; do
  for scen in local_green local_na local_fail; do
    for lib in lib_cran lib_pr; do
      echo "== rep $rep $scen $lib =="
      Rscript "$BENCH" "$SP/$lib" "$scen" "$OUT" "$DATA" 2>&1 | tail -1
    done
  done
done

for rep in 1 2; do
  for scen in lazy_green lazy_fail; do
    for lib in lib_cran lib_pr; do
      echo "== rep $rep $scen $lib =="
      Rscript "$BENCH" "$SP/$lib" "$scen" "$OUT" "$DATA" 2>&1 | tail -1
    done
  done
done
