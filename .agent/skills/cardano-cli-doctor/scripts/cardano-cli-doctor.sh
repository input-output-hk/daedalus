#!/usr/bin/env bash
set -euo pipefail

echo "== cardano-cli-doctor =="
echo

if ! command -v cardano-cli >/dev/null 2>&1; then
  echo "ERROR: cardano-cli not found in PATH"
  exit 1
fi

echo "-- version --"
cardano-cli version || true
echo

echo "-- top help --"
cardano-cli --help | sed -n '1,60p' || true
echo

echo "-- era-prefixed probes --"
for era in latest conway babbage alonzo mary allegra shelley byron; do
  if cardano-cli "$era" --help >/dev/null 2>&1; then
    echo "SUPPORTED era prefix: $era"
  fi
done
echo

echo "-- legacy era flag probe (transaction build help excerpt) --"
cardano-cli transaction build --help | sed -n '1,140p' || true
echo

echo "== done =="
