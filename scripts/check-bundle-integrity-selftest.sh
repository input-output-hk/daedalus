#!/usr/bin/env bash

# Self-test for check-bundle-integrity.sh.
#
# A checker that only ever runs against a bundle that passes is untested in the
# direction that matters. This builds a minimal, self-consistent fixture bundle,
# asserts the checker accepts it, then breaks the fixture one way at a time and
# asserts the checker rejects each break with the expected reason.
#
# The fixture uses a statically linked binary so it has no DT_NEEDED entries of
# its own and is therefore genuinely complete — no dependency on what happens to
# be installed on the machine running the test.
#
# Usage: check-bundle-integrity-selftest.sh <path-to-check-bundle-integrity.sh>

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <path-to-check-bundle-integrity.sh>" >&2
  exit 2
fi

CHECKER="$1"
if [ ! -f "$CHECKER" ]; then
  echo "ERROR: no such file: $CHECKER" >&2
  exit 2
fi

# Invoke through bash rather than executing directly: the checker's
# `#!/usr/bin/env bash` shebang does not resolve inside the Nix build sandbox,
# which has no /usr/bin/env.
BASH_ABS=$(command -v bash)

run_checker() {
  "$BASH_ABS" "$CHECKER" "$@"
}

for tool in cc readelf; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "ERROR: required tool '$tool' not found" >&2
    exit 2
  }
done

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

failures=0

expect_pass() {
  local name="$1" dir="$2"
  if run_checker "$dir" >"$WORK/out" 2>&1; then
    echo "ok   $name"
  else
    echo "FAIL $name — expected the checker to accept this bundle"
    sed 's/^/       /' "$WORK/out"
    failures=$((failures + 1))
  fi
}

expect_fail() {
  local name="$1" dir="$2" pattern="$3"
  if run_checker "$dir" >"$WORK/out" 2>&1; then
    echo "FAIL $name — checker accepted a bundle it should have rejected"
    failures=$((failures + 1))
  elif grep -q "$pattern" "$WORK/out"; then
    echo "ok   $name"
  else
    echo "FAIL $name — rejected, but not for the expected reason"
    echo "       wanted: $pattern"
    sed 's/^/       /' "$WORK/out"
    failures=$((failures + 1))
  fi
}

# A minimal bundle with the same shape as the real one: a launcher that derives
# LIB_DIR/BUNDLE_DIR from its own location and exports paths into the bundle,
# plus a statically linked "electron" so the library closure is complete.
make_fixture() {
  local dir="$1"
  rm -rf "$dir"
  mkdir -p "$dir/bin" "$dir/lib/electron/lib" "$dir/share/X11/xkb"
  echo 'int main(void) { return 0; }' >"$WORK/tiny.c"
  cc -static -o "$dir/lib/electron/electron" "$WORK/tiny.c"
  touch "$dir/share/X11/xkb/rules"
  cat >"$dir/bin/electron" <<'LAUNCHER'
#!/bin/sh
LIB_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")/lib"
BUNDLE_DIR="$(dirname "$LIB_DIR")"
export XKB_CONFIG_ROOT="$BUNDLE_DIR/share/X11/xkb"
export GBM_BACKENDS_PATH="$LIB_DIR/electron/lib"
exec "$LIB_DIR"/electron/electron "$@"
LAUNCHER
  chmod +x "$dir/bin/electron"
}

echo "Self-testing $CHECKER"
echo

make_fixture "$WORK/good"
expect_pass "accepts a well-formed bundle" "$WORK/good"

# 1. A launcher export pointing at something that is not in the bundle.
make_fixture "$WORK/broken-export"
rm -rf "$WORK/broken-export/share/X11/xkb"
expect_fail "rejects an unresolvable launcher export" \
  "$WORK/broken-export" "XKB_CONFIG_ROOT points at"

# 2. A symlink whose target does not exist — the macOS signing failure.
make_fixture "$WORK/broken-symlink"
ln -s "$WORK/broken-symlink/lib/electron/absent.so" \
  "$WORK/broken-symlink/lib/electron/dangling.so"
expect_fail "rejects a dangling symlink" \
  "$WORK/broken-symlink" "dangling symlink"

# 3. A DT_NEEDED entry with nothing in the bundle to satisfy it.
make_fixture "$WORK/broken-closure"
cc -o "$WORK/broken-closure/lib/electron/dynamic" "$WORK/tiny.c"
expect_fail "rejects an unresolvable DT_NEEDED" \
  "$WORK/broken-closure" "which is not in the bundle"

# 4. The checker must refuse to report success when a tool it depends on is
#    absent, rather than skipping every file it was meant to inspect.
mkdir -p "$WORK/empty-path"
if PATH="$WORK/empty-path" "$BASH_ABS" "$CHECKER" "$WORK/good" >"$WORK/out" 2>&1; then
  echo "FAIL reports success with no readelf available"
  failures=$((failures + 1))
else
  if grep -q "refusing to report success" "$WORK/out"; then
    echo "ok   refuses to run without the tools its assertions need"
  else
    echo "FAIL failed without the tools, but not with the guard message"
    sed 's/^/       /' "$WORK/out"
    failures=$((failures + 1))
  fi
fi

echo
if [ "$failures" -gt 0 ]; then
  echo "SELF-TEST FAILED: $failures case(s)"
  exit 1
fi
echo "SELF-TEST PASSED"
