#!/usr/bin/env bash

# Static integrity assertions for a built, relocatable bundle.
#
# The bundle makes claims about itself — environment variables it exports,
# libraries it needs, links it contains — and nothing else verifies that those
# claims hold against the directory actually produced. A build that succeeds
# while producing a bundle that cannot start is the failure mode this guards.
#
# Usage: check-bundle-integrity.sh <bundle-root>
#
# Assertions:
#   1. Every bundle-relative path the launcher exports resolves inside the bundle
#   2. No symlink in the bundle dangles
#   3. Every shared library named in DT_NEEDED is present in the bundle
#
# Deliberately generic: the launcher is parsed for what it exports rather than
# checked against a hardcoded list, so a variable added later is covered with no
# edit here.

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <bundle-root>" >&2
  exit 2
fi

BUNDLE="${1%/}"

if [ ! -d "$BUNDLE" ]; then
  echo "ERROR: not a directory: $BUNDLE" >&2
  exit 2
fi

# The assertions below are only meaningful if the tools that make them are
# present. A missing tool must fail loudly rather than silently pass over every
# file it was supposed to inspect.
for tool in readelf find grep sed; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "ERROR: required tool '$tool' not found; refusing to report success" >&2
    exit 2
  fi
done

failures=0

report() {
  echo "  FAIL: $1"
  failures=$((failures + 1))
}

is_elf() {
  # Cheaper than invoking `file`, and enough to skip scripts and data.
  [ -f "$1" ] && [ "$(head -c 4 "$1" 2>/dev/null | tr -d '\0')" = $'\x7fELF' ]
}

# Collect ELF files once; several assertions iterate over them.
elf_files=()
while IFS= read -r candidate; do
  if is_elf "$candidate"; then
    elf_files+=("$candidate")
  fi
done < <(find "$BUNDLE" -type f)

echo "Bundle: $BUNDLE"
echo "ELF objects: ${#elf_files[@]}"
echo

# ---------------------------------------------------------------------------
# 1. Launcher environment paths
#
# The launcher computes LIB_DIR and BUNDLE_DIR from its own location, then
# exports variables pointing into the bundle. Parse those exports out of the
# built launcher and assert each target exists. Values rooted anywhere else
# (XCURSOR_PATH points at /usr/share/icons and is -d guarded) are correctly
# not matched.
# ---------------------------------------------------------------------------
echo "1. Launcher environment paths"
launcher="$BUNDLE/bin/electron"
if [ ! -f "$launcher" ]; then
  report "no launcher at $launcher"
else
  # shellcheck disable=SC2016  # $BUNDLE_DIR/$LIB_DIR are literals to match in
  # the launcher's own text, not variables to expand here.
  exports=$(grep -oE 'export [A-Z_]+="\$(BUNDLE_DIR|LIB_DIR)[^"]*"' "$launcher" || true)
  if [ -z "$exports" ]; then
    report "launcher exports no bundle-relative paths; the parse is out of date"
  else
    while IFS= read -r line; do
      [ -n "$line" ] || continue
      var=${line#export }
      var=${var%%=*}
      value=${line#*=\"}
      value=${value%\"}
      # LIB_DIR is <bundle>/lib; BUNDLE_DIR is its parent, i.e. the bundle root.
      resolved=${value//\$LIB_DIR/$BUNDLE/lib}
      resolved=${resolved//\$BUNDLE_DIR/$BUNDLE}
      if [ -e "$resolved" ]; then
        echo "  ok: $var -> ${resolved#"$BUNDLE"/}"
      else
        report "$var points at ${resolved#"$BUNDLE"/}, which does not exist"
      fi
    done <<<"$exports"
  fi
fi
echo

# ---------------------------------------------------------------------------
# 2. Dangling symlinks
#
# A broken link inside the bundle is a shipped defect: the macOS signing pass
# rejects one, and on Linux it surfaces as a missing file at runtime.
# ---------------------------------------------------------------------------
echo "2. Dangling symlinks"
dangling=$(find "$BUNDLE" -xtype l || true)
if [ -n "$dangling" ]; then
  while IFS= read -r link; do
    report "dangling symlink: ${link#"$BUNDLE"/} -> $(readlink "$link")"
  done <<<"$dangling"
else
  echo "  ok: none"
fi
echo

# ---------------------------------------------------------------------------
# 3. Shared library closure
#
# Every DT_NEEDED entry must correspond to a file inside the bundle. A library
# that resolves only because the build machine happens to have it installed is
# the defect that is invisible to all of our own testing — every machine we own
# has a populated Nix store, and a user's does not.
# ---------------------------------------------------------------------------
echo "3. Shared library closure"
# Index every shared object in the bundle by basename.
declare -A have_lib=()
for elf in "${elf_files[@]}"; do
  have_lib["$(basename "$elf")"]=1
done
# Symlinked sonames are real resolution targets too — but only when the link
# resolves. `! -xtype l` excludes dangling ones: indexing those would let a
# broken link satisfy a DT_NEEDED entry, so this assertion would report a
# library as present precisely when it is not, and assertion 2 would be the
# only thing left catching it.
while IFS= read -r link; do
  [ -n "$link" ] || continue
  have_lib["$(basename "$link")"]=1
done < <(find "$BUNDLE" -type l ! -xtype l)

unresolved=0
for elf in "${elf_files[@]}"; do
  while IFS= read -r needed; do
    [ -n "$needed" ] || continue
    if [ -z "${have_lib[$needed]:-}" ]; then
      report "${elf#"$BUNDLE"/} needs $needed, which is not in the bundle"
      unresolved=$((unresolved + 1))
    fi
  done < <(readelf -d "$elf" 2>/dev/null | sed -n 's/.*(NEEDED).*\[\(.*\)\]/\1/p')
done
if [ "$unresolved" -eq 0 ]; then
  echo "  ok: every DT_NEEDED resolves inside the bundle"
fi
echo

if [ "$failures" -gt 0 ]; then
  echo "FAILED: $failures problem(s) found"
  exit 1
fi

echo "PASSED"
