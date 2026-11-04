#!/usr/bin/env bash
set -euo pipefail
shopt -s dotglob nullglob
product=$1
expected=$2
view=${3:-materialized}
case "$view" in shells|materialized) ;; *) exit 2 ;; esac

# Compare against the pre-package layout, so missing source files or symlinks
# cannot hide by disappearing from both Shared and the derived app views.
verify_tree() {
  local source=$1 destination=$2 mode=$3 item
  if [ -L "$source" ]; then
    test -L "$destination"
    test "$(readlink "$source")" = "$(readlink "$destination")"
    if [ -e "$source" ]; then test -e "$destination"; fi
  elif [ -d "$source" ]; then
    test -d "$destination"
    test ! -L "$destination"
    if [ "$source" = "$expected/Shared" ]; then mode=metadata; fi
    for item in "$source"/*; do verify_tree "$item" "$destination/${item##*/}" "$mode"; done
  else
    test -f "$destination"
    test ! -L "$destination"
    if [ "$mode" = links ]; then
      test "$source" -ef "$destination"
    elif [ "$mode" = contents ]; then
      cmp "$source" "$destination"
    fi
    if [ -x "$source" ]; then test -x "$destination"; fi
  fi
}
# GNU diff compares the large source tree in one process; the checks below also
# verify file kinds, executable bits and every derived view's inode identity.
diff -r --no-dereference "$expected/Shared" "$product/Shared"
verify_tree "$expected" "$product" contents
names=()
while IFS=$'\t' read -r kind _value name; do
  if [ "$kind" = profile ]; then names+=("$name"); fi
done < "$product/pilot-layout.tsv"
for name in "${names[@]}"; do
  while IFS=$'\t' read -r kind relative _unused; do
    if [ "$kind" = payload ]; then
      destination="$product/$name.app/Contents/$relative"
      if [ "$view" = shells ]; then
        test ! -e "$destination"
        test ! -L "$destination"
      else
        verify_tree "$product/Shared/$relative" "$destination" links
      fi
    fi
  done < "$product/pilot-layout.tsv"
done
echo 'Pilot layout, private files, payload inodes and symlinks verified.'
