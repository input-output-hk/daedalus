#!/usr/bin/env bash
set -euo pipefail
shopt -s dotglob nullglob

product=$1
copy=${2:-cp}
if [ "$#" -lt 2 ] && [ "$(uname -s)" = Darwin ]; then copy=/bin/cp; fi
shared="$product/Shared"
names=()
paths=()
while IFS=$'\t' read -r kind value name; do
  case "$kind" in
    profile)
      case "$name" in ''|.|..|*/*) echo 'Invalid profile name' >&2; exit 1 ;; esac
      names+=("$name")
      ;;
    payload)
      case "$value" in MacOS/*|Frameworks/*|Resources/app/*) ;; *) exit 1 ;; esac
      case "/$value/" in */../*|*/./*) exit 1 ;; esac
      test -z "$name"
      paths+=("$value")
      ;;
    *) echo 'Invalid Pilot layout record' >&2; exit 1 ;;
  esac
done < "$product/pilot-layout.tsv"
test "${#names[@]}" -gt 0
test "${#paths[@]}" -gt 0

# Missing Shared/manifest entries must fail before creating any app views.
for relative in "${paths[@]}"; do
  test -e "$shared/$relative" || test -L "$shared/$relative"
done
for name in "${names[@]}"; do
  test -d "$product/$name.app/Contents"
  test ! -L "$product/$name.app/Contents"
done

# cp -l is broken on some older macOS releases. Probe the actual operation,
# including inode sharing and internal symlinks, instead of trusting its version.
probe=$(mktemp -d "${TMPDIR:-/tmp}/daedalus-pilot-links.XXXXXX")
trap 'rm -rf "$probe"' EXIT
mkdir -p "$probe/source/A"
printf 'probe\n' > "$probe/source/A/file"
ln -s A "$probe/source/Current"
fast=false
if "$copy" -RlP "$probe/source" "$probe/view" > "$probe/diagnostic" 2>&1 &&
  [ -d "$probe/view/A" ] && [ ! -L "$probe/view" ] && [ ! -L "$probe/view/A" ] &&
  [ "$probe/source/A/file" -ef "$probe/view/A/file" ] &&
  [ -L "$probe/view/Current" ] && [ "$(readlink "$probe/view/Current")" = A ]; then
  fast=true
fi
if [ "$fast" = false ]; then
  echo 'Using portable hard-link creation (cp -l capability probe failed).'
fi

# POSIX ln/mkdir/readlink work with the system Bash shipped on older Macs.
# Existing correct views are accepted; conflicting files are never overwritten.
link_tree() {
  local source=$1 destination=$2 item
  if [ -L "$source" ]; then
    if [ -L "$destination" ] && [ "$(readlink "$source")" = "$(readlink "$destination")" ]; then return; fi
    if [ -e "$destination" ] || [ -L "$destination" ]; then echo "Conflicting path: $destination" >&2; return 1; fi
    ln -s "$(readlink "$source")" "$destination"
  elif [ -d "$source" ]; then
    test ! -L "$destination"
    if [ ! -d "$destination" ]; then mkdir "$destination"; fi
    for item in "$source"/*; do link_tree "$item" "$destination/${item##*/}"; done
  elif [ -f "$source" ]; then
    if [ ! -L "$destination" ] && [ "$source" -ef "$destination" ]; then return; fi
    if [ -e "$destination" ] || [ -L "$destination" ]; then echo "Conflicting path: $destination" >&2; return 1; fi
    ln "$source" "$destination"
  else
    echo "Unsupported payload file: $source" >&2
    return 1
  fi
}

for name in "${names[@]}"; do
  app="$product/$name.app/Contents"
  for relative in "${paths[@]}"; do
    destination="$app/$relative"
    if [ "$fast" = true ] && [ ! -e "$destination" ] && [ ! -L "$destination" ]; then
      "$copy" -RlP "$shared/$relative" "$destination"
    else
      link_tree "$shared/$relative" "$destination"
    fi
  done
done
