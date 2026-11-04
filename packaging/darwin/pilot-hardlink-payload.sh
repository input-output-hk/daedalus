#!/usr/bin/env bash
set -euo pipefail
shopt -s dotglob nullglob

product=$1
shared="$product/Shared"
mode=${2:-link}
case "$mode" in link|prune) ;; *) echo 'Expected link or prune' >&2; exit 2 ;; esac
copy="cp"
if [ "$(uname -s)" = Darwin ]; then copy=/bin/cp; fi

# Real bundle directories keep Cocoa/Electron resource discovery inside each
# app. Only regular immutable payload files share inodes. Preserve framework
# version links (Versions/Current, Resources, etc.) exactly as shipped.
share_tree() {
  local source=$1 destination=$2
  # Remove the destination first: overwriting an existing hard-linked file
  # could otherwise change Shared. rm does not traverse directory symlinks.
  rm -rf "$destination"
  if [ "$mode" = link ]; then
    "$copy" -RlP "$source" "$destination"
  fi
}

for name in 'Daedalus Preview' 'Daedalus Pre-Prod' 'Daedalus Flight'; do
  app="$product/$name.app/Contents"
  test -d "$app"
  for item in "$shared/MacOS"/*; do
    case "${item##*/}" in 'Daedalus Flight') continue ;; esac
    share_tree "$item" "$app/MacOS/${item##*/}"
  done
  for item in "$shared/Frameworks"/*; do
    case "${item##*/}" in 'Daedalus Flight Helper'*.app) continue ;; esac
    share_tree "$item" "$app/Frameworks/${item##*/}"
  done
  for item in "$shared/Resources/app"/*; do
    case "${item##*/}" in package.json) continue ;; esac
    share_tree "$item" "$app/Resources/app/${item##*/}"
  done
done
