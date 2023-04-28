#!/usr/bin/env bash

set -o errexit
set -o pipefail

# XXX: From the very beginning we have problems with `*.node` native
# extensions being built against Node.js ABI, and not
# Electron’s. Let’s solve this once and for all.

chmod -R +w node_modules/

echo 'Deleting all current ‘*.node’ files before rebuilding for Electron’s ABI:'
find . -type f -name '*.node' -not -path '*/@swc*/*' -exec rm -vf {} ';'

# Let’s patch electron-rebuild to force correct Node.js headers to
# build native modules against even in `nix-shell`, otherwise, it
# doesn’t work reliably.
nix run -L .#internal.mainnet.newCommon.patchElectronRebuild

# TODO: do we really need to run `electron-rebuild` 3×?

electron-rebuild --force

electron-rebuild -w usb-detection --force -s # <https://github.com/MadLittleMods/node-usb-detection#install-for-electron>

if [ "$(uname)" == "Linux" ] ; then
  # We ship debug version because the release one has issues with Ledger Nano S
  electron-rebuild -w usb --force -s --debug
fi

# XXX: we compare sha1sums before any signing (ad-hoc or real),
# because after signing files that were the same will differ:
echo '===== all *.node files after ‘electron-rebuild’: ====='
find . -type f -name '*.node' | sort | xargs sha1sum
echo ========================================================

# Several native modules have to be linked in Debug/ in
# root directory, for `yarn dev` to work correctly. If a Debug
# version of such extension exists, we use it, otherwise, we
# use Release:

tryLink() {
  local dependency="$1"
  local fileName="$2"
  local symlinkTarget
  symlinkTarget=$(ls 2>/dev/null -d \
    "$PWD/node_modules/${dependency}/build/Debug/${fileName}" \
    "$PWD/node_modules/${dependency}/build/Release/${fileName}" \
    || true | head -1
  )
  if [ -z "$symlinkTarget" ] ; then
    echo >&2 "error: symlink target not found: ‘${fileName}’ in ‘${dependency}’"
    exit 1
  fi
  mkdir -p Debug/ Release/
  ln -svf "$symlinkTarget" Debug/
  ln -svf "$symlinkTarget" Release/
}

tryLink   "usb"           "usb_bindings.node"
tryLink   "usb-detection" "detection.node"
tryLink   "node-hid"      "HID.node"

if [ "$(uname)" == "Linux" ] ; then
  tryLink "node-hid"      "HID_hidraw.node"
fi
