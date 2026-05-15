#!/usr/bin/env bash

set -o errexit
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")"; pwd)"

# XXX: From the very beginning we have problems with `*.node` native
# extensions being built against Node.js ABI, and not
# Electron’s. Let’s solve this once and for all.

chmod -R +w node_modules/

echo "Deleting all current ‘*.node’ files before rebuilding for Electron’s ABI:"
find . -type f -name '*.node' -not -path '*/@swc*/*' -exec rm -vf {} ';'

# Let’s patch electron-rebuild to force correct Node.js headers to
# build native modules against even in `nix-shell`, otherwise, it
# doesn’t work reliably.
nix run -L .#internal."${system:-x86_64-darwin}".common.patchElectronRebuild

# XXX: Electron requires c++17, not 14 (or old 1y):
sed -r 's,std=c\+\+(14|1y),std=c++17,g' -i node_modules/usb/binding.gyp

"$SCRIPT_DIR"/darwin-no-x-compile.sh

# Remove ALL fsevents instances before electron-rebuild: v1.x uses old v8 NAN APIs
# (v8::Object::GetIsolate removed in Electron 41+). yarn v1 nests fsevents v1.x
# inside e.g. node_modules/chokidar/node_modules/fsevents — electron-rebuild scans
# recursively and would find any nested copy. Not needed at runtime by Daedalus.
find node_modules -depth -type d -name 'fsevents' -exec rm -rf '{}' ';' 2>/dev/null || true

electron-rebuild --force -s

if [[ $system == *linux* ]]; then
  # Rebuild usb in Debug mode: the debug build ships instead of Release because
  # the release build has issues with Ledger Nano S:
  electron-rebuild -w usb --force -s --debug
fi

# XXX: we compare sha1sums before any signing (ad-hoc or real),
# because after signing files that were the same will differ:
echo "===== all *.node files after ‘electron-rebuild’: ====="
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
tryLink   "node-hid"      "HID.node"

if [[ $system == *linux* ]]; then
  tryLink "node-hid"      "HID_hidraw.node"
fi
