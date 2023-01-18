#!/usr/bin/env bash

if [[ "$CI" != "true" ]]; then
  yarn lockfile:fix

  # Let’s patch electron-rebuild to force correct Node.js headers to
  # build native modules against even in `nix-shell`, otherwise, it
  # doesn’t work reliably.
  eval "$(nix-build -A rawapp.patchElectronRebuild)"
fi
