#!/usr/bin/env bash
set -e

myDir=$(dirname "$0")

echo '~~~ /etc/nix/nix.conf'

cat /etc/nix/nix.conf

echo '~~~ Building marlowe-cli with stock nix'

nix-build "$myDir"/../default.nix -A marlowe.marlowe-cli --argstr cluster marlowe_pioneers

echo '~~~ Obtaining pkgs.nixUnstable'

nixUnstable=$(nix-build "$myDir"/../default.nix -A pkgs.nixUnstable)
PATH="$nixUnstable/bin:$PATH"

export NIX_CONFIG='
  substituters = https://hydra.iohk.io https://cache.nixos.org/
  trusted-substituters = http://hydra.nixos.org
  trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
  experimental-features = nix-command flakes
'

nix --version

echo '~~~ Building marlowe-cli with updated nix'

nix-build "$myDir"/../default.nix -A marlowe.marlowe-cli --argstr cluster marlowe_pioneers
