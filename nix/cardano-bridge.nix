{ target, pkgs, cardanoWalletPkgs, runCommandCC, cardano-wallet, cardano-node, cardano-shell, cardano-cli, cardano-address, stdenv, lib }:

let
  commonLib = import ../lib.nix {};
  pkgsCross = import cardanoWalletPkgs.path { crossSystem = cardanoWalletPkgs.lib.systems.examples.mingwW64; config = {}; overlays = []; };
in runCommandCC "daedalus-cardano-bridge" {
  passthru = {
    node-version = cardano-node.passthru.identifier.version;
    wallet-version = cardano-wallet.version;
  };
} ''
  mkdir -pv $out/bin
  cd $out/bin
  echo ${cardano-wallet.version} > $out/version
  cp ${cardano-wallet}/bin/* .
  cp -f ${cardano-address}/bin/cardano-address* .
  cp -f ${cardano-shell.haskellPackages.cardano-launcher.components.exes.cardano-launcher}/bin/cardano-launcher* .
  cp -f ${cardano-node}/bin/cardano-node* .
  cp -f ${cardano-cli}/bin/cardano-cli* .
  ${lib.optionalString (target == "x86_64-linux") ''
    chmod +w -R .
    for x in cardano-address cardano-node cardano-launcher cardano-cli cardano-wallet; do
      $STRIP $x
      patchelf --shrink-rpath $x
    done
  ''}
''
