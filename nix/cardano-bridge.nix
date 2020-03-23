{ target, pkgs, runCommand, cardano-wallet, cardano-node, cardano-shell, export-wallets, db-converter }:

let
  commonLib = import ../lib.nix {};
  pkgsCross = import cardano-wallet.pkgs.path { crossSystem = cardano-wallet.pkgs.lib.systems.examples.mingwW64; config = {}; overlays = []; };
in runCommand "daedalus-cardano-bridge" {
  passthru = {
    node-version = cardano-node.version;
    wallet-version = cardano-wallet.version;
  };
} ''
  mkdir -pv $out/bin
  cd $out/bin
  echo ${cardano-wallet.version} > $out/version
  cp ${cardano-wallet.haskellPackages.cardano-wallet-byron.components.exes.cardano-wallet-byron}/bin/cardano-wallet-byron* .
  cp ${cardano-shell.nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  cp ${cardano-node}/bin/cardano-node* .
  cp ${export-wallets}/bin/export-wallets* .
  cp ${db-converter}/bin/db-converter* .
  ${pkgs.lib.optionalString (target == "x86_64-windows") ''
    echo ${cardano-wallet.jormungandr}
    cp ${pkgsCross.libffi}/bin/libffi-6.dll .
  ''}
''
  #${pkgs.lib.optionalString (target == "x86_64-linux") ''
  #  for bin in cardano-launcher cardano-wallet-byron cardano-node export-wallets db-converter; do
  #    ${pkgs.binutils-unwrapped}/bin/strip $bin
  #    ${pkgs.patchelf}/bin/patchelf --shrink-rpath $bin
  #  done
  #''}
