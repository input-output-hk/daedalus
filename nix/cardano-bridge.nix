{ target, pkgs, cardanoWalletPkgs, runCommand, cardano-wallet, cardano-node, cardano-shell, export-wallets, cardano-cli, cardano-address }:

let
  commonLib = import ../lib.nix {};
  pkgsCross = import cardanoWalletPkgs.path { crossSystem = cardanoWalletPkgs.lib.systems.examples.mingwW64; config = {}; overlays = []; };
in runCommand "daedalus-cardano-bridge" {
  passthru = {
    node-version = cardano-node.passthru.identifier.version;
    wallet-version = cardano-wallet.version;
  };
} ''
  mkdir -pv $out/bin
  cd $out/bin
  echo ${cardano-wallet.version} > $out/version
  cp ${cardano-wallet}/bin/* .
  cp ${cardano-address}/bin/* .
  cp -f ${cardano-shell.nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  cp -f ${cardano-node}/bin/cardano-node* .
  cp -f ${export-wallets}/bin/export-wallets* .
  cp -f ${cardano-cli}/bin/cardano-cli* .
  ${pkgs.lib.optionalString (target == "x86_64-windows") ''
    cp -f ${pkgsCross.libffi}/bin/libffi-6.dll .
  ''}
''
