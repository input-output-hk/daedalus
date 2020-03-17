{ runCommand, cardano-wallet, cardano-node, cardano-shell, export-wallets, db-converter }:

runCommand "daedalus-haskell-bridge" {
} ''
  mkdir -pv $out/bin
  cd $out/bin
  cp ${cardano-wallet.haskellPackages.cardano-wallet-byron.components.exes.cardano-wallet-byron}/bin/cardano-wallet-byron* .
  cp ${cardano-shell.nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  cp ${cardano-node.cardano-node}/bin/cardano-node* .
  cp ${export-wallets}/bin/export-wallets* .
  cp ${db-converter}/bin/db-converter* .
''
