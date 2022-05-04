{ target, runCommandCC, cardano-wallet, cardano-node, cardano-shell, cardano-cli, cardano-address, lib, local-cluster ? null, darwin }:

runCommandCC "daedalus-cardano-bridge" {
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
  cp -f ${cardano-shell.haskellPackages.cardano-launcher.components.exes.cardano-launcher}/bin/* .
  cp -f ${cardano-node}/bin/cardano-node* .
  cp -f ${cardano-cli}/bin/cardano-cli* .
  ${lib.optionalString (local-cluster != null) "cp -f ${local-cluster}/bin/local-cluster* ."}
  ${lib.optionalString (target == "x86_64-linux") ''
    chmod +w -R .
    for x in cardano-address cardano-node cardano-launcher cardano-cli cardano-wallet; do
      $STRIP $x
      patchelf --shrink-rpath $x
    done
  ''}
  ${lib.optionalString (target == "aarch64-darwin") ''
    chmod +w -R .
    for x in cardano-address cardano-node cardano-launcher cardano-cli cardano-wallet; do
      ${darwin.sigtool}/bin/codesign --force -s - $x
    done
  ''}
''
