{ target, runCommandCC, cardano-wallet, cardano-node, cardano-shell, cardano-cli, cardano-address, lib, local-cluster ? null, mock-token-metadata-server, darwin }:

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
  cp -f ${cardano-node}/bin/* .
  cp -f ${cardano-cli}/bin/cardano-cli* .
  ${lib.optionalString (local-cluster != null) ''

    ${if target == "x86_64-windows" then ''
      # Recursive for selfnode shelley test data:
      cp -rf ${local-cluster}/bin/* .

    '' else if target == "x86_64-linux" then ''
      cp -f ${local-cluster}/bin/local-cluster .

    '' else if target == "x86_64-darwin" || target == "aarch64-darwin" then ''
      cp -f ${local-cluster}/bin/local-cluster .

    '' else abort "Unknown target: ${target}"}

    cp -f ${mock-token-metadata-server}/bin/* . || true
    cp -f ${./../../utils/cardano/selfnode}/token-metadata.json .
  ''}
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
