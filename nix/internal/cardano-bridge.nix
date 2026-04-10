{
  target,
  runCommandCC,
  cardano-wallet,
  cardano-node,
  cardano-launcher,
  cardano-cli,
  cardano-address,
  lib,
  local-cluster ? null,
  mock-token-metadata-server,
  darwin,
  mithril-client,
}:
runCommandCC "daedalus-cardano-bridge" {
  passthru = {
    node-version = cardano-node.passthru.identifier.version;
    wallet-version = cardano-wallet.version;
  };
} ''
  mkdir -pv $out/bin
  cd $out/bin
  echo ${cardano-wallet.version} > $out/version
  copy_glob() {
    local label="$1"
    local pattern="$2"
    set -- $pattern
    if [ "$1" != "$pattern" ]; then
      cp -f "$@" . || true
    else
      echo "WARNING: Missing binaries for ''${label} (''${pattern})"
    fi
  }
  copy_glob "cardano-wallet" "${cardano-wallet}/bin/*"
  copy_glob "cardano-address" "${cardano-address}/bin/cardano-address*"
  copy_glob "cardano-launcher" "${cardano-launcher}/bin/*"
  copy_glob "cardano-node" "${cardano-node}/bin/*"
  copy_glob "cardano-cli" "${cardano-cli}/bin/cardano-cli*"
  copy_glob "mithril-client" "${mithril-client}/bin/mithril-client*"
  ${lib.optionalString (target == "x86_64-windows") ''
    # Upstream mithril package can emit a binary named `mithril-client`
    # even for Windows cross builds; NSIS/runtime expect `mithril-client.exe`.
    if [ -f mithril-client ] && [ ! -f mithril-client.exe ]; then
      cp -f mithril-client mithril-client.exe
    fi
  ''}
  ${lib.optionalString (local-cluster != null) ''

    ${
      if target == "x86_64-windows"
      then ''
        # Recursive for selfnode shelley test data:
        cp -rf ${local-cluster}/bin/* .

      ''
      else if target == "x86_64-linux"
      then ''
        cp -f ${local-cluster}/bin/local-cluster .

      ''
      else if target == "x86_64-darwin" || target == "aarch64-darwin"
      then ''
        cp -f ${local-cluster}/bin/local-cluster .

      ''
      else abort "Unknown target: ${target}"
    }

    cp -f ${mock-token-metadata-server}/bin/* . || true
    cp -f ${./../../utils/cardano/selfnode}/token-metadata.json .
  ''}
  ${lib.optionalString (target == "aarch64-darwin") ''
    chmod +w -R .
    for x in cardano-address cardano-node cardano-launcher cardano-cli cardano-wallet mithril-client; do
      ${darwin.sigtool}/bin/codesign --force -s - $x
    done
  ''}
''
