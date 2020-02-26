{ target, pkgs, cardano-wallet, cardano-shell, sources, jormungandrLib }:

let
  commonLib = import ../lib.nix {};
  pkgsCross = import cardano-wallet.pkgs.path { crossSystem = cardano-wallet.pkgs.lib.systems.examples.mingwW64; config = {}; overlays = []; };
in pkgs.runCommandCC "daedalus-bridge" {
  passthru = {
    node-version = cardano-wallet.jormungandr.version;
    wallet-version = cardano-wallet.version;
  };
} ''
  mkdir -pv $out/bin
  cd $out/bin
  cp ${cardano-wallet.haskellPackages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr}/bin/cardano-wallet-jormungandr* .
  cp ${cardano-shell.nix-tools.cexes.cardano-launcher.cardano-launcher}/bin/cardano-launcher* .
  cp ${cardano-wallet.jormungandr-cli}/bin/jcli* .
  cp ${cardano-wallet.jormungandr}/bin/jormungandr* .

  echo ${cardano-wallet.version} > $out/version

  chmod +w -R .

  ${pkgs.lib.optionalString (target == "x86_64-windows") ''
    echo ${cardano-wallet.jormungandr}
    cp ${pkgsCross.libffi}/bin/libffi-6.dll .
    #cp {pkgsCross.openssl.out}/lib/libeay32.dll .
  ''}
  ${pkgs.lib.optionalString (target == "x86_64-linux") ''
    for bin in cardano-launcher cardano-wallet-jormungandr; do
      strip $bin
      patchelf --shrink-rpath $bin
    done
  ''}
''
