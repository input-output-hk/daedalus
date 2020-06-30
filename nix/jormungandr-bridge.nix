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
  cp -f ${cardano-wallet.haskellPackages.cardano-wallet-jormungandr.components.exes.cardano-wallet-jormungandr}/bin/* .
  cp -f ${cardano-shell.haskellPackages.cardano-launcher.components.exes.cardano-launcher}/bin/cardano-launcher* .
  cp -f ${cardano-wallet.jormungandr}/bin/* .

  echo ${cardano-wallet.version} > $out/version

  chmod +w -R .

  ${pkgs.lib.optionalString (target == "x86_64-linux") ''
    for bin in cardano-launcher cardano-wallet-jormungandr; do
      ${pkgs.binutils-unwrapped}/bin/strip $bin
      ${pkgs.patchelf}/bin/patchelf --shrink-rpath $bin
    done
  ''}
''
