with import ./. {};
let
  wrappedBundle = pkgs.runCommand "daedaus-installer" {} ''
    mkdir -pv $out/nix-support
    echo "file daedalus-installer ${newBundle}" >> $out/nix-support/hydra-build-products
    size="$(stat ${newBundle} --printf="%s")"
    echo installerSize $(($size / 1024 / 1024)) MB >> $out/nix-support/hydra-metrics
  '';
in {
  inherit daedalus;
  installer = wrappedBundle;
}
