let
  makeJobs = cluster: with import ./. { inherit cluster; }; {
    inherit daedalus;
    installer = wrappedBundle newBundle pkgs;
  };
  wrappedBundle = newBundle: pkgs: pkgs.runCommand "daedaus-installer" {} ''
    mkdir -pv $out/nix-support
    cp ${newBundle} $out/Daedalus-installer.bin
    echo "file binary-dist $out/Daedalus-installer.bin" >> $out/nix-support/hydra-build-products
    size="$(stat $out/Daedalus-installer.bin --printf="%s")"
    echo installerSize $(($size / 1024 / 1024)) MB >> $out/nix-support/hydra-metrics
  '';
in {
  mainnet = makeJobs "mainnet";
  staging = makeJobs "staging";
}
