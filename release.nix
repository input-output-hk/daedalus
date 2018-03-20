{ version ? "1.1.0", buildNr ? "nix" }:
let
  makeJobs = cluster: with import ./. { inherit cluster; version = "${version}.${buildNr}"; }; {
    inherit daedalus;
    installer = wrappedBundle newBundle pkgs cluster;
  };
  wrappedBundle = newBundle: pkgs: cluster: let
    fn = "Daedalus-${cluster}-installer-${version}.${buildNr}.bin";
  in pkgs.runCommand "daedaus-installer" {} ''
    mkdir -pv $out/nix-support
    cp ${newBundle} $out/${fn}
    echo "file binary-dist $out/${fn}" >> $out/nix-support/hydra-build-products
    size="$(stat $out/${fn} --printf="%s")"
    echo installerSize $(($size / 1024 / 1024)) MB >> $out/nix-support/hydra-metrics
  '';
in {
  mainnet = makeJobs "mainnet";
  staging = makeJobs "staging";
}
