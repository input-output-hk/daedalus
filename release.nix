{ system ? builtins.currentSystem
, buildNum ? null
}:
let
  daedalusPkgs = { cluster ? null }: import ./. {
    inherit system buildNum cluster;
    version = "${version}${suffix}";
  };
  suffix = if buildNum == null then "" else "-${toString buildNum}";
  version = (builtins.fromJSON (builtins.readFile (./. + "/package.json"))).version;

  makeJobs = cluster: with daedalusPkgs { inherit cluster; }; {
    inherit daedalus;
    installer = wrappedBundle newBundle pkgs cluster daedalus-bridge.version;
  };
  wrappedBundle = newBundle: pkgs: cluster: cardanoVersion: let
    backend = "cardano-sl-${cardanoVersion}";
    fn = "daedalus-${version}-${backend}-${cluster}-${system}${suffix}.bin";
  in pkgs.runCommand fn {} ''
    mkdir -pv $out/nix-support
    cp ${newBundle} $out/${fn}
    echo "file binary-dist $out/${fn}" >> $out/nix-support/hydra-build-products
    size="$(stat $out/${fn} --printf="%s")"
    echo installerSize $(($size / 1024 / 1024)) MB >> $out/nix-support/hydra-metrics
  '';
  lib = (import ./. {}).pkgs.lib;
  clusters = lib.splitString " " (builtins.replaceStrings ["\n"] [""] (builtins.readFile ./installer-clusters.cfg));
in {
  tests = (daedalusPkgs {}).tests;
} // builtins.listToAttrs (map (x: { name = x; value = makeJobs x; }) clusters)
