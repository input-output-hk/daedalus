{ system ? builtins.currentSystem
, buildNum ? null
}:
let
  daedalusPkgs = { cluster ? null }: import ./. {
    inherit buildNum cluster;
    target = system;
    version = "${version}${suffix}";
  };
  shellEnvs = {
    linux = import ./shell.nix { system = "x86_64-linux"; autoStartBackend = true; };
    darwin = import ./shell.nix { system = "x86_64-darwin"; autoStartBackend = true; };
  };
  suffix = if buildNum == null then "" else "-${toString buildNum}";
  version = (builtins.fromJSON (builtins.readFile ./package.json)).version;
  daedalusPkgsWithSystem = system:
  let
    table = {
      x86_64-linux = import ./. { target = "x86_64-linux"; };
      x86_64-windows = import ./. { target = "x86_64-windows"; };
      x86_64-darwin = import ./. { target = "x86_64-darwin"; };
    };
  in
    table.${system};

  makeJobs = cluster: with daedalusPkgs { inherit cluster; }; {
    daedalus.x86_64-linux = daedalus;
    installer.x86_64-linux = wrappedBundle newBundle pkgs cluster daedalus-bridge.wallet-version;
    installer.x86_64-windows = (import ./. { inherit cluster; target = "x86_64-windows"; }).windows-installer;
  };
  wrappedBundle = newBundle: pkgs: cluster: cardanoVersion: let
    backend = "cardano-wallet-${cardanoVersion}";
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
  mapOverArches = supportedTree: lib.mapAttrsRecursive (path: value: lib.listToAttrs (map (arch: { name = arch; value = lib.attrByPath path null (daedalusPkgsWithSystem arch); }) value)) supportedTree;
in {
  inherit shellEnvs;
  inherit ((daedalusPkgs {}).pkgs) mono;
  wine = (daedalusPkgs {}).pkgs.wine.override { wineBuild = "wine32"; };
  tests = (daedalusPkgs {}).tests;
} // (builtins.listToAttrs (map (x: { name = x; value = makeJobs x; }) clusters))
// (mapOverArches {
  daedalus-installer = [ "x86_64-linux" "x86_64-darwin" ];
  yaml2json = [ "x86_64-linux" "x86_64-darwin" ];
  bridgeTable = {
    jormungandr = [ "x86_64-linux" "x86_64-darwin" "x86_64-windows" ];
    cardano = [ "x86_64-linux" "x86_64-darwin" "x86_64-windows" ];
  };
  cardano-node = [ "x86_64-linux" "x86_64-darwin" "x86_64-windows" ];
  export-wallets = [ "x86_64-linux" "x86_64-darwin" "x86_64-windows" ];
  db-converter = [ "x86_64-linux" "x86_64-darwin" "x86_64-windows" ];
})
