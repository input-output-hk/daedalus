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
  daedalusPkgsWithSystem = system: import ./. { target = system; };
  yaml2json = {
    x86_64-linux = (daedalusPkgsWithSystem "x86_64-linux").yaml2json;
    x86_64-darwin = (daedalusPkgsWithSystem "x86_64-darwin").yaml2json;
  };
  daedalus-installer = {
    x86_64-linux = (daedalusPkgsWithSystem "x86_64-linux").daedalus-installer;
    x86_64-darwin = (daedalusPkgsWithSystem "x86_64-darwin").daedalus-installer;
  };

  makeJobs = cluster: with daedalusPkgs { inherit cluster; }; {
    daedalus.x86_64-linux = daedalus;
    installer.x86_64-linux = wrappedBundle newBundle pkgs cluster daedalus-bridge.version;
    installer.x86_64-windows = (import ./. { inherit cluster; target = "x86_64-windows"; }).windows-installer;
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
  inherit shellEnvs yaml2json daedalus-installer;
  inherit ((daedalusPkgs {}).pkgs) mono;
  wine = (daedalusPkgs {}).pkgs.wine.override { wineBuild = "wine32"; };
  tests = (daedalusPkgs {}).tests;
} // builtins.listToAttrs (map (x: { name = x; value = makeJobs x; }) clusters)
