{ target
, nodeImplementation ? "cardano"
, cluster ? "mainnet"
, version ? "versionNotSet"
, devShell ? false
, useLocalNode ? false
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
, sourceLib
, inputs
}:

let
  system = {
    x86_64-windows = "x86_64-linux"; # Windows can only be cross-built from Linux now
  }.${target} or target;
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  localLib = import ./old-lib.nix { inherit inputs system nodeImplementation; };
  sources = localLib.sources;
  flake-compat = import inputs.cardano-wallet-unpatched.inputs.flake-compat;
  walletFlake = (flake-compat {
    # FIXME: add patches in `flake.nix` after <https://github.com/NixOS/nix/issues/3920>
    src = pkgs.runCommand "cardano-wallet" {} ''
      cp -r ${inputs.cardano-wallet-unpatched} $out
      chmod -R +w $out
      cd $out
      patch -p1 -i ${./cardano-wallet--enable-aarch64-darwin.patch}
      patch -p1 -i ${./cardano-wallet--expose-windowsPackages.patch}
      patch -p1 -i ${./cardano-wallet--proper-runtimeNodePkgs.patch}
    '';
  }).defaultNix // {
    inherit (inputs.cardano-wallet-unpatched) rev shortRev sourceInfo;
  };
  walletPackages = {
    x86_64-windows = walletFlake.packages.x86_64-linux.windowsPackages;
    x86_64-linux = walletFlake.packages.x86_64-linux;
    x86_64-darwin = walletFlake.packages.x86_64-darwin;
    aarch64-darwin = walletFlake.packages.aarch64-darwin;
  }.${target};
  walletPkgs = walletFlake.legacyPackages.${system}.pkgs;
  cardanoWorldFlake = (flake-compat { src = sources.cardano-world; }).defaultNix.outputs;
  shellPkgs = (import "${sources.cardano-shell}/nix") { inherit system; };
  inherit (pkgs.lib) optionalString;
  crossSystem = lib: {
    x86_64-windows = lib.systems.examples.mingwW64;
  }.${target} or null;
  ostable.x86_64-windows = "windows";
  ostable.x86_64-linux = "linux";
  ostable.x86_64-darwin = "macos64";
  ostable.aarch64-darwin = "macos64-arm";

  packages = self: {
    inherit walletFlake cardanoWorldFlake cluster pkgs version target nodeImplementation;
    cardanoLib = walletPkgs.cardanoLib;
    daedalus-bridge = self.bridgeTable.${nodeImplementation};

    sources = localLib.sources;
    bridgeTable = {
      cardano = self.callPackage ./cardano-bridge.nix {};
    };
    inherit (walletPackages) cardano-wallet;
    inherit (walletPackages) cardano-address;
    inherit (walletPackages) mock-token-metadata-server;
    cardano-shell = import self.sources.cardano-shell { inherit system; crossSystem = crossSystem shellPkgs.lib; };
    local-cluster = if cluster == "selfnode" then walletPackages.local-cluster else null;
    cardano-node = walletPackages.cardano-node;
    cardanoNodeVersion = self.cardano-node.version + "-" + builtins.substring 0 9 self.cardano-node.src.rev;
    cardanoWalletVersion = self.daedalus-bridge.wallet-version + "-" + builtins.substring 0 9 walletFlake.rev;
    cardano-cli = walletPackages.cardano-cli;

    launcherConfigs = self.callPackage ./launcher-config.nix {
      inherit devShell topologyOverride configOverride genesisOverride system;
      network = cluster;
      os = ostable.${target};
      backend = nodeImplementation;
    };

    ## TODO: move to installers/nix
    hsDaedalusPkgs = self.callPackage ../installers {
      inherit (self) daedalus-bridge;
      inherit localLib system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;
    source = builtins.filterSource localLib.cleanSourceFilter ../.;

    tests = {
      runShellcheck = self.callPackage ../tests/shellcheck.nix { src = ../.;};
    };

  };
in pkgs.lib.makeScope pkgs.newScope packages
