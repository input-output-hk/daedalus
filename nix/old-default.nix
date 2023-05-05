{ target
, inputs
, cluster
, devShell
}:

let
  system = {
    x86_64-windows = "x86_64-linux"; # Windows can only be cross-built from Linux now
  }.${target} or target;
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  walletFlake = (import inputs.flake-compat {
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
  cardanoWorldFlake = (import inputs.flake-compat { src = inputs.cardano-world; }).defaultNix.outputs;
  crossSystem = {
    x86_64-windows = pkgs.lib.systems.examples.mingwW64;
  }.${target} or null;
  ostable.x86_64-windows = "windows";
  ostable.x86_64-linux = "linux";
  ostable.x86_64-darwin = "macos64";
  ostable.aarch64-darwin = "macos64-arm";

  packages = self: {
    inherit walletFlake cardanoWorldFlake cluster pkgs target;
    inherit (walletFlake.legacyPackages.${system}.pkgs) cardanoLib;
    daedalus-bridge = self.callPackage ./cardano-bridge.nix {};

    inherit (walletPackages) cardano-wallet;
    inherit (walletPackages) cardano-address;
    inherit (walletPackages) mock-token-metadata-server;
    cardano-shell = import inputs.cardano-shell { inherit system crossSystem; };
    local-cluster = if cluster == "selfnode" then walletPackages.local-cluster else null;
    cardano-node = walletPackages.cardano-node;
    cardanoNodeVersion = self.cardano-node.version + "-" + builtins.substring 0 9 walletFlake.inputs.cardano-node-1_35_4.rev;
    cardanoWalletVersion = self.daedalus-bridge.wallet-version + "-" + builtins.substring 0 9 walletFlake.rev;
    cardano-cli = walletPackages.cardano-cli;

    launcherConfigs = self.callPackage ./launcher-config.nix {
      inherit devShell system;
      network = cluster;
      os = ostable.${target};
    };

    ## TODO: move to installers/nix
    hsDaedalusPkgs = self.callPackage ../installers {
      inherit (self) daedalus-bridge;
      inherit system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;

    tests = {
      runShellcheck = self.callPackage ../tests/shellcheck.nix { src = ../.;};
    };

  };
in pkgs.lib.makeScope pkgs.newScope packages
