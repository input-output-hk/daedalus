{
  description = "The open source wallet for ada, built to grow with the Cardano blockchain";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    cardano-wallet-unpatched.url = "github:input-output-hk/cardano-wallet/v2023-04-14";
    cardano-wallet-unpatched.flake = false; # otherwise, +10k quadratic dependencies in flake.lock…
    cardano-world.url = "github:input-output-hk/cardano-world/a0a315100ee320395be97fcc83f46678d5a7fb6e";
    cardano-world.flake = false; # otherwise, +19k quadratic dependencies in flake.lock…
    cardano-shell.url = "github:input-output-hk/cardano-shell/0d1d5f036c73d18e641412d2c58d4acda592d493";
    cardano-shell.flake = false;
    tullia.url = "github:input-output-hk/tullia";
    tullia.flake = false; # otherwie, +1k dependencies in flake.lock…
    flake-compat.url = "github:input-output-hk/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs: let
    sourceLib = import ./nix/source-lib.nix { inherit inputs; };
    inherit (sourceLib) forEachCluster;
  in {

    packages = __mapAttrs (targetSystem: definition: rec {
      internal = forEachCluster (cluster: import definition { inherit inputs cluster targetSystem; });
      package = __mapAttrs (_: a: a.package) internal;
      installer = __mapAttrs (_: a: a.unsignedInstaller) internal;
      default = package.mainnet;
      makeSignedInstaller = __mapAttrs (_: a: a.makeSignedInstaller) internal;
      buildkitePipeline = import ./nix/buildkite-pipeline.nix { inherit inputs targetSystem; };
    }) {
      x86_64-linux = ./nix/x86_64-linux.nix;
      x86_64-windows = ./nix/x86_64-windows.nix;
      x86_64-darwin = ./nix/any-darwin.nix;
      aarch64-darwin = ./nix/any-darwin.nix;
    };

    devShells = sourceLib.forEach [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let all = forEachCluster (cluster: import ./nix/old-shell.nix { inherit inputs system cluster; });
      in all // { default = all.mainnet; }
    );

    # Compatibility with older Nix:
    defaultPackage = __mapAttrs (_: a: a.default) inputs.self.outputs.packages;
    devShell = __mapAttrs (_: a: a.default) inputs.self.outputs.devShells;

  }
  // (import ./nix/cicero.nix {inherit inputs;});

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
