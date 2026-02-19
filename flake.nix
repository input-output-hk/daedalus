{
  description = "The open source wallet for ada, built to grow with the Cardano blockchain";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs.flake = true;
    nixpkgsJs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgsJs.flake = true;
    cardano-wallet-unpatched.url = "github:cardano-foundation/cardano-wallet/v2025-03-31";
    cardano-wallet-unpatched.flake = false; # otherwise, +10k quadratic dependencies in flake.lock…
    cardano-node-override.url = "github:IntersectMBO/cardano-node/10.5.4";
    cardano-node-override.flake = false;
    cardano-playground.url = "github:input-output-hk/cardano-playground/56ebfef5595c43014029b039ade01b0ef06233e0";
    cardano-playground.flake = false; # otherwise, +9k dependencies in flake.lock…
    cardano-shell.url = "github:input-output-hk/cardano-shell/79f48aa3aa8007a1597cbedc22031eab1f05decd";
    cardano-shell.flake = false;
    mithril.url = "github:input-output-hk/mithril/2603.1";
    mithril.flake = true;
    mithril.inputs.nixpkgs.follows = "nixpkgs";
    tullia.url = "github:input-output-hk/tullia";
    tullia.flake = false; # otherwie, +1k dependencies in flake.lock…
    flake-compat.url = "github:input-output-hk/flake-compat";
    flake-compat.flake = false;
    nix-bundle-exe.url = "github:3noch/nix-bundle-exe";
    nix-bundle-exe.flake = false;
  };

  outputs = inputs: let
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    inherit (inputs.nixpkgs) lib;
  in {
    internal = import ./nix/internal.nix { inherit inputs; };

    packages = lib.genAttrs supportedSystems (buildSystem:
      import ./nix/packages.nix { inherit inputs buildSystem; }
    );

    devShells = lib.genAttrs supportedSystems (targetSystem:
      import ./nix/devshells.nix { inherit inputs targetSystem; }
    );

    # Compatibility with older Nix:
    defaultPackage = __mapAttrs (_: a: a.default) inputs.self.outputs.packages;
    devShell = __mapAttrs (_: a: a.default) inputs.self.outputs.devShells;

    hydraJobs = {
      installer = lib.genAttrs (supportedSystems ++ ["x86_64-windows"]) (
        targetSystem: inputs.self.internal.${targetSystem}.unsignedInstaller
      );
      devshell = lib.genAttrs supportedSystems (system: inputs.self.devShells.${system}.default);
      # Exposing these DLLs for easier development/debugging on Windows:
      nativeModules.x86_64-windows = inputs.self.internal.x86_64-windows.nativeModulesZip;
      required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
        name = "github-required";
        meta.description = "All jobs required to pass CI";
        constituents =
          lib.collect lib.isDerivation inputs.self.hydraJobs.installer
          ++ lib.collect lib.isDerivation inputs.self.hydraJobs.devshell;
      };
    };
  };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
