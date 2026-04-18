{
  description = "The open source wallet for ada, built to grow with the Cardano blockchain";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs.flake = true;
    nixpkgsJs.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgsJs.flake = true;
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    cardano-wallet-unpatched.url = "github:cardano-foundation/cardano-wallet/v2025-03-31";
    cardano-wallet-unpatched.flake = false; # otherwise, +10k quadratic dependencies in flake.lock…
    cardano-node-override.url = "github:IntersectMBO/cardano-node/10.6.4";
    cardano-node-override.flake = false;
    cardano-playground.url = "github:input-output-hk/cardano-playground/node-10.6.2-config";
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

  outputs = {
    self,
    flake-parts,
    nixpkgs,
    ...
  } @ inputs: let
    inherit ((import ./flake/lib.nix {inherit inputs;}).flake.lib) recursiveImports;
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    inherit (nixpkgs) lib;
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports =
        recursiveImports [
          ./flake
          ./perSystem
        ]
        ++ [
          inputs.treefmt-nix.flakeModule
        ];
      systems = supportedSystems;

      flake = {
        # Keep internal for backward compatibility and cross-compilation
        internal = import ./nix/internal.nix {inherit inputs;};

        # Compatibility with older Nix
        defaultPackage = __mapAttrs (_: a: a.default) self.outputs.packages;
        devShell = __mapAttrs (_: a: a.default) self.outputs.devShells;

        # Hydra jobs
        hydraJobs = {
          installer = lib.genAttrs (supportedSystems ++ ["x86_64-windows"]) (
            targetSystem: self.internal.${targetSystem}.unsignedInstaller
          );
          devshell = lib.genAttrs supportedSystems (system: self.devShells.${system}.default);
          # Exposing these DLLs for easier development/debugging on Windows:
          nativeModules.x86_64-windows = self.internal.x86_64-windows.nativeModulesZip;
          required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents =
              lib.collect lib.isDerivation self.hydraJobs.installer
              ++ lib.collect lib.isDerivation self.hydraJobs.devshell;
          };
        };
      };
    }
    // {
      inherit inputs;
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
