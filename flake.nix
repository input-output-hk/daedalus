{
  description = "The open source wallet for ada, built to grow with the Cardano blockchain";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs.flake = true;
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    cardano-wallet.url = "github:cardano-foundation/cardano-wallet/v2026-09-16";
    cardano-wallet.flake = false; # otherwise, +10k quadratic dependencies in flake.lock…
    cardano-node.url = "github:IntersectMBO/cardano-node/11.1.2";
    cardano-node.flake = false;
    cardano-playground.url = "github:input-output-hk/cardano-playground/next-2026-04-16";
    cardano-playground.flake = false; # otherwise, +9k dependencies in flake.lock…
    cardano-shell.url = "github:input-output-hk/cardano-shell/79f48aa3aa8007a1597cbedc22031eab1f05decd";
    cardano-shell.flake = false;
    # switch to a release as soon as mac fixes are included
    mithril.url = "github:input-output-hk/mithril/sl/fix-mismatch-rust-versions";
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
    inherit ((import ./flake/lib/recursive-imports.nix {inherit inputs;}).flake.lib) recursiveImports;
    supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    inherit (nixpkgs) lib;
    installerClusters = let
      readClustersFile = fileName: let
        unique = builtins.foldl' (acc: e:
          if builtins.elem e acc
          then acc
          else acc ++ [e]) [];
      in
        unique (
          builtins.map builtins.unsafeDiscardStringContext (
            builtins.filter (el: builtins.isString el && el != "") (
              builtins.split "[ \n\r\t]+" (
                builtins.readFile fileName
              )
            )
          )
        );
    in
      readClustersFile (inputs.self + "/installer-clusters.cfg");
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
        # Compatibility with older Nix
        defaultPackage = __mapAttrs (_: a: a.default) self.outputs.packages;
        devShell = __mapAttrs (_: a: a.default) self.outputs.devShells;

        # Hydra jobs
        hydraJobs = {
          installer = {
            x86_64-linux = lib.genAttrs installerClusters (cluster: self.packages.x86_64-linux."installer-${cluster}");
            x86_64-darwin = lib.genAttrs installerClusters (cluster: self.packages.x86_64-darwin."installer-${cluster}");
            aarch64-darwin = lib.genAttrs installerClusters (cluster: self.packages.aarch64-darwin."installer-${cluster}");
            x86_64-windows = lib.genAttrs installerClusters (cluster: self.packages.x86_64-linux."installer-x86_64-windows-${cluster}");
          };
          devshell = lib.genAttrs supportedSystems (system: self.devShells.${system}.default);
          # Exposing these DLLs for easier development/debugging on Windows:
          nativeModules.x86_64-windows = self.packages.x86_64-linux.nativeModules-x86_64-windows;
          # Every system's checks, not only x86_64-linux. Pinned to one system,
          # a derivation added to `checks.aarch64-darwin` was a flake output
          # Hydra never evaluated and `required` never collected — present
          # locally, absent from CI, and silently so.
          checks = lib.genAttrs supportedSystems (system: self.checks.${system});
          # Only x86_64-linux gates a merge. The darwin checks are built and
          # reported on every commit, so a failure is visible on the pull
          # request, but a scarce or flaky mac builder does not hold up work —
          # the same posture the darwin installers already have.
          required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents =
              lib.collect lib.isDerivation self.hydraJobs.checks.x86_64-linux;
          };
          nonrequired = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
            name = "github-nonrequired";
            meta.description = "Jobs built by Hydra but not required to pass CI";
            constituents =
              lib.collect lib.isDerivation self.hydraJobs.installer
              ++ lib.collect lib.isDerivation self.hydraJobs.devshell
              ++ lib.collect lib.isDerivation
              (removeAttrs self.hydraJobs.checks ["x86_64-linux"]);
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
