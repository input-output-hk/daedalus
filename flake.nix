{
  description = "Daedalus itself, and jobs populating <https://cache.iog.io> with its dependencies";
  inputs = {
    nixpkgs.follows = "tullia/nixpkgs";
    tullia.url = "github:input-output-hk/tullia";
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
      let all = forEachCluster (cluster: import ./nix/old-shell.nix { inherit system cluster; });
      in all // { default = all.mainnet; }
    );

    # Compatibility with older Nix:
    defaultPackage = __mapAttrs (_: a: a.default) inputs.self.outputs.packages;
    devShell = __mapAttrs (_: a: a.default) inputs.self.outputs.devShells;

    hydraJobs = rec {

      # --- Linux ----------------------------------------------------
      x86_64-linux.x86_64-linux = let
        d = inputs.self.outputs.packages.x86_64-linux.internal.mainnet.oldCode;
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
        devShellGCRoot = inputs.self.outputs.devShells.x86_64-linux.default.gcRoot;
        nodejs = d.nodejs;
        tests = d.pkgs.releaseTools.aggregate { name = "tests"; constituents = __attrValues d.tests; };
        yaml2json = d.yaml2json;
      };
      # --------------------------------------------------------------

      # --- Windows (x-compiled from Linux) --------------------------
      x86_64-linux.x86_64-windows = let
        d = inputs.self.outputs.packages.x86_64-windows.internal.mainnet.oldCode;
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
        daedalus-installer = d.daedalus-installer;
        mono = d.pkgs.mono;
        wine = d.wine;
        wine64 = d.wine64;
        yaml2json = d.yaml2json;
      };
      # --------------------------------------------------------------

      # --- Darwin ---------------------------------------------------
      x86_64-darwin.x86_64-darwin = let
        d = inputs.self.outputs.packages.x86_64-darwin.internal.mainnet.oldCode;
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
        daedalus-installer = d.daedalus-installer;
        devShellGCRoot = inputs.self.outputs.devShells.x86_64-darwin.default.gcRoot;
        nodejs = d.nodejs;
        yaml2json = d.yaml2json;
      };
      # --------------------------------------------------------------

      # --- What CI should build -------------------------------------
      x86_64-linux.required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
        name = "required for CI";
        constituents = __attrValues x86_64-linux.x86_64-linux ++ __attrValues x86_64-linux.x86_64-windows;
      };
      x86_64-darwin.required = inputs.nixpkgs.legacyPackages.x86_64-darwin.releaseTools.aggregate {
        name = "required for CI";
        constituents = __attrValues x86_64-darwin.x86_64-darwin;
      };
      # --------------------------------------------------------------

    };
  }
  // (let
    x86_64-linux  = inputs.tullia.fromSimple "x86_64-linux"  (import ./nix/tullia.nix);
    x86_64-darwin = inputs.tullia.fromSimple "x86_64-darwin" (import ./nix/tullia.nix);
    fakeEvent = { inputs."GitHub event" = {id = ""; created_at = ""; value = {github_body.head_commit.id="0000000";};}; id=""; ociRegistry=""; };
  in {
    tullia.x86_64-linux = x86_64-linux.tullia;
    cicero.x86_64-linux = x86_64-linux.cicero;
    tullia.x86_64-darwin = x86_64-darwin.tullia;
    cicero.x86_64-darwin = x86_64-darwin.cicero;

    ciceroLocalTest.x86_64-linux  = (x86_64-linux.cicero."daedalus/ci"  fakeEvent).job;
    ciceroLocalTest.x86_64-darwin = (x86_64-darwin.cicero."daedalus/ci" fakeEvent).job;

  });
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io" "https://iog.cachix.org"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iog.cachix.org-1:nYO0M9xTk/s5t1Bs9asZ/Sww/1Kt/hRhkLP0Hhv/ctY="];
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
