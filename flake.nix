{
  description = "The open source wallet for ada, built to grow with the Cardano blockchain";

  inputs = {
    nixpkgs.follows = "cardano-wallet-unpatched/nixpkgs";
    cardano-wallet-unpatched.url = "github:input-output-hk/cardano-wallet/v2022-10-06";
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

  }
  // (let
    tullia = inputs.tullia.fromSimple "x86_64-linux" (import ./nix/tullia.nix);
    fakeEvent = { inputs."GitHub event" = {id = ""; created_at = ""; value = {github_body.head_commit.id="0000000";};}; id=""; ociRegistry=""; };
  in {
    tullia.x86_64-linux = tullia.tullia;
    cicero.x86_64-linux = tullia.cicero;
    ciceroLocalTest.x86_64-linux = (tullia.cicero."daedalus/ci" fakeEvent).job; # XXX: only nix-eval this
    tulliaLocalTest.x86_64-linux = tullia.tullia.task.ci.computedCommand; # XXX: fine to nix-build
  });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
