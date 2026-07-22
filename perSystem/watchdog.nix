# Rust `cardano-watchdog` — crane-based build.
# Replaces cardano-launcher for independent node/wallet process supervision.
# Picked up automatically by the flake's recursiveImports scan.
{inputs, ...}: {
  perSystem = {
    inputs',
    lib,
    pkgs,
    ...
  }: let
    toolchain = with inputs'.fenix.packages;
      combine [
        stable.rustc
        stable.cargo
        stable.clippy
        stable.rustfmt
      ];

    craneLib = (inputs.crane.mkLib pkgs).overrideToolchain toolchain;

    cargoLockExists = builtins.pathExists ./../watchdog/Cargo.lock;

    watchdogBuild = lib.optionalAttrs cargoLockExists (let
      src = lib.fileset.toSource {
        root = ./../watchdog;
        fileset = lib.fileset.unions [
          ./../watchdog/Cargo.toml
          ./../watchdog/Cargo.lock
          ./../watchdog/src
        ];
      };
      commonArgs = {
        inherit src;
        strictDeps = true;
        pname = "cardano-watchdog";
        version = "0.1.0";
      };
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;
    in {
      packages.cardano-watchdog = craneLib.buildPackage (commonArgs
        // {
          inherit cargoArtifacts;
          meta = {
            description = "Independent process supervisor for cardano-node and cardano-wallet";
            mainProgram = "cardano-watchdog";
            license = lib.licenses.asl20;
          };
        });

      checks.watchdog-clippy = craneLib.cargoClippy (commonArgs
        // {
          inherit cargoArtifacts;
          cargoClippyExtraArgs = "--all-targets -- --deny warnings";
        });
    });
  in
    watchdogBuild
    // {
      devShells =
        (watchdogBuild.devShells or {})
        // {
          watchdog = pkgs.mkShell {
            packages = [toolchain pkgs.cargo-watch];
            shellHook = ''
              echo "cardano-watchdog devShell"
              echo "  build: cargo build --manifest-path watchdog/Cargo.toml"
              echo "  run:   cargo run --manifest-path watchdog/Cargo.toml"
            '';
          };
        };
    };
}
