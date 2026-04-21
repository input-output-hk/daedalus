# Rust `drt` (Daedalus release tool) — crane-based build.
#
# This file is picked up automatically by the flake's recursiveImports scan.
# It adds:
#   devShells.release-cli — Rust toolchain + release tools (use this for
#                           release-cli development; isolated from the main
#                           Daedalus devShell so neither breaks the other)
#   devShells.ops         — minimal shell: just drt + gpg, no Yarn/Node
#   packages.drt          — the release-cli binary  \  only once
#   checks.drt-clippy     — workspace clippy          |  Cargo.lock
#   checks.drt-fmt        — rustfmt check            /   exists
{inputs, ...}: {
  perSystem = {
    inputs',
    config,
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
        stable.rust-analyzer
      ];

    craneLib = (inputs.crane.mkLib pkgs).overrideToolchain toolchain;

    cargoLockExists = builtins.pathExists ./../release-cli/Cargo.lock;

    opsShellHook = ''
      bold='\033[1m'
      cyan='\033[1;36m'
      yellow='\033[1;33m'
      green='\033[1;32m'
      dim='\033[2m'
      reset='\033[0m'
      printf "\n"
      printf "''${cyan}  ██████╗  █████╗ ███████╗██████╗  █████╗ ██╗     ██╗   ██╗███████╗\n"
      printf "  ██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔══██╗██║     ██║   ██║██╔════╝\n"
      printf "  ██║  ██║███████║█████╗  ██║  ██║███████║██║     ██║   ██║███████╗\n"
      printf "  ██║  ██║██╔══██║██╔══╝  ██║  ██║██╔══██║██║     ██║   ██║╚════██║\n"
      printf "  ██████╔╝██║  ██║███████╗██████╔╝██║  ██║███████╗╚██████╔╝███████║\n"
      printf "  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═════╝ ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚══════╝''${reset}\n"
      printf "''${yellow}  ██████╗ ███████╗██╗     ███████╗ █████╗ ███████╗███████╗\n"
      printf "  ██╔══██╗██╔════╝██║     ██╔════╝██╔══██╗██╔════╝██╔════╝\n"
      printf "  ██████╔╝█████╗  ██║     █████╗  ███████║███████╗█████╗\n"
      printf "  ██╔══██╗██╔══╝  ██║     ██╔══╝  ██╔══██║╚════██║██╔══╝\n"
      printf "  ██║  ██║███████╗███████╗███████╗██║  ██║███████║███████╗\n"
      printf "  ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝''${reset}\n"
      printf "''${green}  ████████╗ ██████╗  ██████╗ ██╗\n"
      printf "  ╚══██╔══╝██╔═══██╗██╔═══██╗██║\n"
      printf "     ██║   ██║   ██║██║   ██║██║\n"
      printf "     ██║   ██║   ██║██║   ██║██║\n"
      printf "     ██║   ╚██████╔╝╚██████╔╝███████╗\n"
      printf "     ╚═╝    ╚═════╝  ╚═════╝ ╚══════╝''${reset}''${dim}  ops shell''${reset}\n"
      printf "\n"
      printf "  ''${bold}$(drt --version)''${reset}\n"
      printf "\n"
      printf "  ''${yellow}Workflow''${reset}\n"
      printf "  ''${green}1. fetch''${reset}    drt fetch-installers --url https://ci.iog.io/eval/<ID> --env <cluster> -o installers/<cluster>/\n"
      printf "  ''${green}2. sign''${reset}     drt sign installers/<cluster>/\n"
      printf "  ''${dim}            OSX_SIGN_HOST  WIN_SIGN_HOST  GPG_USER from .envrc.local''${reset}\n"
      printf "  ''${green}3. serve''${reset}    drt serve --installers installers/<cluster>/\n"
      printf "  ''${dim}            test locally before releasing''${reset}\n"
      printf "  ''${green}4. release''${reset}  drt release installers/<cluster>/ --bucket <bucket> --bucket-url <url>\n"
      printf "\n"
    '';

    # ── crane build (only when Cargo.lock is present) ────────────────────────
    drtBuild = lib.optionalAttrs cargoLockExists (let
      src = lib.fileset.toSource {
        root = ./../release-cli;
        fileset = lib.fileset.unions [
          ./../release-cli/Cargo.toml
          ./../release-cli/Cargo.lock
          ./../release-cli/src
        ];
      };
      commonArgs = {
        inherit src;
        strictDeps = true;
        pname = "drt";
        version = "0.1.0";
      };
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;
    in {
      packages.drt = craneLib.buildPackage (commonArgs
        // {
          inherit cargoArtifacts;
          meta = {
            description = "Daedalus release tool — hash, sign, upload, serve";
            mainProgram = "drt";
            license = lib.licenses.asl20;
          };
        });

      checks = {
        drt-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });
      };
    });
  in
    drtBuild
    // {
      devShells =
        (drtBuild.devShells or {})
        // {
          release-cli = pkgs.mkShell {
            packages = [
              toolchain
              config.treefmt.build.wrapper
              pkgs.gnupg
              pkgs.awscli2
              pkgs.cargo-watch
            ];
            shellHook = ''
              echo "drt release-cli devShell"
              echo "  build: cargo build --manifest-path release-cli/Cargo.toml"
              echo "  run:   cargo run --manifest-path release-cli/Cargo.toml -- --help"
            '';
          };
        }
        // lib.optionalAttrs cargoLockExists {
          ops = pkgs.mkShell {
            packages = [config.packages.drt pkgs.gnupg];
            shellHook = opsShellHook;
          };
        };
    };
}
