{...}: {
  perSystem = {
    system,
    pkgs,
    lib,
    mkCommon,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      # Generate the same scripts with macOS configuration paths, while replacing
      # native payload inputs. Syntax checking must not build or execute them.
      generated = (import ./pilot-darwin.nix {}).perSystem {
        system = "aarch64-darwin";
        inherit pkgs lib;
        common = mkCommon "aarch64-darwin";
        darwinBuild = {
          package.mainnet-flight = "/fixture-payload";
          darwin-launcher = "/fixture-launcher";
        };
      };
      scripts = map (name:
        pkgs.writeText "${name}-syntax.sh"
        (builtins.unsafeDiscardStringContext generated.content.packages.${name}.buildCommand))
      ["daedalus-pilot-prototype" "installer-pilot-prototype"];
    in {
      checks.pilot-darwin-helper-layout =
        pkgs.runCommand "pilot-darwin-helper-layout" {
          nativeBuildInputs = [pkgs.jq];
        } ''
          bash ${../packaging/darwin/pilot-frameworks-check.sh} ${../packaging/darwin/pilot-frameworks.sh}
          touch "$out"
        '';
      checks.pilot-darwin-script-syntax = pkgs.runCommand "pilot-darwin-script-syntax" {} ''
        set -eu
        for script in ${lib.escapeShellArgs (map toString scripts)}; do
          bash -n "$script" 2> diagnostics
          cat diagnostics
          test ! -s diagnostics
        done
        touch "$out"
      '';
    });
}
