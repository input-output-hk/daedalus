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
      ["daedalus-pilot-layout" "daedalus-pilot-prototype" "installer-pilot-prototype"];
      profileConfigs =
        pkgs.writeText "pilot-darwin-profile-configs.json"
        (builtins.unsafeDiscardStringContext (builtins.toJSON
          (lib.concatMap (targetSystem: let
            targetCommon = mkCommon targetSystem;
          in
            map (profile: {
              inherit targetSystem profile;
              config =
                builtins.fromJSON
                (targetCommon.mkDaedalusConfigs {
                  cluster = profile;
                  product = "pilot";
                })
                  .configFiles.daedalusConfigJson;
            })
            targetCommon.installerProducts.pilot.profiles)
          ["aarch64-darwin" "x86_64-darwin"])));
    in {
      checks.pilot-darwin-hardlink-layout = pkgs.runCommand "pilot-darwin-hardlink-layout" {} ''
        bash ${../packaging/darwin/pilot-hardlink-payload-check.sh} \
          ${../packaging/darwin/pilot-hardlink-payload.sh} \
          ${generated.content.packages.daedalus-pilot-layout.profileManifest} \
          ${generated.content.packages.installer-pilot.postinstall} \
          ${../packaging/darwin/pilot-layout-verify.sh}
        touch "$out"
      '';
      checks.pilot-darwin-config-paths =
        pkgs.runCommand "pilot-darwin-config-paths" {
          nativeBuildInputs = [pkgs.jq];
        } ''
          # Evaluate the actual configs without requiring Darwin payload builds.
          # Electron resolves its genesis beside Resources/daedalus-config.json;
          # node and wallet must use that same directory for every app shell.
          jq -e --arg root ${lib.escapeShellArg "\${DAEDALUS_INSTALL_DIRECTORY}/../Resources"} '
            length == 6 and all(.[];
              .config.node.args[2] == ($root + "/config.yaml") and
              .config.node.args[4] == ($root + "/topology.yaml") and
              (if .profile == "mainnet-flight"
               then (.config.wallet.args | index("--mainnet")) != null
               else (.config.wallet.args | index($root + "/genesis-byron.json")) != null
               end) and
              .config.dappNetwork.genesisFile == "genesis.json")
          ' ${profileConfigs}
          touch "$out"
        '';
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
