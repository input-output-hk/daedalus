# NixOS module for Daedalus system package support.
#
# Installs chrome-sandbox (0755) and sandbox-identity.json into
# /var/lib/daedalus/<cluster>/ via an activation script so Daedalus
# (running as a normal user) can read the manifest and the sandbox
# helper is world-readable at the right mode.
#
# Usage in NixOS configuration:
#   services.daedalus = {
#     enable = true;
#     clusters = [ "mainnet" ];
#     packages.mainnet = inputs.daedalus.packages.${system}."nixos-package-mainnet";
#   };
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.daedalus;

  mkClusterModule = cluster: let
    varDir = "/var/lib/daedalus/${cluster}";
    helperDest = "${varDir}/chrome-sandbox";
    manifestDest = "${varDir}/sandbox-identity.json";
    pkg = cfg.packages.${cluster};
  in {
    system.activationScripts."daedalus-sandbox-${cluster}" = {
      text = ''
        mkdir -p ${varDir}
        chmod 0755 ${varDir}
        chown root:root ${varDir}

        # Copy chrome-sandbox from package share/ (0555 in store) to mutable
        # location. Write manifest LAST — no window where manifest exists but
        # helper is still 0555 / not yet present.
        cp -f ${pkg}/share/chrome-sandbox ${helperDest}.tmp
        chown root:root ${helperDest}.tmp
        chmod 0755 ${helperDest}.tmp
        mv -f ${helperDest}.tmp ${helperDest}

        install -o root -g root -m 0644 ${pkg}/share/sandbox-identity.json ${manifestDest}
      '';
      deps = [];
    };

    environment.systemPackages = [pkg];
  };
in {
  options.services.daedalus = {
    enable = lib.mkEnableOption "Daedalus wallet";

    clusters = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["mainnet"];
      description = "Which Daedalus clusters to install.";
    };

    packages = lib.mkOption {
      type = lib.types.attrsOf lib.types.package;
      description = ''
        Per-cluster Daedalus NixOS packages. Set from the Daedalus flake output:
          services.daedalus.packages.mainnet =
            inputs.daedalus.packages.''${system}."nixos-package-mainnet";
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge (map mkClusterModule cfg.clusters)
  );
}
