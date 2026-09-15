{inputs, ...}: {
  perSystem = {
    system,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      internal = inputs.self.internal.x86_64-linux;
      inherit (internal) common originalPackageJson daedalusJs newPackage;
      inherit (common) sourceLib launcherConfigs;
      installerClusters = inputs.self.internal.installerClusters;
      genClusters = lib.genAttrs installerClusters;

      commonPackagingArgs = cluster: {
        inherit pkgs lib cluster;
        bundle = systemPackageBundle.${cluster};
        icon = launcherConfigs.${cluster}.installerConfig.iconPath.base + "/512x512.png";
        version = originalPackageJson.version;
        inherit (sourceLib) buildCounter buildRev buildRevShort;
        sourceDateEpoch = inputs.self.lastModified or sourceLib.daedalusEpoch;
      };

      systemPackageBundle = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus-system-package-bundle";
          dontUnpack = true;
          buildCommand = ''
            cp -r ${newPackage.${cluster}} $out
            chmod -R +w $out
            for symlink in $out/libexec/{daedalus-js,bundle-*} ; do
              target=$(readlink "$symlink")
              rm "$symlink"
              cp -r "$target" "$symlink"
            done
            find $out/libexec/daedalus-js/ -type f -iname '*.node' | while IFS= read -r file ; do
              chmod +w "$file"
              patchelf --set-rpath \
                "\$ORIGIN/$(realpath --relative-to="$(dirname "$file")" $out/libexec/bundle-electron/lib/electron/lib)" \
                "$file"
            done
            rm -rf $out/share/applications
            rm -f $out/share/icon_large.png
            rm -f $out/libexec/update-runner
          '';
        });

      debInstaller =
        genClusters (cluster:
          import ../../packaging/linux/deb.nix (commonPackagingArgs cluster));

      rpmInstaller =
        genClusters (cluster:
          import ../../packaging/linux/rpm.nix (commonPackagingArgs cluster));

      archInstaller =
        genClusters (cluster:
          import ../../packaging/linux/arch.nix (commonPackagingArgs cluster));

      nixosPackage = genClusters (cluster: let
        nixosElectronBin = "${pkgs.electron}/libexec/electron/electron";
        helperSha256 = builtins.hashFile "sha256" "${pkgs.electron}/libexec/electron/chrome-sandbox";
        manifest = pkgs.writeText "sandbox-identity-${cluster}.json" (builtins.toJSON {
          schemaVersion = 2;
          packageFamily = "nix";
          matrixRevision = "task-112-matrix-2026-09-11";
          matrixRow = "nixos-26.05";
          helper = {
            mode = "0755";
            sha256 = helperSha256;
          };
          launch = {
            electron = nixosElectronBin;
          };
        });
      in
        pkgs.stdenv.mkDerivation {
          name = "daedalus-nixos-${cluster}";
          meta.mainProgram = "daedalus-${cluster}";
          dontUnpack = true;
          buildCommand = ''
            mkdir -p $out/{bin,libexec,config,share}

            cp -r ${launcherConfigs.${cluster}.configFiles}/. $out/config/

            # daedalus-bridge is a static musl binary — no nix-bundle-exe needed on NixOS.
            ln -sf ${common.daedalus-bridge.${cluster}} $out/libexec/bundle-daedalus-bridge
            ( cd $out/libexec/ && ln -sf bundle-daedalus-bridge/bin/* ./ ; )

            ln -sf ${daedalusJs.${cluster}}/share/daedalus $out/libexec/daedalus-js

            cp ${pkgs.writeText "daedalus-${cluster}-launcher" ''
              #!/bin/sh
              set -e

              if [ -n "$LD_LIBRARY_PATH" ]; then
                echo >&2 "Warning: 'LD_LIBRARY_PATH' is set, unsetting it."
                unset LD_LIBRARY_PATH
              fi

              ENTRYPOINT_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")"
              export ENTRYPOINT_DIR
              export PATH="$ENTRYPOINT_DIR/libexec:$PATH"

              XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
              export CLUSTER=${cluster}
              export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
              export DAEDALUS_CONFIG="$ENTRYPOINT_DIR/config"

              # Point Chromium at the activation-installed mutable copy (0755); the Nix
              # store copy is 0555 and cannot be used directly as the SUID/userns helper.
              export CHROME_DEVEL_SANDBOX="/var/lib/daedalus/${cluster}/chrome-sandbox"

              mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Logs/pub
              mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Secrets
              cd "''${DAEDALUS_DIR}/${cluster}/"

              exec cardano-launcher --config "$ENTRYPOINT_DIR/config/launcher-config.yaml"
            ''} $out/bin/daedalus-${cluster}

            cp ${pkgs.writeText "daedalus-frontend-nixos-${cluster}" ''
              #!/bin/sh
              set -xe
              # daedalus-frontend execs the real electron binary directly so
              # process.execPath matches manifest.launch.electron exactly.
              # shellcheck disable=SC2086
              exec ${nixosElectronBin} ''${DAEDALUS_ELECTRON_FLAGS-} "$ENTRYPOINT_DIR"/libexec/daedalus-js "$@"
            ''} $out/libexec/daedalus-frontend

            chmod +x $out/bin/* $out/libexec/daedalus-frontend

            # Bundle manifest and chrome-sandbox so the activation script always copies
            # from the same derivation that built the package (no version skew).
            cp ${manifest} $out/share/sandbox-identity.json
            cp ${pkgs.electron}/libexec/electron/chrome-sandbox $out/share/chrome-sandbox
          '';
        });

      nixosSandboxSetup = genClusters (cluster:
        pkgs.writeShellApplication {
          name = "nixos-sandbox-setup-${cluster}";
          runtimeInputs = [];
          text = ''
            if [ "$(id -u)" != "0" ]; then
              echo "error: nixos-sandbox-setup must be run as root" >&2
              exit 1
            fi

            varDir="/var/lib/daedalus/${cluster}"
            mkdir -p "$varDir"
            chmod 0755 "$varDir"
            chown root:root "$varDir"

            # Copy chrome-sandbox from package share/ (0555 in store) to mutable
            # location. Write manifest LAST — no window where manifest exists but
            # helper is still 0555 / not yet present.
            cp -f ${nixosPackage.${cluster}}/share/chrome-sandbox "$varDir/chrome-sandbox.tmp"
            chown root:root "$varDir/chrome-sandbox.tmp"
            chmod 0755 "$varDir/chrome-sandbox.tmp"
            mv -f "$varDir/chrome-sandbox.tmp" "$varDir/chrome-sandbox"

            install -o root -g root -m 0644 \
              ${nixosPackage.${cluster}}/share/sandbox-identity.json \
              "$varDir/sandbox-identity.json"

            echo "Daedalus NixOS sandbox setup complete for cluster: ${cluster}"
          '';
        });
    in {
      packages = lib.listToAttrs (lib.concatMap (cluster: [
          {
            name = "deb-installer-${cluster}";
            value = debInstaller.${cluster};
          }
          {
            name = "rpm-installer-${cluster}";
            value = rpmInstaller.${cluster};
          }
          {
            name = "arch-installer-${cluster}";
            value = archInstaller.${cluster};
          }
          {
            name = "nixos-package-${cluster}";
            value = nixosPackage.${cluster};
          }
          {
            name = "nixos-sandbox-setup-${cluster}";
            value = nixosSandboxSetup.${cluster};
          }
        ])
        installerClusters);
    });
}
