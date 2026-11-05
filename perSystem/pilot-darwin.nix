{...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    common,
    darwinBuild ? null,
    ...
  }:
    lib.mkIf (builtins.elem system ["aarch64-darwin" "x86_64-darwin"]) (let
      profiles = common.installerProducts.pilot.profiles;
      configurations = lib.genAttrs profiles (cluster:
        common.mkDaedalusConfigs {
          inherit cluster;
          product = "pilot";
        });
      source = darwinBuild.package.mainnet-flight;
      sourceName = (common.mkDaedalusConfigs {cluster = "mainnet-flight";}).installerConfig.spacedName;
      profileManifest =
        pkgs.writeText "pilot-darwin-profiles.tsv"
        (lib.concatMapStrings (profile: "profile\t${profile}\t${configurations.${profile}.installerConfig.spacedName}\n") profiles);
      # This is the only classification of shared versus profile-owned paths.
      payloadGroups = [
        {
          directory = "MacOS";
          exclude = lib.escapeShellArg sourceName;
        }
        {
          directory = "Frameworks";
          exclude = "${lib.escapeShellArg "${sourceName} Helper"}*.app";
        }
        {
          directory = "Resources/app";
          exclude = "package.json";
        }
      ];
      postinstall = pkgs.writeText "pilot-darwin-postinstall" ''
        #!/bin/bash
        set -euo pipefail
        target="''${3:-/}"
        exec "$BASH" "''${0%/*}/hardlink-payload" "''${target%/}/Applications/Daedalus Pilot"
      '';
      layout =
        pkgs.runCommand "daedalus-pilot-darwin-layout" {
          nativeBuildInputs = [pkgs.jq];
          passthru = {inherit profileManifest;};
        } ''
          set -eu
          product="$out/Applications/Daedalus Pilot"
          shared="$product/Shared"
          mkdir -p "$shared"
          cp -R '${source}/Applications/${sourceName}.app/Contents'/. "$shared/"
          chmod -R u+w "$shared"
          rm -rf "$shared/_CodeSignature"
          # A real profile app wraps only the small Electron entry executable.
          # Large frameworks, native addons and application code stay in Shared.
          test "$(${pkgs.coreutils}/bin/stat -c %s "$shared/MacOS/Frontend")" -lt 2097152
          ${lib.concatMapStringsSep "\n" (profile: let
              configuration = configurations.${profile};
              name = configuration.installerConfig.spacedName;
            in ''
              app="$product/${name}.app/Contents"
              mkdir -p "$app"/{MacOS,Resources/app}
              cp "$shared/Info.plist" "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleIdentifier org.daedalus.pilot.${profile}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleName ${name}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleDisplayName ${name}' "$app/Info.plist" || \
                /usr/libexec/PlistBuddy -c 'Add :CFBundleDisplayName string ${name}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleExecutable ${name}' "$app/Info.plist"
              bash ${../packaging/darwin/pilot-frameworks.sh} "$shared" "$app" \
                '${name}' 'org.daedalus.pilot.${profile}' '${sourceName}'
              cp ${darwinBuild.darwin-launcher}/bin/darwin-launcher "$app/MacOS/${name}"
              # Each shell selects one profile. Electron and the backends share
              # the same config/genesis directory beside the launcher config.
              cp -R ${configuration.configFiles}/. "$app/Resources/"
              # Common app contents are materialized only in a runnable view
              # or by postinstall. The base shell owns its package identity.
              jq --arg name '${name}' '.productName = $name' "$shared/Resources/app/package.json" > "$app/Resources/app/package.json"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleIconFile electron.icns' "$app/Info.plist"
              /usr/bin/iconutil --convert icns --output "$app/Resources/electron.icns" ${../installers/icons + "/${profile}.iconset"}
              cat > "$app/Resources/helper" <<'HELPER'
              #!/bin/sh
              set -eu
              mkdir -p "${configuration.installerConfig.dataDir}/Secrets-1.0" "${configuration.installerConfig.dataDir}/Logs/pub"
              HELPER
              chmod +x "$app/Resources/helper"
              # CI builds the unsigned installer. Bundle signing and notarization
              # belong to the release process.
            '')
            profiles}
          cp ${profileManifest} "$product/pilot-layout.tsv"
          chmod u+w "$product/pilot-layout.tsv"
          shopt -s dotglob nullglob
          ${lib.concatMapStringsSep "\n" (group: ''
              for item in "$shared/${group.directory}"/*; do
                basename=''${item##*/}
                case "$basename" in ${group.exclude}) continue ;; esac
                printf 'payload\t%s\n' "${group.directory}/$basename" >> "$product/pilot-layout.tsv"
              done
            '')
            payloadGroups}
          printf 'daedalus-pilot-prototype-v1\n' > "$product/pilot-owner"
        '';
      prototype =
        pkgs.runCommand "daedalus-pilot-darwin-prototype" {
          meta.mainProgram = "daedalus";
        } ''
          mkdir -p "$out"
          cp -R ${layout}/Applications "$out/"
          chmod -R u+w "$out"
          product="$out/Applications/Daedalus Pilot"
          bash ${../packaging/darwin/pilot-hardlink-payload.sh} "$product"
          mkdir -p "$out/bin"
          cat > "$out/bin/daedalus" <<'LAUNCH'
          #!/bin/sh
          set -eu
          case "''${1:-}" in
            ${lib.concatMapStringsSep "\n" (profile: "${lib.escapeShellArg profile}) name=${lib.escapeShellArg configurations.${profile}.installerConfig.spacedName} ;;") profiles}
            *) echo ${lib.escapeShellArg "Choose ${lib.concatStringsSep ", " profiles}."} >&2; exit 2 ;;
          esac
          root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
          exec "$root/Applications/Daedalus Pilot/$name.app/Contents/MacOS/$name"
          LAUNCH
          chmod +x "$out/bin/daedalus"
        '';
      installer =
        pkgs.runCommand "daedalus-pilot-prototype-pkg" {
          nativeBuildInputs = [pkgs.jq];
          passthru = {inherit postinstall;};
        } ''
          mkdir -p "$out"
          mkdir -p stage scripts
          # The canonical layout already contains Shared once and empty shells.
          cp -R ${layout}/Applications stage/
          chmod -R u+w stage
          cp ${../packaging/darwin/pilot-hardlink-payload.sh} scripts/hardlink-payload
          cp ${postinstall} scripts/postinstall
          cat > scripts/preinstall <<'PREINSTALL'
          #!/bin/sh
          if [ -e '/Applications/Daedalus Pilot' ]; then
            echo 'Close all Pilot profiles and remove the previous prototype before installing this test build.' >&2
            exit 1
          fi
          PREINSTALL
          chmod +x scripts/preinstall scripts/postinstall
          # Installer must keep the entire product together, even when it finds
          # matching bundle IDs in moved apps or extracted diagnostic copies.
          /usr/bin/pkgbuild --analyze --root stage components.plist
          /usr/bin/plutil -convert json -o components.json components.plist
          jq 'walk(if type == "object" and has("RootRelativeBundlePath") then .BundleIsRelocatable = false else . end)' \
            components.json > fixed-components.json
          /usr/bin/plutil -convert xml1 -o components.plist fixed-components.json
          /usr/bin/pkgbuild --root stage --component-plist components.plist --scripts scripts --identifier org.daedalus.pilot.prototype \
            --version ${common.originalPackageJson.version} --install-location / \
            "$out/daedalus-pilot-prototype-${system}.pkg"
          mkdir -p "$out/nix-support"
          printf 'file binary-dist "%s"\n' "$out/daedalus-pilot-prototype-${system}.pkg" > "$out/nix-support/hydra-build-products"
        '';
    in {
      checks.pilot-darwin-hardlink-layout =
        pkgs.runCommand "pilot-darwin-hardlink-layout" {
          nativeBuildInputs = [pkgs.diffutils];
        } ''
          /bin/bash ${../packaging/darwin/pilot-hardlink-payload-check.sh} \
            ${../packaging/darwin/pilot-hardlink-payload.sh} ${profileManifest} ${postinstall} \
            ${../packaging/darwin/pilot-layout-verify.sh} /bin/bash
          touch "$out"
        '';
      checks.pilot-darwin-package =
        pkgs.runCommand "pilot-darwin-package" {
          nativeBuildInputs = [pkgs.diffutils];
        } ''
          cp -R ${layout}/Applications expected
          /usr/sbin/pkgutil --expand-full ${installer}/daedalus-pilot-prototype-${system}.pkg expanded
          if [ "$(/usr/bin/xmllint --xpath 'count(/pkg-info/relocate/bundle)' expanded/PackageInfo)" != 0 ]; then
            echo 'Pilot bundles must not be relocated outside the product directory.' >&2
            exit 1
          fi
          mkdir -p 'target volume'
          mv expanded/Payload/Applications 'target volume/Applications'
          bash ${../packaging/darwin/pilot-layout-verify.sh} \
            "$PWD/target volume/Applications/Daedalus Pilot" "$PWD/expected/Daedalus Pilot" shells
          # Execute the wrapper shipped in the actual package, with Installer's
          # three arguments and a non-default target path containing spaces.
          /bin/bash expanded/Scripts/postinstall fixture.pkg / "$PWD/target volume"
          bash ${../packaging/darwin/pilot-layout-verify.sh} \
            "$PWD/target volume/Applications/Daedalus Pilot" "$PWD/expected/Daedalus Pilot"
          touch "$out"
        '';
      packages.daedalus-pilot-layout = layout;
      packages.daedalus-pilot-prototype = prototype;
      packages.installer-pilot-prototype = installer;
      packages.installer-pilot = installer;
    });
}
