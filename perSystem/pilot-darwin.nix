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
      prototype =
        pkgs.runCommand "daedalus-pilot-darwin-prototype" {
          nativeBuildInputs = [pkgs.jq];
          meta.mainProgram = "daedalus";
        } ''
          set -eu
          product="$out/Applications/Daedalus Pilot"
          shared="$product/Shared"
          mkdir -p "$shared" "$out/bin"
          cp -R '${source}/Applications/Daedalus Flight.app/Contents'/. "$shared/"
          chmod -R u+w "$shared"
          rm -rf "$shared/_CodeSignature"
          # A real profile app wraps only the small Electron entry executable.
          # All frameworks, native addons and application code stay in Shared.
          test "$(${pkgs.coreutils}/bin/stat -c %s "$shared/MacOS/Frontend")" -lt 2097152
          ${lib.concatMapStringsSep "\n" (profile: let
              configuration = configurations.${profile};
              name = configuration.installerConfig.spacedName;
            in ''
              app="$product/${name}.app/Contents"
              mkdir -p "$app"/{MacOS,Resources/app,Resources/profiles/${profile}}
              cp "$shared/Info.plist" "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleIdentifier org.daedalus.pilot.${profile}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleName ${name}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleDisplayName ${name}' "$app/Info.plist" || \
                /usr/libexec/PlistBuddy -c 'Add :CFBundleDisplayName string ${name}' "$app/Info.plist"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleExecutable ${name}' "$app/Info.plist"
              ln -s ../../Shared/Frameworks "$app/Frameworks"
              for item in "$shared/MacOS"/*; do
                basename=$(basename "$item")
                case "$basename" in Frontend|'Daedalus Flight') continue ;; esac
                ln -s "../../../Shared/MacOS/$basename" "$app/MacOS/$basename"
              done
              cp "$shared/MacOS/Frontend" "$app/MacOS/Frontend"
              cp ${darwinBuild.darwin-launcher}/bin/darwin-launcher "$app/MacOS/${name}"
              cp -R ${configuration.configFiles}/. "$app/Resources/profiles/${profile}/"
              cp "$app/Resources/profiles/${profile}/daedalus-config.json" "$app/Resources/daedalus-config.json"
              # Keep a real app directory and per-profile package identity; its
              # large children link back to the one shared JavaScript payload.
              for item in "$shared/Resources/app"/*; do
                basename=$(basename "$item")
                test "$basename" = package.json || ln -s "../../../../Shared/Resources/app/$basename" "$app/Resources/app/$basename"
              done
              jq --arg name '${name}' '.productName = $name' "$shared/Resources/app/package.json" > "$app/Resources/app/package.json"
              /usr/libexec/PlistBuddy -c 'Set :CFBundleIconFile electron.icns' "$app/Info.plist"
              /usr/bin/iconutil --convert icns --output "$app/Resources/electron.icns" ${../installers/icons + "/${profile}.iconset"}
              cat > "$app/Resources/helper" <<'HELPER'
              #!/bin/sh
              set -eu
              mkdir -p "${configuration.installerConfig.dataDir}/Secrets-1.0" "${configuration.installerConfig.dataDir}/Logs/pub"
              HELPER
              chmod +x "$app/Resources/helper"
              /usr/bin/codesign --force --sign - --options runtime --entitlements ${../installers/entitlements.xml} "$product/${name}.app"
              /usr/bin/codesign --verify --deep --strict "$product/${name}.app"
            '')
            profiles}
          cat > "$out/bin/daedalus" <<'LAUNCH'
          #!/bin/sh
          set -eu
          case "''${1:-}" in
            preview) name='Daedalus Preview' ;;
            preprod) name='Daedalus Pre-Prod' ;;
            mainnet-flight) name='Daedalus Flight' ;;
            *) echo 'Choose preview, preprod or mainnet-flight.' >&2; exit 2 ;;
          esac
          root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
          exec "$root/Applications/Daedalus Pilot/$name.app/Contents/MacOS/$name"
          LAUNCH
          chmod +x "$out/bin/daedalus"
          printf 'daedalus-pilot-prototype-v1\n' > "$product/pilot-owner"
        '';
      installer = pkgs.runCommand "daedalus-pilot-prototype-pkg" {} ''
        mkdir -p "$out"
        mkdir -p stage scripts
        cp -R ${prototype}/Applications stage/
        cat > scripts/preinstall <<'PREINSTALL'
        #!/bin/sh
        if [ -e '/Applications/Daedalus Pilot' ]; then
          echo 'Close all Pilot profiles and remove the previous prototype before installing this test build.' >&2
          exit 1
        fi
        PREINSTALL
        chmod +x scripts/preinstall
        /usr/bin/pkgbuild --root stage --scripts scripts --identifier org.daedalus.pilot.prototype \
          --version ${common.originalPackageJson.version} --install-location / \
          "$out/daedalus-pilot-prototype-${system}.pkg"
        mkdir -p "$out/nix-support"
        printf 'file binary-dist "%s"\n' "$out/daedalus-pilot-prototype-${system}.pkg" > "$out/nix-support/hydra-build-products"
      '';
    in {
      packages.daedalus-pilot-prototype = prototype;
      packages.installer-pilot-prototype = installer;
    });
}
