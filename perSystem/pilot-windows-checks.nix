{...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    config,
    common,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") {
      packages.windows-pilot-archive-check =
        pkgs.runCommand "windows-pilot-archive-check" {
          nativeBuildInputs = [pkgs._7zz pkgs.jq];
        } ''
          set -eu
          installer=$(printf '%s\n' ${config.packages.installer-x86_64-windows-pilot}/*.exe)
          7zz x "$installer" -opayload > extract.log
          root="$PWD/payload"
          test -f "$root/Daedalus Pilot.exe"
          test -f "$root/cardano-watchdog.exe"
          test -f "$root/resources/app/launch.cjs"
          test -f "$root/resources/shared-app/dist/main/index.js"
          test ! -f "$root/daedalus-config.json"
          test ! -f "$root/Daedalus Preview.exe"
          for profile in preview preprod mainnet-flight; do
            selected="$root/config/profiles/$profile"
            test -f "$selected/daedalus-config.json"
            test -f "$selected/frontend/bootstrap.cjs"
            test -f "$root/icons/$profile.ico"
            test ! -e "$selected/frontend/node_modules"
            jq -e '.main == "bootstrap.cjs"' "$selected/frontend/package.json"
          done
          touch "$out"
        '';
      checks.pilot-windows-configs =
        pkgs.runCommand "pilot-windows-configs-check" {
          nativeBuildInputs = [pkgs.jq common.nodejs pkgs.nsis];
        } ''
          set -eu
          bundle=${config.packages.windows-pilot-configs}
          node ${../packaging/windows/pilot-bootstrap.spec.cjs} ${../packaging/windows/pilot-bootstrap.cjs} "$bundle"
          for profile in preview preprod mainnet-flight; do
            selected="$bundle/config/profiles/$profile"
          jq -e --arg profile "$profile" \
            --arg root "\''${DAEDALUS_INSTALL_DIRECTORY}\\config\\profiles\\" \
            --arg settings "--user-data-dir=\''${APPDATA}\\" \
            --arg name "$(jq -r .productName "$selected/frontend/package.json")" '
            .electron.args[1] == ($root + $profile + "\\frontend") and
            .electron.args[0] == ($settings + $name) and
              .electron.env.DAEDALUS_UPDATE_MODE == "system-package-disabled"' "$selected/daedalus-config.json"
            node --check "$selected/frontend/bootstrap.cjs"
            grep -F "org.daedalus.pilot.$profile" "$selected/frontend/bootstrap.cjs"
          done
          mkdir -p work/installers
          cd work/installers
          ln -s ${../installers/nsis_plugins} nsis_plugins
          cp "$bundle"/*.nsi .
          # Compile the actual NSIS templates against a tiny payload, independently
          # of native Node addon/Wine builds. No installer is executed here.
          for binary in cardano-node cardano-wallet cardano-watchdog cardano-address cardano-cli mithril-client snapshot-converter uninstall; do touch "$binary.exe"; done
          mkdir -p '../release/win32-x64/Daedalus Pilot-win32-x64/resources' config profile-icons
          cp -r "$bundle/config"/. config/
          touch '../release/win32-x64/Daedalus Pilot-win32-x64/resources/fixture'
          touch profile-icons/fixture
          makensis uninstaller.nsi > uninstaller.log
          makensis daedalus.nsi > installer.log
          test -s tempinstaller.exe
          touch "$out"
        '';
    };
}
