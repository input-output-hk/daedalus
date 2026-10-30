{...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    config,
    linuxPackaging ? null,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      inherit (linuxPackaging) mkDebInstaller mkRpmInstaller mkArchInstaller pilotPackagingArgs pilotLaunchers;
      fixture = pkgs.runCommand "pilot-package-fixture" {} ''
        mkdir -p "$out"/{bin,share,libexec/daedalus-js/main,libexec/bundle-electron/lib/electron}
        cp ${pkgs.hello}/bin/hello "$out/libexec/bundle-electron/lib/electron/electron"
        cp ${pkgs.hello}/bin/hello "$out/libexec/bundle-electron/lib/electron/chrome-sandbox"
        cp -L ${pkgs.stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 "$out/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2"
        printf 'fixture\n' > "$out/libexec/daedalus-js/main/dapp.js"
        cp -r ${config.packages.pilot-profile-configs} "$out/config"
      '';
      arguments =
        pilotPackagingArgs
        // {
          bundle = fixture;
          buildRev = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
          buildRevShort = "aaaaaaaaa";
          buildCounter = 1;
        };
      deb = mkDebInstaller (arguments // {debCompression = "-Zzstd -z1";});
      rpm = mkRpmInstaller arguments;
      arch = mkArchInstaller (arguments // {zstdLevel = 1;});
      packageContract = {
        deb,
        rpm,
        arch,
      }: name:
        pkgs.runCommand name {
          nativeBuildInputs = [pkgs.dpkg pkgs.rpm pkgs.libarchive pkgs.jq pkgs.desktop-file-utils pkgs.patchelf];
        } (import ../packaging/linux/package-contract-check.nix {inherit deb rpm arch;});
    in {
      checks.pilot-linux-launchers =
        pkgs.runCommand "pilot-linux-launchers" {
          nativeBuildInputs = [pkgs.desktop-file-utils pkgs.shellcheck];
        } ''
          set -eu
          desktop_root="$TMPDIR/stage"
          root="$desktop_root/opt/daedalus/pilot"
          mkdir -p "$root/libexec"
          ${pilotLaunchers}
          for profile in preview preprod mainnet-flight; do
            desktop-file-validate "$desktop_root/usr/share/applications/Daedalus-$profile.desktop"
            grep -Fx "Exec=/opt/daedalus/pilot/bin/daedalus-$profile" "$desktop_root/usr/share/applications/Daedalus-$profile.desktop"
            shellcheck "$root/bin/daedalus-$profile" "$desktop_root/usr/bin/daedalus-$profile"
          done
          shellcheck "$root/bin/daedalus" "$root/bin/refuse-live" "$root/libexec/daedalus-frontend"
          # Relocate only the installation prefix to exercise the actual generated
          # launchers without installing anything into the host's /opt or /usr.
          substituteInPlace "$root"/bin/* "$root/libexec/daedalus-frontend" \
            --replace-fail /opt/daedalus/pilot "$root"
          cat >"$root/libexec/cardano-watchdog" <<'WATCHDOG'
          #!/bin/sh
          printf '%s\n' "$PWD" "$CLUSTER" "$DAEDALUS_CONFIG" "$ENTRYPOINT_DIR" "$@" > "$CAPTURE"
          WATCHDOG
          cat >"$root/libexec/electron" <<'ELECTRON'
          #!/bin/sh
          printf '%s\n' "$DAEDALUS_WINDOW_ICON" "$@" > "$CAPTURE"
          ELECTRON
          chmod +x "$root/libexec/cardano-watchdog" "$root/libexec/electron"
          export XDG_DATA_HOME="$TMPDIR/data with spaces"
          export CAPTURE="$TMPDIR/arguments"
          for profile in preview preprod mainnet-flight; do
            "$root/bin/daedalus-$profile"
            test "$(sed -n '1p' "$CAPTURE")" = "$XDG_DATA_HOME/Daedalus/$profile"
            test "$(sed -n '2p' "$CAPTURE")" = "$profile"
            test "$(sed -n '3p' "$CAPTURE")" = "$root/config/profiles/$profile"
            grep -Fx "$root/config/profiles/$profile/daedalus-config.json" "$CAPTURE"
            grep -Fx "$XDG_DATA_HOME/Daedalus/$profile/tls" "$CAPTURE"
            CLUSTER="$profile" "$root/libexec/daedalus-frontend"
            grep -Fx "/usr/share/icons/hicolor/512x512/apps/daedalus-$profile.png" "$CAPTURE"
            grep -Fx -- "--class=Daedalus-$profile" "$CAPTURE"
          done
          if "$root/bin/daedalus"; then exit 1; fi
          if "$root/bin/daedalus" mainnet; then exit 1; fi
          if CLUSTER=mainnet "$root/libexec/daedalus-frontend"; then exit 1; fi
          "$root/bin/daedalus" preview
          "$root/bin/refuse-live"
          cp ${pkgs.coreutils}/bin/sleep "$root/libexec/sleep"
          "$root/libexec/sleep" 30 &
          live_pid=$!
          trap 'kill "$live_pid" 2>/dev/null || true' EXIT
          for attempt in $(seq 1 100); do
            test "$(readlink "/proc/$live_pid/exe" 2>/dev/null || true)" != "$root/libexec/sleep" || break
            sleep 0.01
          done
          if "$root/bin/refuse-live"; then exit 1; fi
          kill "$live_pid"
          wait "$live_pid" || true
          trap - EXIT
          "$root/bin/refuse-live"
          touch "$out"
        '';

      checks.pilot-linux-package-contract = packageContract {inherit deb rpm arch;} "pilot-linux-package-contract";
      checks.pilot-linux-artifacts = packageContract {
        deb = config.packages.deb-dev-installer-pilot;
        rpm = config.packages.rpm-installer-pilot;
        arch = config.packages.arch-dev-installer-pilot;
      } "pilot-linux-artifacts";
    });
}
