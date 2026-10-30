{...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    config,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") {
      checks.pilot-nixos-package =
        pkgs.runCommand "pilot-nixos-package-check" {
          nativeBuildInputs = [pkgs.jq pkgs.shellcheck pkgs.desktop-file-utils];
        } ''
          set -eu
          package=${config.packages.nixos-package-pilot}
          manifest="$package/share/sandbox-identity.json"
          electron="$package/libexec/bundle-electron/lib/electron/electron"
          test -f "$electron" && test ! -L "$electron"
          jq -e --arg electron "$electron" \
            '.cluster == "pilot" and .packageFamily == "nix" and
             .launch.electron == $electron and .helper.mode == "0755"' "$manifest"
          test "$(jq -r '.helper.sha256' "$manifest")" = "$(sha256sum "$package/share/chrome-sandbox" | cut -d' ' -f1)"
          test -L "$package/libexec/daedalus-js"
          test -x "$package/libexec/cardano-watchdog"
          test ! -e "$package/config/daedalus-config.json"
          test ! -e "$package/usr"
          shellcheck "$package"/bin/* "$package/libexec/daedalus-frontend"
          if "$package/bin/daedalus"; then exit 1; fi
          if "$package/bin/daedalus" mainnet; then exit 1; fi

          # Exercise the generated launchers with only the immutable store prefix
          # relocated. Stub process boundaries so no desktop or backend is started.
          root="$TMPDIR/pilot"
          mkdir -p "$root/bin" "$root/libexec/bundle-electron/lib/electron"
          cp "$package"/bin/* "$root/bin/"
          cp "$package/libexec/daedalus-frontend" "$root/libexec/"
          chmod -R u+w "$root"
          substituteInPlace "$root"/bin/* "$root/libexec/daedalus-frontend" \
            --replace-fail "$package" "$root"
          cat > "$root/libexec/cardano-watchdog" <<'WATCHDOG'
          #!/bin/sh
          printf '%s\n' "$PWD" "$CLUSTER" "$ENTRYPOINT_DIR" "$DAEDALUS_CONFIG" "$CHROME_DEVEL_SANDBOX" "$XDG_CONFIG_HOME" "$@" > "$CAPTURE"
          WATCHDOG
          cat > "$root/libexec/bundle-electron/lib/electron/electron" <<'ELECTRON'
          #!/bin/sh
          printf '%s\n' "$DAEDALUS_WINDOW_ICON" "$@" > "$CAPTURE"
          ELECTRON
          chmod +x "$root/libexec/cardano-watchdog" "$root/libexec/bundle-electron/lib/electron/electron"
          export CAPTURE="$TMPDIR/arguments"
          export XDG_DATA_HOME="$TMPDIR/data with spaces"
          export XDG_CONFIG_HOME="$TMPDIR/config with spaces"
          for profile in preview preprod mainnet-flight; do
            desktop="$package/share/applications/Daedalus-$profile.desktop"
            desktop-file-validate "$desktop"
            grep -Fx "Exec=$package/bin/daedalus-$profile" "$desktop"
            test -f "$package/config/profiles/$profile/daedalus-config.json"
            "$root/bin/daedalus" "$profile"
            test "$(sed -n '1p' "$CAPTURE")" = "$XDG_DATA_HOME/Daedalus/$profile"
            test "$(sed -n '2p' "$CAPTURE")" = "$profile"
            test "$(sed -n '3p' "$CAPTURE")" = "$root"
            test "$(sed -n '4p' "$CAPTURE")" = "$root/config/profiles/$profile"
            test "$(sed -n '5p' "$CAPTURE")" = /var/lib/daedalus/pilot/chrome-sandbox
            test "$(sed -n '6p' "$CAPTURE")" = "$XDG_CONFIG_HOME"
            grep -Fx "$root/config/profiles/$profile/daedalus-config.json" "$CAPTURE"
            grep -Fx "$XDG_DATA_HOME/Daedalus/$profile/tls" "$CAPTURE"
            CLUSTER="$profile" DAEDALUS_ELECTRON_FLAGS='--disable-gpu --ozone-platform=x11' "$root/libexec/daedalus-frontend"
            grep -Fx "$root/share/icons/hicolor/512x512/apps/daedalus-$profile.png" "$CAPTURE"
            grep -Fx -- "--class=Daedalus-$profile" "$CAPTURE"
            grep -Fx -- --disable-gpu "$CAPTURE"
            grep -Fx -- --ozone-platform=x11 "$CAPTURE"
          done
          if CLUSTER=mainnet "$root/libexec/daedalus-frontend"; then exit 1; fi
          touch "$out"
        '';
    };
}
