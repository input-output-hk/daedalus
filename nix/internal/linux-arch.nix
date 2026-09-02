{
  pkgs,
  lib,
  cluster,
  bundle,
  icon,
  version,
  buildCounter,
  buildRev,
  buildRevShort,
  sourceDateEpoch,
}: let
  installRoot = "/opt/daedalus/${cluster}";
  packageName = "daedalus-${cluster}";
  revision =
    if builtins.match "[0-9a-f]{9}" buildRevShort != null
    then buildRevShort
    else lib.substring 0 9 (builtins.hashString "sha256" (toString bundle));
in
  pkgs.runCommand "arch-installer-${cluster}" {
    nativeBuildInputs = [
      pkgs.coreutils
      pkgs.fakeroot
      pkgs.file
      pkgs.findutils
      pkgs.gnugrep
      pkgs.gnused
      pkgs.jq
      pkgs.libarchive
      pkgs.pacman
      pkgs.patchelf
      pkgs.yq-go
      pkgs.zstd
    ];
    SOURCE_DATE_EPOCH = toString sourceDateEpoch;
  } ''
    set -eu
    work="$TMPDIR/makepkg"
    payload="$work/payload"
    root="$payload${installRoot}"
    mkdir -p "$root" "$work/home" "$work/pkgdest"

    cp -a ${bundle}/. "$root/"
    chmod -R u+w "$root"
    find "$root" -type l -print0 | while IFS= read -r -d $'\0' link; do
      target=$(readlink "$link")
      case "$target" in
        /nix/store/*)
          test -e "$target"
          rm "$link"
          cp -aL "$target" "$link"
          ;;
      esac
    done
    if find "$root" -type l -lname '/nix/store/*' -print -quit | grep -q .; then
      echo 'Nix-store symlink remains in Arch payload' >&2
      exit 1
    fi
    test ! -e "$root/libexec/.patchelf-static"
    test ! -e "$root/libexec/update-runner"
    test ! -e "$root/share/icon_large.png"
    test ! -e "$root/share/applications"

    electron="$root/libexec/bundle-electron/lib/electron/electron"
    helper="$root/libexec/bundle-electron/lib/electron/chrome-sandbox"
    test -f "$electron" -a ! -L "$electron"
    test -f "$helper" -a ! -L "$helper"
    patchelf --set-interpreter '${installRoot}/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2' "$electron"
    helper_sha=$(sha256sum "$helper" | cut -d' ' -f1)
    chmod 0755 "$helper"

    cat >"$root/bin/daedalus" <<'EOF'
    #!/bin/sh
    set -eu
    unset LD_LIBRARY_PATH ENTRYPOINT_DIR
    export CLUSTER='${cluster}'
    export DAEDALUS_CONFIG='${installRoot}/config'
    export ENTRYPOINT_DIR='${installRoot}'
    export CHROME_DEVEL_SANDBOX='${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox'
    XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    mkdir -p "''${DAEDALUS_DIR}/${cluster}/Logs/pub" "''${DAEDALUS_DIR}/${cluster}/Secrets"
    cd "''${DAEDALUS_DIR}/${cluster}"
    exec '${installRoot}/libexec/cardano-launcher' --config '${installRoot}/config/launcher-config.yaml'
    EOF
    cat >"$root/libexec/daedalus-frontend" <<'EOF'
    #!/bin/sh
    set -eu
    exec '${installRoot}/libexec/electron' '${installRoot}/libexec/daedalus-js' "$@"
    EOF
    rm -f "$root/libexec/electron"
    cat >"$root/libexec/electron" <<'EOF'
    #!/bin/sh
    set -eu
    if [ -z "''${XCURSOR_PATH:-}" ] && [ -d /usr/share/icons ]; then export XCURSOR_PATH=/usr/share/icons; fi
    export XKB_CONFIG_ROOT='${installRoot}/libexec/bundle-electron/share/X11/xkb'
    export GBM_BACKENDS_PATH='${installRoot}/libexec/bundle-electron/lib/electron/lib'
    exec '${installRoot}/libexec/bundle-electron/lib/electron/electron' "$@"
    EOF
    chmod 0755 "$root/bin/daedalus" "$root/libexec/daedalus-frontend" "$root/libexec/electron"

    yq -i '
      .applicationUpdateMode = "system-package-disabled"
      | del(.updateRunnerBin)
      | .daedalusBin = "${installRoot}/libexec/daedalus-frontend"
      | .nodeBin = "${installRoot}/libexec/cardano-node"
      | .cliBin = "${installRoot}/libexec/cardano-cli"
      | .walletBin = "${installRoot}/libexec/cardano-wallet"
      | .cardanoAddressBin = "${installRoot}/libexec/cardano-address"
    ' "$root/config/launcher-config.yaml"
    if yq -e 'has("selfnodeBin")' "$root/config/launcher-config.yaml" >/dev/null 2>&1; then
      yq -i '.selfnodeBin = "${installRoot}/libexec/local-cluster"' "$root/config/launcher-config.yaml"
    fi
    if yq -e 'has("mockTokenMetadataServerBin")' "$root/config/launcher-config.yaml" >/dev/null 2>&1; then
      yq -i '.mockTokenMetadataServerBin = "${installRoot}/libexec/mock-token-metadata-server"' "$root/config/launcher-config.yaml"
    fi

    mkdir -p \
      "$payload/usr/bin" \
      "$payload/usr/share/applications" \
      "$payload/usr/share/icons/hicolor/512x512/apps" \
      "$payload/usr/share/libalpm/hooks" \
      "$payload/usr/share/libalpm/scripts"
    cat >"$payload/usr/bin/daedalus-${cluster}" <<'EOF'
    #!/bin/sh
    exec '${installRoot}/bin/daedalus' "$@"
    EOF
    chmod 0755 "$payload/usr/bin/daedalus-${cluster}"
    cat >"$payload/usr/share/libalpm/scripts/daedalus-${cluster}-refuse-live" <<'EOF'
    #!/bin/sh
    set -eu
    electron='${installRoot}/libexec/bundle-electron/lib/electron/electron'
    for proc in /proc/[0-9]*; do
      [ -L "$proc/exe" ] || continue
      if [ "$(readlink -f "$proc/exe" 2>/dev/null || true)" = "$electron" ]; then
        echo 'Daedalus ${cluster} must be stopped before the package can be changed.' >&2
        exit 1
      fi
    done
    EOF
    chmod 0755 "$payload/usr/share/libalpm/scripts/daedalus-${cluster}-refuse-live"
    cat >"$payload/usr/share/libalpm/hooks/daedalus-${cluster}-refuse-live.hook" <<EOF
    [Trigger]
    Operation = Upgrade
    Operation = Remove
    Type = Package
    Target = ${packageName}

    [Action]
    Description = Checking that Daedalus ${cluster} is stopped...
    When = PreTransaction
    Exec = /usr/share/libalpm/scripts/daedalus-${cluster}-refuse-live
    AbortOnFail
    EOF
    cat >"$payload/usr/share/applications/Daedalus-${cluster}.desktop" <<EOF
    [Desktop Entry]
    Type=Application
    Name=Daedalus ${cluster}
    GenericName=Crypto-Currency Wallet
    Exec=${installRoot}/bin/daedalus
    Icon=daedalus-${cluster}
    Categories=Application;Network;
    StartupWMClass=Daedalus ${cluster}
    Terminal=false
    EOF
    install -m 0644 ${icon} "$payload/usr/share/icons/hicolor/512x512/apps/daedalus-${cluster}.png"

    files_json=$(for item in launcher:bin/daedalus frontend:libexec/daedalus-frontend wrapper:libexec/electron electron:libexec/bundle-electron/lib/electron/electron chromeSandbox:libexec/bundle-electron/lib/electron/chrome-sandbox; do
      name=''${item%%:*}
      relative=''${item#*:}
      jq -n --arg name "$name" --arg sha "$(sha256sum "$root/$relative" | cut -d' ' -f1)" '{key:$name,value:{sha256:$sha}}'
    done | jq -s 'from_entries')
    jq -n \
      --arg buildRev '${buildRev}' \
      --arg helperSha "$helper_sha" \
      --argjson files "$files_json" \
      '{
        schemaVersion:2,
        packageFamily:"arch",
        matrixRevision:"task-111-matrix-2026-09-02",
        matrixRow:"arch-2026.09.01",
        supportState:"supported",
        reason:"supported",
        distribution:{id:"arch",versionId:"2026.09.01",buildId:"rolling",kernelRelease:"7.2.2-arch1-1"},
        cluster:"${cluster}",
        sourceRevision:(if $buildRev == "0000000000000000000000000000000000000000" then null else $buildRev end),
        sourceDirty:($buildRev == "0000000000000000000000000000000000000000"),
        helper:{mode:"0755",sha256:$helperSha},
        policy:{kind:"none"},
        sandbox:{mode:"userns-only"},
        launch:{launcher:"${installRoot}/bin/daedalus",electron:"${installRoot}/libexec/bundle-electron/lib/electron/electron"},
        files:$files
      }' >"$root/share/daedalus-sandbox-identity.base.json"
    chmod 0644 "$root/share/daedalus-sandbox-identity.base.json"

    sed 's|@CLUSTER@|${cluster}|g' ${./linux-arch.install} >"$work/${packageName}.install"
    chmod 0755 "$work/${packageName}.install"
    cat >"$work/PKGBUILD" <<EOF
    pkgname=${packageName}
    pkgver=${version}
    pkgrel=1
    pkgdesc='Daedalus full-node cryptocurrency wallet (${cluster})'
    arch=('x86_64')
    url='https://daedaluswallet.io/'
    depends=('jq')
    license=('custom')
    install='${packageName}.install'
    options=('!strip' '!debug')
    package() {
      install -d "\$pkgdir"
      cp -a '$work/payload/.' "\$pkgdir/"
    }
    EOF
    cat ${pkgs.pacman}/etc/makepkg.conf >"$work/makepkg.conf"
    cat >>"$work/makepkg.conf" <<EOF
    PKGDEST='$work/pkgdest'
    PKGEXT='.pkg.tar.zst'
    COMPRESSZST=(zstd -c -T1 -19 -)
    EOF

    find "$payload" -exec touch -h -d "@$SOURCE_DATE_EPOCH" {} +
    find "$payload" -type d -exec chmod 0755 {} +
    (cd "$work" && HOME="$work/home" makepkg --config makepkg.conf --nodeps --noconfirm --nosign -f -p PKGBUILD)

    mkdir -p "$out/nix-support"
    target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${revision}-x86_64-linux.pkg.tar.zst"
    package=$(printf '%s\n' "$work"/pkgdest/*.pkg.tar.zst)
    mv "$package" "$target"
    printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
    sha256sum "$target" >"$out/SHA256SUMS"
  ''
