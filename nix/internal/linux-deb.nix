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
  packageVersion = "${version}+build${toString buildCounter}.git${buildRevShort}-1";
  installRoot = "/opt/daedalus/${cluster}";
  commonTemplate = ./linux-deb-common.sh;
  scriptTemplates = {
    preinst = ./linux-deb-preinst.sh;
    postinst = ./linux-deb-postinst.sh;
    prerm = ./linux-deb-prerm.sh;
    postrm = ./linux-deb-postrm.sh;
  };
in
  pkgs.runCommand "daedalus-${cluster}-${packageVersion}-deb" {
    nativeBuildInputs = [pkgs.dpkg pkgs.jq pkgs.patchelf pkgs.findutils pkgs.coreutils pkgs.file];
    SOURCE_DATE_EPOCH = toString sourceDateEpoch;
  } ''
    set -eu
    stage="$TMPDIR/stage"
    root="$stage${installRoot}"
    mkdir -p "$root" "$stage/DEBIAN" "$stage/usr/bin" \
      "$stage/usr/share/applications" "$stage/usr/share/icons/hicolor/512x512/apps"
    cp -a ${bundle}/. "$root/"
    chmod -R u+w "$root"
    rm -f "$root/libexec/.patchelf-static" "$root/libexec/update-runner" "$root/share/icon_large.png"
    rm -rf "$root/share/applications"

    electron="$root/libexec/bundle-electron/lib/electron/electron"
    helper="$root/libexec/bundle-electron/lib/electron/chrome-sandbox"
    test -f "$electron" -a ! -L "$electron"
    test -f "$helper" -a ! -L "$helper"
    patchelf --set-interpreter '${installRoot}/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2' "$electron"
    helper_sha=$(sha256sum "$helper" | cut -d' ' -f1)
    chmod 0755 "$helper"

    # The bundle must provide every non-glibc ELF dependency itself. Keep this
    # allowlist limited to ABI components guaranteed by supported Debian hosts.
    while IFS= read -r -d ''' candidate; do
      if file -b "$candidate" | grep -q '^ELF '; then
        needed_entries=$(patchelf --print-needed "$candidate" 2>/dev/null || true)
        while IFS= read -r needed; do
          test -n "$needed" || continue
          case "$needed" in
            ld-linux-x86-64.so.2|libc.so.6|libdl.so.2|libm.so.6|libpthread.so.0|librt.so.1) continue ;;
          esac
          if ! find "$root" \( -type f -o -type l \) -name "$needed" -print -quit | grep -q .; then
            echo "unmapped ELF dependency $needed required by $candidate" >&2
            exit 1
          fi
        done <<<"$needed_entries"
      fi
    done < <(find "$root" -type f -print0)

    cat >"$root/bin/daedalus" <<'EOF'
    #!/bin/sh
    set -eu
    unset LD_LIBRARY_PATH ENTRYPOINT_DIR
    if [ -e '/var/lib/daedalus-package/${cluster}/removing' ]; then
      echo 'Daedalus package removal is in progress.' >&2
      exit 1
    fi
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
    test -f "$root/libexec/electron" -a ! -L "$root/libexec/electron"
    chmod 0755 "$root/bin/daedalus" "$root/libexec/daedalus-frontend" "$root/libexec/electron"

    jq \
      --arg root '${installRoot}' \
      '.applicationUpdateMode = "system-package-disabled"
       | del(.updateRunnerBin)
       | .daedalusBin = ($root + "/libexec/daedalus-frontend")
       | .nodeBin = ($root + "/libexec/cardano-node")
       | .cliBin = ($root + "/libexec/cardano-cli")
       | .walletBin = ($root + "/libexec/cardano-wallet")
       | .cardanoAddressBin = ($root + "/libexec/cardano-address")
       | if has("selfnodeBin") then .selfnodeBin = ($root + "/libexec/local-cluster") else . end
       | if has("mockTokenMetadataServerBin") then .mockTokenMetadataServerBin = ($root + "/libexec/mock-token-metadata-server") else . end' \
      "$root/config/launcher-config.yaml" >"$root/config/launcher-config.yaml.new"
    mv "$root/config/launcher-config.yaml.new" "$root/config/launcher-config.yaml"

    mkdir -p "$root/share/apparmor"
    for row in ubuntu-24.04 ubuntu-26.04; do
      cat >"$root/share/apparmor/$row" <<EOF
    abi <abi/4.0>,
    include <tunables/global>
    profile ${installRoot}/libexec/bundle-electron/lib/electron/electron flags=(default_allow) {
      userns,
    }
    EOF
      chmod 0644 "$root/share/apparmor/$row"
    done

    cat >"$stage/usr/bin/daedalus-${cluster}" <<'EOF'
    #!/bin/sh
    exec '${installRoot}/bin/daedalus' "$@"
    EOF
    chmod 0755 "$stage/usr/bin/daedalus-${cluster}"
    cat >"$stage/usr/share/applications/Daedalus-${cluster}.desktop" <<EOF
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
    install -m 0644 ${icon} "$stage/usr/share/icons/hicolor/512x512/apps/daedalus-${cluster}.png"

    for script in preinst postinst prerm postrm; do
      sed \
        -e 's|@CLUSTER@|${cluster}|g' \
        -e "s|@HELPER_SHA@|$helper_sha|g" \
        -e 's|@PACKAGE_VERSION@|${packageVersion}|g' \
        -e 's|@BUILD_REV@|${buildRev}|g' \
        ${commonTemplate} >"$stage/DEBIAN/$script"
      sed '1d;/shellcheck source=/d;/^\. .*linux-deb-common\.sh/d' "${scriptTemplates.preinst}" >/dev/null
      case "$script" in
        preinst) template=${scriptTemplates.preinst} ;;
        postinst) template=${scriptTemplates.postinst} ;;
        prerm) template=${scriptTemplates.prerm} ;;
        postrm) template=${scriptTemplates.postrm} ;;
      esac
      sed '1d;/shellcheck source=/d;/^\. .*linux-deb-common\.sh/d' "$template" >>"$stage/DEBIAN/$script"
      chmod 0755 "$stage/DEBIAN/$script"
    done

    cat >"$stage/DEBIAN/control" <<EOF
    Package: daedalus-${cluster}
    Version: ${packageVersion}
    Section: utils
    Priority: optional
    Architecture: amd64
    Maintainer: DevOps <devops@iohk.io>
    Depends: libcap2-bin, util-linux
    Suggests: apparmor
    Homepage: https://daedaluswallet.io/
    Description: Daedalus full-node cryptocurrency wallet (${cluster})
     Root-managed system package prepared for fail-closed Chromium sandbox setup.
    EOF
    dpkg --validate-version '${packageVersion}'

    find "$stage" -exec touch -h -d "@$SOURCE_DATE_EPOCH" {} +
    find "$stage" -type d -exec chmod 0755 {} +
    chmod 0755 "$stage/DEBIAN"
    chmod 0755 "$stage/DEBIAN/preinst" "$stage/DEBIAN/postinst" "$stage/DEBIAN/prerm" "$stage/DEBIAN/postrm"
    chmod 0644 "$stage/DEBIAN/control"

    mkdir -p "$out"
    target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${buildRevShort}-x86_64-linux.deb"
    dpkg-deb --root-owner-group -Zxz -z9 --build "$stage" "$target"
    mkdir -p "$out/nix-support"
    printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
    sha256sum "$target" >"$out/SHA256SUMS"
  ''
