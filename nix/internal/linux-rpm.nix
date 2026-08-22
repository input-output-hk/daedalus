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
  release = "${toString buildCounter}.git${revision}";
  moduleName = "daedalus_${builtins.replaceStrings ["-"] ["_"] cluster}";
  policyTemplate = ./linux-rpm-policy.cil;
  specTemplate = ./linux-rpm.spec.in;
in
  pkgs.runCommand "${packageName}-${version}-${release}-rpm" {
    nativeBuildInputs = [
      pkgs.coreutils
      pkgs.file
      pkgs.findutils
      pkgs.gnugrep
      pkgs.gnused
      pkgs.jq
      pkgs.patchelf
      pkgs.rpm
      pkgs.yq-go
    ];
    SOURCE_DATE_EPOCH = toString sourceDateEpoch;
  } ''
    set -eu
    printf '%s' '${version}' | grep -Eq '^[0-9]+(\.[0-9]+)*$'
    printf '%s' '${release}' | grep -Eq '^[0-9]+\.git[0-9a-f]{9}$'

    top="$TMPDIR/rpmbuild"
    payload="$TMPDIR/payload"
    root="$payload${installRoot}"
    policy_path="/usr/share/selinux/packages/daedalus-${cluster}.cil"
    electron_path="${installRoot}/libexec/bundle-electron/lib/electron/electron"
    helper_path="${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox"
    mkdir -p "$top"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS,build,home,rpmdb,tmp} \
      "$root" "$payload/usr/bin" "$payload/usr/share/applications" \
      "$payload/usr/share/icons/hicolor/512x512/apps" \
      "$payload/usr/share/selinux/packages"
    export HOME="$top/home"

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
      echo 'Nix-store symlink remains in RPM payload' >&2
      exit 1
    fi
    rm -f "$root/libexec/.patchelf-static" "$root/libexec/update-runner" "$root/share/icon_large.png"
    rm -rf "$root/share/applications"

    electron="$root/libexec/bundle-electron/lib/electron/electron"
    helper="$root/libexec/bundle-electron/lib/electron/chrome-sandbox"
    test -f "$electron" -a ! -L "$electron"
    test -f "$helper" -a ! -L "$helper"
    patchelf --set-interpreter '${installRoot}/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2' "$electron"
    helper_sha=$(sha256sum "$helper" | cut -d' ' -f1)
    # rpmbuild applies the privileged mode from %attr; Nix builders cannot
    # create setuid files in the build sandbox.
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
      yq -i '.selfnodeBin = "${installRoot}/libexec/local-cluster"' \
        "$root/config/launcher-config.yaml"
    fi
    if yq -e 'has("mockTokenMetadataServerBin")' "$root/config/launcher-config.yaml" >/dev/null 2>&1; then
      yq -i '.mockTokenMetadataServerBin = "${installRoot}/libexec/mock-token-metadata-server"' \
        "$root/config/launcher-config.yaml"
    fi

    cat >"$payload/usr/bin/daedalus-${cluster}" <<'EOF'
    #!/bin/sh
    exec '${installRoot}/bin/daedalus' "$@"
    EOF
    chmod 0755 "$payload/usr/bin/daedalus-${cluster}"
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

    sed \
      -e 's|@ELECTRON_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/electron|g' \
      -e 's|@HELPER_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox|g' \
      ${policyTemplate} >"$payload$policy_path"
    chmod 0644 "$payload$policy_path"
    policy_sha=$(sha256sum "$payload$policy_path" | cut -d' ' -f1)

    files_json=$(
      for item in \
        launcher:bin/daedalus \
        frontend:libexec/daedalus-frontend \
        wrapper:libexec/electron \
        electron:libexec/bundle-electron/lib/electron/electron \
        chromeSandbox:libexec/bundle-electron/lib/electron/chrome-sandbox; do
        name=''${item%%:*}
        relative=''${item#*:}
        jq -n --arg name "$name" --arg sha "$(sha256sum "$root/$relative" | cut -d' ' -f1)" \
          '{key:$name,value:{sha256:$sha}}'
      done | jq -s 'from_entries'
    )
    jq -n \
      --arg buildRev '${buildRev}' \
      --arg helperSha "$helper_sha" \
      --arg policySha "$policy_sha" \
      --argjson files "$files_json" \
      '{
        schemaVersion:2,
        packageFamily:"rpm",
        matrixRevision:"task-108-matrix-2026-08-18",
        matrixRow:"fedora-43",
        supportState:"supported",
        reason:"supported",
        distribution:{id:"fedora",versionId:"43"},
        cluster:"${cluster}",
        sourceRevision:(if $buildRev == "0000000000000000000000000000000000000000" then null else $buildRev end),
        sourceDirty:($buildRev == "0000000000000000000000000000000000000000"),
        helper:{mode:"4755",sha256:$helperSha},
        policy:{
          kind:"selinux",
          module:"${moduleName}",
          priority:200,
          semanticVersion:"2.0.0",
          sourceCilSha256:$policySha,
          transitionSourceRole:"unconfined_r",
          transitionSourceType:"unconfined_t",
          mainProcessRole:"unconfined_r",
          mainProcessType:"unconfined_t",
          rendererProcessRole:"unconfined_r",
          rendererProcessType:"unconfined_t",
          electronFileContext:"system_u:object_r:bin_t:s0",
          helperFileContext:"system_u:object_r:chrome_sandbox_exec_t:s0",
          hostPolicyDomain:"chrome_sandbox_t",
          hostPolicyBoolean:"unconfined_chrome_sandbox_transition"
        },
        files:($files + {policyAsset:{sha256:$policySha}})
      }' >"$root/share/daedalus-sandbox-identity.json"
    chmod 0644 "$root/share/daedalus-sandbox-identity.json"

    package_id='${packageName}-${version}-${release}.x86_64'
    sed \
      -e 's|@PACKAGE_NAME@|${packageName}|g' \
      -e 's|@VERSION@|${version}|g' \
      -e 's|@RELEASE@|${release}|g' \
      -e 's|@CLUSTER@|${cluster}|g' \
      -e "s|@PAYLOAD@|$payload|g" \
      -e 's|@POLICY_PATH@|/usr/share/selinux/packages/daedalus-${cluster}.cil|g' \
      -e 's|@MODULE_NAME@|${moduleName}|g' \
      -e 's|@ELECTRON_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/electron|g' \
      -e 's|@HELPER_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox|g' \
      -e "s|@POLICY_SHA@|$policy_sha|g" \
      -e "s|@PACKAGE_ID@|$package_id|g" \
      ${specTemplate} >"$top/SPECS/daedalus.spec"

    find "$payload" -exec touch -h -d "@$SOURCE_DATE_EPOCH" {} +
    find "$payload" -type d -exec chmod 0755 {} +

    rpmbuild -bb "$top/SPECS/daedalus.spec" \
      --define "_topdir $top" \
      --define "_builddir $top/build" \
      --define "_dbpath $top/rpmdb" \
      --define "_tmppath $top/tmp" \
      --define "_source_date_epoch $SOURCE_DATE_EPOCH" \
      --define "use_source_date_epoch_as_buildtime 1" \
      --define "build_mtime_policy clamp_to_source_date_epoch"

    mkdir -p "$out/nix-support"
    target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${revision}-x86_64-linux.rpm"
    cp "$top"/RPMS/x86_64/*.rpm "$target"
    printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
    sha256sum "$target" >"$out/SHA256SUMS"
  ''
