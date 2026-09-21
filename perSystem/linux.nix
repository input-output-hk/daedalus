{inputs, ...}: {
  perSystem = {
    system,
    pkgs,
    lib,
    config,
    common,
    linuxBuild,
    windowsBuild,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      clusters = common.sourceLib.installerClusters;
      inherit (common) sourceLib daedalusConfigs;
      inherit (linuxBuild) originalPackageJson daedalusJs newPackage;
      genClusters = lib.genAttrs clusters;

      commonPackagingArgs = cluster: {
        inherit pkgs lib cluster;
        bundle = systemPackageBundle.${cluster};
        icon = daedalusConfigs.${cluster}.installerConfig.iconPath.base + "/512x512.png";
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

      # ---------------------------------------------------------------------------
      # Inlined packaging/linux/deb.nix as a local function
      # ---------------------------------------------------------------------------
      mkDebInstaller = {
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
        debCompression ? "-Zxz -z9",
      }: let
        packageVersion = "${version}+build${toString buildCounter}.git${buildRevShort}-1";
        installRoot = "/opt/daedalus/${cluster}";
        commonTemplate = ../packaging/linux/linux-deb-common.sh;
        scriptTemplates = {
          preinst = ../packaging/linux/deb-preinst.sh;
          postinst = ../packaging/linux/deb-postinst.sh;
          prerm = ../packaging/linux/deb-prerm.sh;
          postrm = ../packaging/linux/deb-postrm.sh;
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

          # The bundle must provide every non-glibc ELF dependency itself.
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
          export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
          export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
          mkdir -p "''${DAEDALUS_DIR}/${cluster}/Logs/pub" "''${DAEDALUS_DIR}/${cluster}/Secrets"
          cd "''${DAEDALUS_DIR}/${cluster}"
          exec '${installRoot}/libexec/cardano-watchdog' \
            --config '${installRoot}/config/daedalus-config.json' \
            --pub-logs-dir "''${DAEDALUS_DIR}/${cluster}/Logs/pub" \
            --tls-dir "''${DAEDALUS_DIR}/${cluster}/tls"
          EOF
          cat >"$root/libexec/daedalus-frontend" <<'EOF'
          #!/bin/sh
          set -eu
          exec '${installRoot}/libexec/electron' '${installRoot}/libexec/daedalus-js' "$@"
          EOF
          cat >"$root/libexec/electron" <<'EOF'
          #!/bin/sh
          set -eu
          if [ -z "''${XCURSOR_PATH:-}" ] && [ -d /usr/share/icons ]; then export XCURSOR_PATH=/usr/share/icons; fi
          export XKB_CONFIG_ROOT='${installRoot}/libexec/bundle-electron/share/X11/xkb'
          export GBM_BACKENDS_PATH='${installRoot}/libexec/bundle-electron/lib/electron/lib'
          exec '${installRoot}/libexec/bundle-electron/lib/electron/electron' "$@"
          EOF
          chmod 0755 "$root/bin/daedalus" "$root/libexec/daedalus-frontend" "$root/libexec/electron"

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

          find "$stage" -type d -exec chmod 0755 {} +
          chmod 0755 "$stage/DEBIAN"
          chmod 0755 "$stage/DEBIAN/preinst" "$stage/DEBIAN/postinst" "$stage/DEBIAN/prerm" "$stage/DEBIAN/postrm"
          chmod 0644 "$stage/DEBIAN/control"

          mkdir -p "$out"
          target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${buildRevShort}-x86_64-linux.deb"
          dpkg-deb --root-owner-group ${debCompression} --build "$stage" "$target"
          mkdir -p "$out/nix-support"
          printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
          sha256sum "$target" >"$out/SHA256SUMS"
        '';

      # ---------------------------------------------------------------------------
      # Stub RPM database for the Nix sandbox.
      # rpmbuild resolves Requires() scriptlet deps by querying an RPM db; in the
      # Nix sandbox there is no system db and the resolver hangs waiting for a
      # socket/dbus that doesn't exist.  Pre-populate a minimal db with stubs for
      # the packages listed in rpm.spec.in so queries return immediately.
      # This derivation has fixed inputs and is cached by Nix forever.
      # ---------------------------------------------------------------------------
      rpmStubDb = let
        stubNames = ["bash" "coreutils" "grep" "policycoreutils" "sed" "selinux-policy-targeted"];
        mkStubSpec = name:
          pkgs.writeText "${name}-stub.spec" ''
            Name: ${name}
            Version: 1
            Release: 1
            Summary: Stub for Nix RPM build sandbox
            License: GPL-3.0-or-later
            AutoReqProv: no
            BuildArch: noarch

            %description
            Stub

            %files
          '';
      in
        pkgs.runCommand "rpm-stub-db" {
          nativeBuildInputs = [pkgs.rpm pkgs.coreutils];
        } ''
          export HOME="$TMPDIR/home"
          mkdir -p "$HOME" "$out"
          rpm --dbpath "$out" --initdb
          ${lib.concatMapStrings (name: let
              spec = mkStubSpec name;
            in ''
              mkdir -p "$TMPDIR/rb-${name}"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS,tmp}
              rpmbuild --define "_topdir $TMPDIR/rb-${name}" \
                       --define "_tmppath $TMPDIR/rb-${name}/tmp" \
                       --define "__check_files %{nil}" \
                       --define "__find_requires %{nil}" \
                       --define "__find_provides %{nil}" \
                       --define "__scriptlet_requires %{nil}" \
                       -bb --nodeps ${spec}
              rpm --dbpath "$out" -i --nodeps --justdb \
                  "$TMPDIR/rb-${name}"/RPMS/noarch/*.rpm
            '')
            stubNames}
        '';

      # ---------------------------------------------------------------------------
      # Inlined packaging/linux/rpm.nix as a local function
      # ---------------------------------------------------------------------------
      mkRpmInstaller = {
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
        binaryPayload ? "w9.xzdio",
        devBuild ? false,
      }: let
        installRoot = "/opt/daedalus/${cluster}";
        packageName = "daedalus-${cluster}";
        revision =
          if builtins.match "[0-9a-f]{9}" buildRevShort != null
          then buildRevShort
          else lib.substring 0 9 (builtins.hashString "sha256" (toString bundle));
        release = "${toString buildCounter}.git${revision}";
        moduleName = "daedalus_${builtins.replaceStrings ["-"] ["_"] cluster}";
        policyTemplate = ../packaging/linux/rpm-policy.cil;
        specTemplate =
          if devBuild
          then ../packaging/linux/rpm-dev.spec.in
          else ../packaging/linux/rpm.spec.in;
        payloadTarball = "/usr/share/daedalus-${cluster}/payload.tar.zst";
      in
        pkgs.runCommand "${packageName}-${version}-${release}-rpm" {
          nativeBuildInputs =
            [
              pkgs.coreutils
              pkgs.file
              pkgs.findutils
              pkgs.gnugrep
              pkgs.gnused
              pkgs.jq
              pkgs.patchelf
              pkgs.rpm
            ]
            ++ lib.optionals devBuild [pkgs.zstd];
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
          mkdir -p "$top"/{BUILD,BUILDROOT,RPMS,SOURCES,SPECS,SRPMS,build,home,tmp} \
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
          chmod 0755 "$helper"

          cat >"$root/bin/daedalus" <<'EOF'
          #!/bin/sh
          set -eu
          unset LD_LIBRARY_PATH ENTRYPOINT_DIR
          export CLUSTER='${cluster}'
          export DAEDALUS_CONFIG='${installRoot}/config'
          export ENTRYPOINT_DIR='${installRoot}'
          export CHROME_DEVEL_SANDBOX='${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox'
          export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
          export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
          mkdir -p "''${DAEDALUS_DIR}/${cluster}/Logs/pub" "''${DAEDALUS_DIR}/${cluster}/Secrets"
          cd "''${DAEDALUS_DIR}/${cluster}"
          exec '${installRoot}/libexec/cardano-watchdog' \
            --config '${installRoot}/config/daedalus-config.json' \
            --pub-logs-dir "''${DAEDALUS_DIR}/${cluster}/Logs/pub" \
            --tls-dir "''${DAEDALUS_DIR}/${cluster}/tls"
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

          ${lib.optionalString devBuild ''
            # Dev: pack the 40k-file bundle into a single tar.zst so rpmbuild
            # only hashes ~5 files instead of 40k (ZFS per-file overhead kills us).
            tarball_host="$payload${payloadTarball}"
            mkdir -p "$(dirname "$tarball_host")"
            tar -c -C "$root" . | zstd -T0 -1 -f -o "$tarball_host"
            rm -rf "$root"  # removed from payload; extracted by %post at install time
          ''}

          ${lib.optionalString (!devBuild) ''
            find "$payload" -type d -exec chmod 0755 {} +
          ''}

          sed \
            -e 's|@PACKAGE_NAME@|${packageName}|g' \
            -e 's|@VERSION@|${version}|g' \
            -e 's|@RELEASE@|${release}|g' \
            -e 's|@CLUSTER@|${cluster}|g' \
            -e "s|@PAYLOAD@|$payload|g" \
            -e 's|@BINARY_PAYLOAD@|${binaryPayload}|g' \
            -e 's|@PAYLOAD_TARBALL@|${payloadTarball}|g' \
            -e 's|@POLICY_PATH@|/usr/share/selinux/packages/daedalus-${cluster}.cil|g' \
            -e 's|@MODULE_NAME@|${moduleName}|g' \
            -e 's|@ELECTRON_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/electron|g' \
            -e 's|@HELPER_PATH@|${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox|g' \
            -e "s|@POLICY_SHA@|$policy_sha|g" \
            -e "s|@PACKAGE_ID@|$package_id|g" \
            ${specTemplate} >"$top/SPECS/daedalus.spec"

          rpm_buildroot=""
          ${lib.optionalString devBuild ''rpm_buildroot="--buildroot=$payload"''}
          # shellcheck disable=SC2086
          rpmbuild -bb --nodeps "$top/SPECS/daedalus.spec" \
            $rpm_buildroot \
            --define "_topdir $top" \
            --define "_builddir $top/build" \
            --define "_dbpath ${rpmStubDb}" \
            --define "_tmppath $top/tmp" \
            --define "_binary_payload ${binaryPayload}" \
            --define "_source_date_epoch $SOURCE_DATE_EPOCH" \
            --define "use_source_date_epoch_as_buildtime 1" \
            --define "build_mtime_policy clamp_to_source_date_epoch" \
            --define "__check_files %{nil}" \
            --define "__find_requires %{nil}" \
            --define "__find_provides %{nil}" \
            --define "__scriptlet_requires %{nil}"

          mkdir -p "$out/nix-support"
          target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${revision}-x86_64-linux.rpm"
          cp "$top"/RPMS/x86_64/*.rpm "$target"
          printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
          sha256sum "$target" >"$out/SHA256SUMS"
        '';

      # ---------------------------------------------------------------------------
      # Inlined packaging/linux/arch.nix as a local function
      # ---------------------------------------------------------------------------
      mkArchInstaller = {
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
        zstdLevel ? 19,
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
          export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
          export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
          mkdir -p "''${DAEDALUS_DIR}/${cluster}/Logs/pub" "''${DAEDALUS_DIR}/${cluster}/Secrets"
          cd "''${DAEDALUS_DIR}/${cluster}"
          exec '${installRoot}/libexec/cardano-watchdog' \
            --config '${installRoot}/config/daedalus-config.json' \
            --pub-logs-dir "''${DAEDALUS_DIR}/${cluster}/Logs/pub" \
            --tls-dir "''${DAEDALUS_DIR}/${cluster}/tls"
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

          sed 's|@CLUSTER@|${cluster}|g' ${../packaging/linux/arch.install} >"$work/${packageName}.install"
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
          COMPRESSZST=(zstd -c -T0 -${toString zstdLevel} -)
          EOF

          find "$payload" -type d -exec chmod 0755 {} +
          (cd "$work" && HOME="$work/home" makepkg --config makepkg.conf --nodeps --noconfirm --nosign -f -p PKGBUILD)

          mkdir -p "$out/nix-support"
          target="$out/daedalus-${version}-${toString buildCounter}-${cluster}-${revision}-x86_64-linux.pkg.tar.zst"
          package=$(printf '%s\n' "$work"/pkgdest/*.pkg.tar.zst)
          mv "$package" "$target"
          printf 'file binary-dist "%s"\n' "$target" >"$out/nix-support/hydra-build-products"
          sha256sum "$target" >"$out/SHA256SUMS"
        '';

      debInstaller = genClusters (cluster: mkDebInstaller (commonPackagingArgs cluster));
      debInstallerDev = genClusters (cluster: mkDebInstaller ((commonPackagingArgs cluster) // {debCompression = "-Zzstd -z1";}));
      rpmInstaller = genClusters (cluster: mkRpmInstaller (commonPackagingArgs cluster));
      rpmInstallerDev = genClusters (cluster:
        mkRpmInstaller ((commonPackagingArgs cluster)
          // {
            binaryPayload = "w1T16.zstdio";
            devBuild = true;
          }));
      archInstaller = genClusters (cluster: mkArchInstaller (commonPackagingArgs cluster));
      archInstallerDev = genClusters (cluster: mkArchInstaller ((commonPackagingArgs cluster) // {zstdLevel = 1;}));

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

            cp -r ${daedalusConfigs.${cluster}.configFiles}/. $out/config/

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

              exec cardano-watchdog \
                --config "$ENTRYPOINT_DIR/config/daedalus-config.json" \
                --pub-logs-dir "''${DAEDALUS_DIR}/${cluster}/Logs/pub" \
                --tls-dir "''${DAEDALUS_DIR}/${cluster}/tls"
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
      packages =
        # Linux-native packages
        lib.listToAttrs (lib.concatMap (cluster: [
            {
              name = "deb-installer-${cluster}";
              value = debInstaller.${cluster};
            }
            {
              name = "deb-dev-installer-${cluster}";
              value = debInstallerDev.${cluster};
            }
            {
              name = "rpm-installer-${cluster}";
              value = rpmInstaller.${cluster};
            }
            {
              name = "rpm-dev-installer-${cluster}";
              value = rpmInstallerDev.${cluster};
            }
            {
              name = "arch-installer-${cluster}";
              value = archInstaller.${cluster};
            }
            {
              name = "arch-dev-installer-${cluster}";
              value = archInstallerDev.${cluster};
            }
            {
              name = "nixos-package-${cluster}";
              value = nixosPackage.${cluster};
            }
            {
              name = "nixos-sandbox-setup-${cluster}";
              value = nixosSandboxSetup.${cluster};
            }
            {
              name = "daedalus-${cluster}";
              value = newPackage.${cluster};
            }
            {
              name = "installer-${cluster}";
              value = linuxBuild.unsignedInstaller.${cluster};
            }
            {
              name = "makeSignedInstaller-${cluster}";
              value = linuxBuild.makeSignedInstaller.${cluster};
            }
            {
              name = "daedalus-bridge-${cluster}";
              value = common.daedalus-bridge.${cluster};
            }
            # Windows cross-compiled packages (built on x86_64-linux)
            {
              name = "daedalus-x86_64-windows-${cluster}";
              value = windowsBuild.package.${cluster};
            }
            {
              name = "installer-x86_64-windows-${cluster}";
              value = windowsBuild.unsignedInstaller.${cluster};
            }
            {
              name = "makeSignedInstaller-x86_64-windows-${cluster}";
              value = windowsBuild.makeSignedInstaller.${cluster};
            }
            {
              name = "daedalus-bridge-x86_64-windows-${cluster}";
              value = windowsBuild.daedalus-bridge.${cluster};
            }
          ])
          clusters)
        // {
          default = newPackage.mainnet;
          nativeModules-x86_64-windows = windowsBuild.nativeModulesZip;
        };
    });
}
