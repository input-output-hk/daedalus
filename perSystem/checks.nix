{inputs, ...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    ...
  }: let
    # We reuse the pre-built node_modules from the installer pipeline so there's
    # no redundant yarn install. Resolved per-system rather than pinned to
    # x86_64-linux, so a check can run anywhere that pipeline already builds —
    # which is every system here, since the darwin installers depend on it.
    internal = inputs.self.internal.${system};
    inherit (internal) nodejs yarn srcWithoutNix node_modules;

    mkJsCheck = name: command:
      pkgs.stdenv.mkDerivation {
        inherit name;
        src = srcWithoutNix;
        nativeBuildInputs = [yarn nodejs];
        configurePhase = ''
          export HOME=$(realpath $NIX_BUILD_TOP/home)
          mkdir -p $HOME
          # Prevent yarn from hitting the network in the sandbox:
          echo '"--offline" true' >>$HOME/.yarnrc
          cp -r ${node_modules}/. ./
          chmod -R +w .
          patchShebangs .
        '';
        buildPhase = command;
        installPhase = "touch $out";
        dontFixup = true;
      };

    linuxPackages = inputs.self.packages.x86_64-linux;
    installerClusters = inputs.self.internal.installerClusters;

    linuxReleaseArtifactsContract =
      assert !(internal ? unsignedInstaller);
      assert !(internal ? makeSignedInstaller);
      assert !(internal ? selfExtractingArchive);
      assert !(internal ? removeOldNixChroot);
      assert !(internal ? satisfyOldUpdateRunner);
      assert !(internal ? newBundle);
      assert !(builtins.pathExists (inputs.self + "/nix/internal/linux-self-extracting-archive.sh"));
      assert internal ? systemPackageBundle;
      assert internal ? debInstaller;
      assert internal ? rpmInstaller;
      assert lib.all (
        cluster:
          !(builtins.hasAttr "installer-${cluster}" linuxPackages)
          && !(builtins.hasAttr "makeSignedInstaller-${cluster}" linuxPackages)
          && builtins.hasAttr "deb-installer-${cluster}" linuxPackages
          && builtins.hasAttr "rpm-installer-${cluster}" linuxPackages
      )
      installerClusters;
      assert !(builtins.hasAttr "x86_64-linux" inputs.self.hydraJobs.installer);
      assert builtins.hasAttr "x86_64-linux" inputs.self.hydraJobs."deb-installer";
      assert builtins.hasAttr "x86_64-linux" inputs.self.hydraJobs."rpm-installer";
      pkgs.runCommand "linux-release-artifacts-contract" {} ''
          set -eu
          pipeline=${linuxPackages.buildkitePipeline}/bin/buildkite-pipeline

          for cluster in ${lib.escapeShellArgs installerClusters}; do
            grep -F ".#packages.x86_64-linux.deb-installer-$cluster" "$pipeline"
            grep -F ".#packages.x86_64-linux.rpm-installer-$cluster" "$pipeline"
          done
          grep -F 'artifact upload "csl-daedalus-deb/*.deb"' "$pipeline"
          grep -F 'artifact upload "csl-daedalus-rpm/*.rpm"' "$pipeline"

          if grep -E '\.#packages\.x86_64-linux\.(installer|makeSignedInstaller)-|artifact upload \*/\*|\.bin([^[:alnum:]]|$)' "$pipeline"; then
            echo 'generic Linux installer, signing, upload, or .bin seam found in Buildkite pipeline' >&2
            exit 1
          fi

          touch "$out"
        '';

    linuxDebPackageContract =
      pkgs.runCommand "linux-deb-package-contract" {
        nativeBuildInputs = [pkgs.dpkg pkgs.jq pkgs.file pkgs.patchelf];
      } ''
          set -eu
          deb=$(printf '%s\n' ${internal.debInstaller.mainnet}/*.deb)
          mkdir extracted
          dpkg-deb --extract "$deb" extracted
          dpkg-deb --control "$deb" control

          root=extracted/opt/daedalus/mainnet
          test -x "$root/bin/daedalus"
          test -x "$root/libexec/daedalus-frontend"
          test -x "$root/libexec/electron"
          test ! -e "$root/libexec/update-runner"
          test ! -e "$root/libexec/.patchelf-static"
          test ! -e "$root/share/icon_large.png"
          test ! -e "$root/share/applications"
          test "$(jq -r .applicationUpdateMode "$root/config/launcher-config.yaml")" = system-package-disabled
          test "$(jq -r 'has("updateRunnerBin")' "$root/config/launcher-config.yaml")" = false
          jq -e '.dappBrowserPolicy == {"revision":1,"globalEnabled":false,"preferredCatalogEnabled":false,"diagnosticsEnabled":false,"cip104Revision":0,"cip142Revision":0}' \
            "$root/config/launcher-config.yaml" >/dev/null
          test "$(jq -r .daedalusBin "$root/config/launcher-config.yaml")" = /opt/daedalus/mainnet/libexec/daedalus-frontend
          test "$(stat -c %a "$root/libexec/bundle-electron/lib/electron/chrome-sandbox")" = 755
          test "$(patchelf --print-interpreter "$root/libexec/bundle-electron/lib/electron/electron")" = /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2

          for surface in \
            "$root/bin/daedalus" \
            "$root/libexec/daedalus-frontend" \
            "$root/libexec/electron" \
            "$root/config/launcher-config.yaml" \
            extracted/usr/share/applications/Daedalus-mainnet.desktop \
            control/preinst control/postinst control/prerm control/postrm; do
            if grep -E -- '--no-sandbox|--disable-setuid-sandbox|ELECTRON_DISABLE_SANDBOX|\.daedalus/.*/bin/daedalus|pre-auto-update|update-runner|updateRunnerBin|\.patchelf-static' "$surface"; then
              echo "forbidden portable or sandbox-bypass content in $surface" >&2
              exit 1
            fi
          done

          grep -F 'Exec=/opt/daedalus/mainnet/bin/daedalus' extracted/usr/share/applications/Daedalus-mainnet.desktop
        grep -F "export CHROME_DEVEL_SANDBOX='/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox'" "$root/bin/daedalus"
        grep -F 'abort-upgrade' control/preinst
        grep -F 'install)' control/preinst
        grep -F 'upgrade)' control/preinst
        grep -F 'configure)' control/postinst
        grep -F 'configure_transaction' control/postinst
        grep -F 'rm -rf "$state_dir/previous"' control/postinst
        grep -F 'abort-remove' control/postinst
        grep -F 'rm -f "$state_dir/removing"' control/postinst
        grep -F 'abort-deconfigure' control/postinst
        grep -F 'failed-upgrade' control/prerm
        grep -F 'remove)' control/prerm
        grep -F 'disappear' control/postrm
        grep -F 'abort-install' control/postrm
        grep -F 'purge)' control/postrm
        grep -F 'preserving modified sandbox manifest and package state' control/postrm
        grep -F 'rmdir "$install_root/share" "$install_root"' control/postrm
        grep -F 'task-108-matrix-2026-08-18' control/postinst
        grep -F 'Package: daedalus-mainnet' control/control
        grep -F 'Architecture: amd64' control/control
        grep -F 'Depends: libcap2-bin, util-linux' control/control

        sed '/^lock$/,$d' control/postinst >common.sh
        . ./common.sh
        install_root=$PWD/extracted/opt/daedalus/mainnet
        state_dir=$PWD/manifest-state
        manifest_path=$PWD/generated-manifest.json
        profile_path=$install_root/share/apparmor/ubuntu-24.04
        matrix_row=ubuntu-24.04
        host_id=ubuntu
        host_version=24.04
        support_state=supported
        reason=supported
        policy_kind=apparmor
        helper_mode=4755
        mkdir "$state_dir"
        chown() { :; }
        write_manifest default-allow-userns-ubuntu-24.04
        jq -e '
          .matrixRow == "ubuntu-24.04"
          and .distribution == {"id":"ubuntu","versionId":"24.04"}
          and .policy.loadedProfileSuffix == " (unconfined)"
        ' "$manifest_path" >/dev/null

        install() {
          mode=
          while [ "$#" -gt 2 ]; do
            if [ "$1" = -m ]; then
              mode=$2
            fi
            shift 2
          done
          command cp "$1" "$2"
          [ -z "$mode" ] || command chmod "$mode" "$2"
        }
        parser_log=$PWD/apparmor-parser.log
        apparmor_parser() { printf '%s\n' "$*" >>"$parser_log"; }
        helper_path=$PWD/rollback-helper
        printf helper >"$helper_path"
        helper_mode_state=$PWD/rollback-helper.mode
        printf 755 >"$helper_mode_state"
        chmod() {
          if [ "$#" -eq 2 ] && [ "$2" = "$helper_path" ]; then
            case "$1" in
              0*) printf '%s' "$1" | cut -c2- >"$helper_mode_state" ;;
              *) printf '%s' "$1" >"$helper_mode_state" ;;
            esac
          else
            command chmod "$@"
          fi
        }
        stat() {
          if [ "$#" -eq 3 ] && [ "$1" = -c ] && [ "$2" = %a ] && [ "$3" = "$helper_path" ]; then
            cat "$helper_mode_state"
          else
            command stat "$@"
          fi
        }

        state_dir=$PWD/upgrade-state
        profile_path=$PWD/upgrade-profile
        manifest_path=$PWD/upgrade-manifest
        mkdir "$state_dir"
        printf old-profile >"$profile_path"
        printf old-manifest >"$manifest_path"
        sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/profile.sha256"
        sha256sum "$manifest_path" | cut -d' ' -f1 >"$state_dir/manifest.sha256"
        printf old-version >"$state_dir/configured-version"
        printf old-statoverride >"$state_dir/statoverride"
        chmod 0755 "$helper_path"
        snapshot_state
        printf new-profile >"$profile_path"
        printf new-manifest >"$manifest_path"
        sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/candidate-profile.sha256"
        sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/profile.sha256"
        sha256sum "$manifest_path" | cut -d' ' -f1 >"$state_dir/manifest.sha256"
        printf new-version >"$state_dir/configured-version"
        printf new-statoverride >"$state_dir/statoverride"
        chmod 4755 "$helper_path"
        restore_state
        test "$(cat "$profile_path")" = old-profile
        test "$(cat "$manifest_path")" = old-manifest
        test "$(cat "$state_dir/profile.sha256")" = "$(sha256sum "$profile_path" | cut -d' ' -f1)"
        test "$(cat "$state_dir/manifest.sha256")" = "$(sha256sum "$manifest_path" | cut -d' ' -f1)"
        test "$(cat "$state_dir/configured-version")" = old-version
        test "$(cat "$state_dir/statoverride")" = old-statoverride
        test "$(stat -c %a "$helper_path")" = 755
        test ! -e "$state_dir/candidate-profile.sha256"
        grep -F -- "-r $profile_path" "$parser_log"

        rm -rf "$state_dir"
        rm -f "$profile_path" "$manifest_path" "$parser_log" "$helper_path"
        mkdir "$state_dir"
        snapshot_state
        configure_package() (
          set -e
          printf helper >"$helper_path"
          printf 755 >"$helper_mode_state"
          printf candidate-profile >"$profile_path"
          printf candidate-manifest >"$manifest_path"
          sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/candidate-profile.sha256"
          sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/profile.sha256"
          sha256sum "$manifest_path" | cut -d' ' -f1 >"$state_dir/manifest.sha256"
          printf candidate-version >"$state_dir/configured-version"
          printf candidate-statoverride >"$state_dir/statoverride"
          chmod 4755 "$helper_path"
          false
          printf must-not-run >"$state_dir/errexit-broken"
        )
        set +e
        configure_transaction
        configure_status=$?
        set -e
        test "$configure_status" -ne 0
        test ! -e "$state_dir/errexit-broken"
        test ! -e "$profile_path"
        test ! -e "$manifest_path"
        test ! -e "$state_dir/profile.sha256"
        test ! -e "$state_dir/manifest.sha256"
        test ! -e "$state_dir/configured-version"
        test ! -e "$state_dir/statoverride"
        test ! -e "$state_dir/candidate-profile.sha256"
        test "$(stat -c %a "$helper_path")" = 755
        grep -F -- "-R $profile_path" "$parser_log"
          touch "$out"
      '';

    linuxRemainingLauncherContract =
      pkgs.runCommand "linux-remaining-launcher-contract" {
        nativeBuildInputs = [pkgs.jq];
      } ''
        set -eu
        package=${internal.newPackage.mainnet}
        launcher="$package/bin/daedalus"
        frontend="$package/libexec/daedalus-frontend"
        config="$package/config/launcher-config.yaml"

        test -x "$launcher"
        test -x "$frontend"
        test ! -e "$package/libexec/update-runner"
        test "$(jq -r .applicationUpdateMode "$config")" = system-package-disabled
        test "$(jq -r 'has("updateRunnerBin")' "$config")" = false
        jq -e '.dappBrowserPolicy == {"revision":1,"globalEnabled":false,"preferredCatalogEnabled":false,"diagnosticsEnabled":false,"cip104Revision":0,"cip142Revision":0}' \
          "$config" >/dev/null

        if grep -E -- '--no-sandbox|--disable-setuid-sandbox|ELECTRON_DISABLE_SANDBOX|\.daedalus/.*/bin/daedalus|pre-auto-update|update-runner|updateRunnerBin|\.patchelf-static' \
          "$launcher" "$frontend" "$config"; then
          echo 'portable updater, home restart, or sandbox bypass found in remaining Linux launcher' >&2
          exit 1
        fi
        grep -F 'exec electron ' "$frontend"
        grep -F '"$ENTRYPOINT_DIR"/libexec/daedalus-js "$@"' "$frontend"
        touch "$out"
      '';

    linuxRpmPackageContract =
      pkgs.runCommand "linux-rpm-package-contract" {
        nativeBuildInputs = [pkgs.jq pkgs.libarchive nodejs pkgs.patchelf pkgs.rpm pkgs.yq-go];
      } ''
        set -eu
        rpm=$(printf '%s\n' ${internal.rpmInstaller.mainnet}/*.rpm)
        mkdir extracted
        (
          cd extracted
          bsdtar --no-same-permissions -xf "$rpm"
        )

        root=extracted/opt/daedalus/mainnet
        helper="$root/libexec/bundle-electron/lib/electron/chrome-sandbox"
        electron="$root/libexec/bundle-electron/lib/electron/electron"
        manifest="$root/share/daedalus-sandbox-identity.json"
        policy=extracted/usr/share/selinux/packages/daedalus-mainnet.cil
        scripts=$PWD/scripts
        rpm -qp --scripts "$rpm" >"$scripts"

        test "$(rpm -qp --qf '%{NAME}\n%{ARCH}\n' "$rpm")" = $'daedalus-mainnet\nx86_64'
        test -x "$root/bin/daedalus"
        test -x "$root/libexec/daedalus-frontend"
        test -x "$root/libexec/electron"
        test ! -e "$root/libexec/update-runner"
        test ! -e "$root/libexec/.patchelf-static"
        test ! -e "$root/share/icon_large.png"
        test ! -e "$root/share/applications"
        test -f "$helper" -a ! -L "$helper"
        rpm -qp --qf '[%{FILENAMES} %{FILEMODES:perms}\n]' "$rpm" >rpm-file-modes
        grep -F '/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox -rwsr-xr-x' rpm-file-modes
        test "$(patchelf --print-interpreter "$electron")" = /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2
        test "$(yq -r .applicationUpdateMode "$root/config/launcher-config.yaml")" = system-package-disabled
        test "$(yq -r 'has("updateRunnerBin")' "$root/config/launcher-config.yaml")" = false
        yq -e '.dappBrowserPolicy == {"revision":1,"globalEnabled":false,"preferredCatalogEnabled":false,"diagnosticsEnabled":false,"cip104Revision":0,"cip142Revision":0}' \
          "$root/config/launcher-config.yaml" >/dev/null
        NODE_PATH=${node_modules}/node_modules node -e \
          "require('yamljs').parse(require('fs').readFileSync(process.argv[1], 'utf8'))" \
          "$root/config/launcher-config.yaml"
        jq -e '
          .packageFamily == "rpm"
          and .matrixRow == "fedora-43"
          and .supportState == "supported"
          and .helper.mode == "4755"
          and .policy.module == "daedalus_mainnet"
          and .policy.mainProcessType == "unconfined_t"
          and .policy.rendererProcessType == "unconfined_t"
          and .policy.electronFileContext == "system_u:object_r:bin_t:s0"
          and .policy.helperFileContext == "system_u:object_r:chrome_sandbox_exec_t:s0"
        ' "$manifest" >/dev/null

        grep -F 'chrome_sandbox_exec_t' "$policy"
        for forbidden in '(allow ' '(dontaudit ' '(typepermissive ' unconfined_domain_type; do
          if grep -F "$forbidden" "$policy"; then
            echo "forbidden SELinux policy construct: $forbidden" >&2
            exit 1
          fi
        done
        grep -F 'unconfined_chrome_sandbox_transition' "$scripts"
        grep -F 'getenforce' "$scripts"
        grep -F 'semodule -X 200' "$scripts"
        grep -F 'restorecon' "$scripts"
        grep -F 'chrome_sandbox_exec_t' "$scripts"
        grep -F 'Exec=/opt/daedalus/mainnet/bin/daedalus' \
          extracted/usr/share/applications/Daedalus-mainnet.desktop
        grep -F "export CHROME_DEVEL_SANDBOX='/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox'" \
          "$root/bin/daedalus"
        if grep -E -- '--no-sandbox|--disable-setuid-sandbox|ELECTRON_DISABLE_SANDBOX|\.daedalus/.*/bin/daedalus|pre-auto-update|update-runner|updateRunnerBin|\.patchelf-static' \
          "$root/bin/daedalus" "$root/libexec/daedalus-frontend" \
          "$root/libexec/electron" "$root/config/launcher-config.yaml" "$scripts"; then
          echo 'portable updater, home restart, or sandbox bypass found in RPM launch or lifecycle surface' >&2
          exit 1
        fi

        touch "$out"
      '';
  in {
    checks =
      # The suites that execute the code under test run natively on each OS we
      # ship. They are full of platform branches — path separators, case
      # sensitivity, symlink and junction handling — and on a single platform
      # those branches are only exercised by overriding `process.platform`
      # in-process, which asserts what the code does when told it is elsewhere
      # rather than what it does when it is.
      #
      # Per OS, not per system: x86_64-darwin and aarch64-darwin share a kernel
      # and a filesystem, and nothing in these suites is architecture
      # dependent, so the second darwin run costs builder time for signal that
      # is already covered.
      lib.optionalAttrs (system != "x86_64-darwin") {
        jest = mkJsCheck "daedalus-jest" "yarn test:jest --maxWorkers=4";
        cucumber-unit = mkJsCheck "daedalus-cucumber-unit" "yarn test:unit";
      }
      # The rest are static analysis over the same source, and produce identical
      # results wherever they run. Repeating them per system would spend darwin
      # builder time for no additional signal.
      // lib.optionalAttrs (system == "x86_64-linux") {
        lint = mkJsCheck "daedalus-lint" "yarn lint";
        compile = mkJsCheck "daedalus-compile" "yarn compile";
        stylelint = mkJsCheck "daedalus-stylelint" "yarn stylelint";
        # `yarn i18n:manage` regenerates translation artifacts that are tracked in
        # the repository. Running it is not enough on its own: the command exits 0
        # whether or not the regenerated output matches what is committed, so the
        # tracked artifacts can drift from source indefinitely with CI green.
        # Snapshot them, regenerate, and require the result to be identical.
        i18n = mkJsCheck "daedalus-i18n" ''
          cp -r source/renderer/app/i18n/locales .i18n-locales-before
          cp translations/messages.json .i18n-messages-before.json

          yarn i18n:manage

          if ! diff -r .i18n-locales-before source/renderer/app/i18n/locales \
            || ! diff .i18n-messages-before.json translations/messages.json; then
            echo
            echo "ERROR: committed translation artifacts are out of date."
            echo "Run 'yarn i18n:manage' and commit the resulting changes."
            exit 1
          fi

          rm -rf .i18n-locales-before .i18n-messages-before.json
        '';
        storybook = mkJsCheck "daedalus-storybook-build" "yarn storybook:build";
        shellcheck = pkgs.callPackage ../tests/shellcheck.nix {src = inputs.self;};
        linux-deb-package-contract = linuxDebPackageContract;
        linux-release-artifacts-contract = linuxReleaseArtifactsContract;
        linux-rpm-package-contract = linuxRpmPackageContract;
        linux-remaining-launcher-contract = linuxRemainingLauncherContract;
      };
  };
}
