# Inlines: lib/linux-build.nix
# Provides _module.args.linuxBuild for x86_64-linux perSystem modules.
{inputs, ...}: {
  perSystem = {
    common,
    pkgs,
    system,
    lib,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      inherit (common) sourceLib commonSources;
      inherit (sourceLib) installerClusters;

      genClusters = lib.genAttrs installerClusters;

      inherit (common) nodejs yarn yarn2nix offlineCache srcLockfiles srcWithoutNix electronVersion originalPackageJson;

      package = newPackage;

      unsignedInstaller = selfExtractingArchive;

      makeSignedInstaller = genClusters (cluster:
        pkgs.writeShellScriptBin "make-signed-installer-stub" ''
          echo "We don't sign native code for '${system}', please, use unsigned 'nix build .#installer-${cluster}'"
          exit 1
        '');

      inherit (sourceLib) buildRev;

      node_modules = pkgs.stdenv.mkDerivation {
        name = "daedalus-node_modules";
        src = srcLockfiles;
        nativeBuildInputs =
          [yarn nodejs]
          ++ (with pkgs; [pkg-config jq python3])
          ++ [pkgs.jq];
        buildInputs = with pkgs; [libusb1];
        configurePhase = common.setupCacheAndGypDirs;
        buildPhase = ''
          # Do not look up in the registry, but in the offline cache:
          ${yarn2nix.fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock

          yarn install --ignore-scripts

          # Remove all prebuilt *.node files extracted from `.tgz`s
          find . -type f -name '*.node' -not -path '*/@swc*/*' -exec rm -vf {} ';'

          patchShebangs . >/dev/null  # a real lot of paths to patch, no need to litter logs

          # And now, with correct shebangs, run the install scripts:
          find node_modules -type f -name 'package.json' | sort | xargs grep -F '"install":' | cut -d: -f1 | while IFS= read -r dependency ; do
            if [ "$(jq .scripts.install "$dependency")" != "null" ] ; then
              case "$dependency" in
                */electron/package.json | */electron-chromedriver/package.json)
                  echo "Skipping binary-download install script for $dependency (binary provided by Nix)"
                  continue ;;
              esac
              echo ' '
              echo "Running the install script for '$dependency':"
              ( cd "$(dirname "$dependency")" ; yarn run install ; )
            fi
          done

          patchShebangs . >/dev/null  # a few new files will have appeared
        '';
        installPhase = ''
          mkdir $out
          cp -r node_modules $out/
        '';
        dontFixup = true;
      };

      daedalusJs = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus-js";
          src = srcWithoutNix;
          nativeBuildInputs =
            [yarn nodejs]
            ++ (with pkgs; [pkg-config jq python3])
            ++ [pkgs.jq];
          buildInputs = with pkgs; [libusb1];
          CARDANO_WALLET_VERSION = common.cardanoWalletVersion;
          CARDANO_NODE_VERSION = common.cardanoNodeVersion;
          CI = "nix";
          NETWORK = common.daedalusConfigs.${cluster}.daedalusConfig.networkName;
          BUILD_REV = sourceLib.buildRev;
          BUILD_REV_SHORT = sourceLib.buildRevShort;
          BUILD_COUNTER = sourceLib.buildCounter;
          NODE_ENV = "production";
          BUILDTYPE = "Release";
          configurePhase =
            common.setupCacheAndGypDirs
            + ''
              # Grab all cached `node_modules` from above:
              cp -r ${node_modules}/. ./
              chmod -R +w .
            '';
          patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
            pkgs.lib.recursiveUpdate originalPackageJson {
              productName = common.daedalusConfigs.${cluster}.installerConfig.spacedName;
              main = "dist/main/index.js";
            }
          ));
          buildPhase = ''
            cp -v $patchedPackageJson package.json

            patchShebangs .
            sed -r 's#.*patch-electron-rebuild.*#${common.patchElectronRebuild}/bin/*#' -i scripts/rebuild-native-modules.sh
            yarn build:electron

            ${common.temporaryNodeModulesPatches}

            yarn build:main
            yarn build:renderer
          '';
          installPhase = ''
            mkdir -p $out/bin $out/share/daedalus
            cp -R dist/. $out/share/daedalus/.
            cp $patchedPackageJson $out/share/daedalus/package.json

            chmod +w $out/share/daedalus/package.json
            sed -r 's,"dist/main/index.js","main/index.js",g' -i $out/share/daedalus/package.json

            # XXX: the webpack utils embed the original source paths into map files, which causes the derivation
            # to depend on the original inputs at the nix layer, and double the size of the linux installs.
            # this will just replace all storepaths with an invalid one:
            (
              cd $out/share/daedalus
              for x in {main,renderer}/{0.,}index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
                ${pkgs.nukeReferences}/bin/nuke-refs $x
              done
            )

            mkdir -p $out/share/fonts
            ln -sv $out/share/daedalus/renderer/assets $out/share/fonts/daedalus

            mkdir -pv $out/share/daedalus/node_modules
            jq -r '.[]' <${../../../packaging/runtime-nodejs-deps.json} | while IFS= read -r rtdep ; do
              mkdir -p "$out/share/daedalus/node_modules/$(dirname "$rtdep")"
              cp -r node_modules/"$rtdep" $out/share/daedalus/node_modules/"$rtdep"
            done

            chmod -R +w $out

            # XXX: they increase the closure (i.e. installer) size greatly:
            echo 'Deleting all redundant /nix/store references from to-be-distributed node_modules/:'
            (
              cd $out/share/daedalus/
              find node_modules -type f '(' -name '*.o' -o -name '*.o.d' -o -name '*.target.mk' -o -name '*.Makefile' -o -name 'Makefile' -o -name 'config.gypi' ')' -exec rm -vf '{}' ';'

              # Get rid of ${nodejs}, too – another 60 MiB:
              cd node_modules/
              for file in $(grep -RF ${nodejs} . 2>/dev/null | cut -d: -f1) ; do
                sed -r 's,^#!${nodejs}/bin/,#!/usr/bin/env ,g' -i "$file"
              done
            )

            mkdir -p $out/share/daedalus/node_modules/usb/build
            cp node_modules/usb/build/Debug/usb_bindings.node $out/share/daedalus/node_modules/usb/build

            mkdir -p $out/share/daedalus/node_modules/node-hid/build
            cp node_modules/node-hid/build/Debug/HID_hidraw.node $out/share/daedalus/node_modules/node-hid/build

            find $out/share/daedalus/node_modules -type f -iname '*.node' | while IFS= read -r file ; do
              $STRIP "$file"
              patchelf --set-rpath ${relocatableElectron}/lib/electron/lib "$file"
            done
          '';
          dontFixup = true;
        });

      electron-loader = pkgs.glibc;

      electronBundleExe = let
        additionalLibs = ''
          additionalLibs=(
            ${pkgs.xorg.libX11}/lib/libX11-xcb.so.1
            ${pkgs.xorg.libxcb}/lib/*.so.?
            ${pkgs.systemd}/lib/{libudev.so.1,libsystemd.so.0,libnss_*.so.2}
            ${pkgs.nss}/lib/*.so
            ${pkgs.libusb1}/lib/*.so.0
            ${pkgs.nssmdns}/lib/*.so.2
            ${pkgs.numactl}/lib/libnuma.so.1
            ${pkgs.pciutils}/lib/libpci.so.3
            ${pkgs.libva.out}/lib/*.so.2
            ${pkgs.atk}/lib/libatk-bridge-2.0.so
            ${pkgs.libgbm}/lib/libgbm.so.1
            ${pkgs.mesa}/lib/gbm/dri_gbm.so
            $(find ${pkgs.glibc}/lib -type l)
          )
        '';
      in
        (import (pkgs.runCommand "nix-bundle-exe-patched" {} ''
            cp -r ${inputs.nix-bundle-exe} $out
            chmod -R +w $out
            ${additionalLibs}
            for additionalLib in "''${additionalLibs[@]}" ; do
              sed -r '/bundleExe "\$binary"/a\  bundleLib "'"$additionalLib"'" "lib"' -i $out/bundle-linux.sh
            done
          '') {
            exe_dir = "lib/electron";
            lib_dir = "lib/electron/lib";
            inherit pkgs;
          }
          electronBin).overrideAttrs (drv: {
          buildCommand =
            additionalLibs
            + (builtins.replaceStrings ["find '"] ["find -L '"] drv.buildCommand);
        });

      relocatableElectron = pkgs.stdenv.mkDerivation {
        name = "relocatable-electron";
        dontUnpack = true;
        dontFixup = true;
        nativeBuildInputs = [pkgs.patchelf];
        buildCommand = ''
          mkdir -p $out/lib $out/bin $out/share/X11
          cp -RL ${pkgs.xkeyboard-config}/etc/X11/xkb $out/share/X11/xkb

          # Start with the nix-bundle-exe output: patched binary + bundled .so deps
          cp -R ${electronBundleExe}/lib/electron $out/lib/
          chmod -R +w $out/lib/electron

          # Overlay the raw electron app assets; -n ensures the patched binary and
          # bundled libs from electronBundleExe are not clobbered:
          cp -Rn ${electronBin}/lib/electron/. $out/lib/electron/

          cp ${electron-loader}/lib/ld-linux-x86-64.so.2 $out/lib/electron/
          rm -f $out/lib/electron/lib/ld-linux-x86-64.so.2
          ( cd $out/lib/electron && mv libffmpeg.so lib/libffmpeg.so && ln -s lib/libffmpeg.so libffmpeg.so ; )
          ( cd $out/lib/electron/lib && ln -s libatk-bridge-2.0.so libatk-bridge.so ; )

          # nixpkgs-25.11: libgtk-3 now depends on libtinysparql, which has libsqlite3.so
          # as a full-path DT_NEEDED that was nuked to eeee... Replace with a soname so
          # the bundled sqlite copy can satisfy it.
          cp ${pkgs.sqlite.out}/lib/libsqlite3.so.0 $out/lib/electron/lib/
          for f in $out/lib/electron/lib/libtinysparql-3.0.so*; do
            patchelf --replace-needed \
              '/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-sqlite-3.50.4/lib/libsqlite3.so' \
              'libsqlite3.so.0' \
              "$f"
          done

          patchelf --set-rpath '$ORIGIN/lib:$ORIGIN' $out/lib/electron/electron

          # Embed the bundled ld-linux as the ELF interpreter so electron can be exec'd
          # directly.
          patchelf --set-interpreter $out/lib/electron/ld-linux-x86-64.so.2 $out/lib/electron/electron

          cp ${pkgs.writeScript "electron" ''
            #!/bin/sh
            if [ -z "''${XCURSOR_PATH}" ] && [ -d "/usr/share/icons" ]; then
              # Debians don't set this, and in effect all cursors are 2x too small on HiDPI displays:
              export XCURSOR_PATH="/usr/share/icons"
            fi
            # nix-bundle-exe nukes the xkeyboard-config path baked into libxkbcommon.so;
            # restore it so keyboard input works:
            LIB_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")/lib"
            BUNDLE_DIR="$(dirname "$LIB_DIR")"
            export XKB_CONFIG_ROOT="$BUNDLE_DIR/share/X11/xkb"
            export GBM_BACKENDS_PATH="$LIB_DIR/electron/lib"
            # DAEDALUS_DISABLE_GPU=1 disables Chromium hardware GPU acceleration.
            if [ -n "''${DAEDALUS_DISABLE_GPU}" ]; then
              set -- "--disable-gpu" "$@"
            fi
            # Run electron directly (interpreter is embedded via patchelf above):
            exec "$LIB_DIR"/electron/electron "$@"
          ''} $out/bin/electron
        '';
        meta.mainProgram = "electron";
      };

      # A completely portable directory that you can run on _any_ Linux:
      newBundle = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus-bundle";
          meta.mainProgram = "daedalus";
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

            # The electron binary has a Nix store path as its ELF interpreter (PT_INTERP) pointing to
            # the relocatableElectron derivation. After installing the bundle outside the Nix store, that
            # path won't exist. Bundle a static patchelf so the installer script can re-patch PT_INTERP
            # to the installed ld-linux path after extraction.
            cp ${(pkgs.pkgsStatic.patchelf.overrideAttrs (_: {doCheck = false;}))}/bin/patchelf $out/libexec/.patchelf-static
            chmod +x $out/libexec/.patchelf-static

            chmod -R +w $out/share/applications/
            cp ${desktopItemTemplate.${cluster}}/share/applications/*.desktop $out/share/applications/Daedalus-${cluster}.desktop
          '';
        });

      # A package that will work only on NixOS:
      newPackage = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus";
          meta.mainProgram = "daedalus";
          dontUnpack = true;
          buildCommand = ''
            mkdir -p $out/{bin,libexec,config}

            cp -r ${common.daedalusConfigs.${cluster}.configFiles}/. $out/config/

            ln -sf ${import inputs.nix-bundle-exe {inherit pkgs;} common.daedalus-bridge.${cluster}} $out/libexec/bundle-daedalus-bridge
            ( cd $out/libexec/ && ln -sf bundle-daedalus-bridge/bin/* ./ ; )

            ln -sf ${daedalusJs.${cluster}}/share/daedalus $out/libexec/daedalus-js

            ln -sf ${relocatableElectron} $out/libexec/bundle-electron
            ( cd $out/libexec/ && ln -sf bundle-electron/bin/* ./ ; )

            cp ${pkgs.writeText "daedalus" ''
              #!/bin/sh

              if [ -n "$LD_LIBRARY_PATH" ]; then
                echo >&2 "Warning: 'LD_LIBRARY_PATH' is set, it's been known to cause problems in the past, unsetting it."
                unset LD_LIBRARY_PATH
              fi

              set -ex

              ENTRYPOINT_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")"
              export ENTRYPOINT_DIR
              export PATH="$ENTRYPOINT_DIR/libexec:$PATH"

              export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
              export CLUSTER=${cluster}
              export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
              export DAEDALUS_CONFIG="$ENTRYPOINT_DIR/config"

              mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Logs/pub
              mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Secrets
              cd "''${DAEDALUS_DIR}/${cluster}/"

              exec cardano-watchdog \
                --config "$ENTRYPOINT_DIR/config/daedalus-config.json" \
                --pub-logs-dir "''${DAEDALUS_DIR}/${cluster}/Logs/pub" \
                --tls-dir "''${DAEDALUS_DIR}/${cluster}/tls"
            ''} $out/bin/daedalus

            cp ${pkgs.writeText "daedalus-frontend" ''
              #!/bin/sh
              set -xe

              # `daedalus-frontend` is what `cardano-launcher` restarts during auto-update; let's detect
              # this case here, and restart the `cardano-launcher` itself, in case we need it to
              # be updated as well:
              if [ -e "''${DAEDALUS_DIR}/${cluster}"/daedalus_lockfile.pre-auto-update ] ; then
                nohup setsid ~/.daedalus/${cluster}/bin/daedalus </dev/null >/dev/null 2>/dev/null &
                exit 0
              fi

              # Escape hatch for Linux-only Chromium switches that have no cross-platform
              # equivalent, e.g. `DAEDALUS_ELECTRON_FLAGS=--ozone-platform=wayland`. Left
              # unquoted on purpose, so that several flags can be passed at once.
              # shellcheck disable=SC2086
              exec electron ''${DAEDALUS_ELECTRON_FLAGS-} --disable-setuid-sandbox --no-sandbox "$ENTRYPOINT_DIR"/libexec/daedalus-js "$@"
            ''} $out/libexec/daedalus-frontend

            cp ${pkgs.writeText "update-runner" ''
              #!/bin/sh
              set -xe
              exec "$1"
            ''} $out/libexec/update-runner

            chmod +x $out/bin/* $out/libexec/{daedalus-frontend,update-runner}

            mkdir -p $out/share/applications
            cp ${common.daedalusConfigs.${cluster}.installerConfig.iconPath.large} $out/share/icon_large.png
            (
              cd $out/share/applications/
              cp ${desktopItemTemplate.${cluster}}/share/applications/*.desktop ./Daedalus-${cluster}.desktop
              chmod +w *.desktop
              sed -r "s,INSERT_PATH_HERE,$out/bin/daedalus,g" -i *.desktop
              sed -r "s,INSERT_ICON_PATH_HERE,$out/share/icon_large.png,g" -i *.desktop
            )
          '';
        });

      desktopItemTemplate = genClusters (cluster:
        pkgs.makeDesktopItem {
          name = "Daedalus-${cluster}";
          exec = "INSERT_PATH_HERE";
          desktopName = "Daedalus ${cluster}";
          genericName = "Crypto-Currency Wallet";
          categories = ["Application" "Network"];
          icon = "INSERT_ICON_PATH_HERE";
          startupWMClass = common.daedalusConfigs.${cluster}.installerConfig.spacedName;
        });

      selfExtractingArchive = genClusters (cluster: let
        scriptTemplate =
          __replaceStrings [
            "@CLUSTER@"
            "@REMOVE_OLD_NIX_CHROOT@"
          ] [
            (lib.escapeShellArg cluster)
            removeOldNixChroot.${cluster}
          ] (__readFile ../../../packaging/linux-self-extracting-archive.sh);
        script = __replaceStrings ["1010101010"] [(toString (1000000000 + __stringLength scriptTemplate))] scriptTemplate;
        version = (builtins.fromJSON (builtins.readFile ../../../package.json)).version;
      in
        pkgs.runCommand "daedalus-${cluster}-installer" {
          inherit script;
          passAsFile = ["script"];
          meta.mainProgram = "daedalus-${cluster}-installer";
        } ''
          mkdir -p $out
          target=$out/daedalus-${version}-${toString sourceLib.buildCounter}-${cluster}-${sourceLib.buildRevShort}-x86_64-linux.bin
          cat $scriptPath >$target
          chmod +x $target

          echo 'Compressing (xz)...'
          tar -cJf tmp-archive.tar.xz -C ${newBundle.${cluster}} . -C ${satisfyOldUpdateRunner.${cluster}} .

          checksum=$(sha256sum tmp-archive.tar.xz | cut -d' ' -f1)
          sed -r "s/0000000000000000000000000000000000000000000000000000000000000000/$checksum/g" -i $target

          cat tmp-archive.tar.xz >>$target

          # Create bin directory with hard link for nix run
          mkdir -p $out/bin
          ln $target $out/bin/daedalus-${cluster}-installer

          # Make it downloadable from Hydra:
          mkdir -p $out/nix-support
          echo "file binary-dist \"$target\"" >$out/nix-support/hydra-build-products
        '');

      removeOldNixChroot = genClusters (cluster: ''
        old_nix="$HOME"/.daedalus/nix
        old_etc="$HOME"/.daedalus/etc
        if [ -e "$old_nix" ] ; then
          old_clusters=$(ls "$old_nix"/var/nix/profiles/ | grep '^profile-' | grep -v '[0-9]' || true)
          if [ "$old_clusters" = "profile-${cluster}" ] ; then
            echo "Found an older non-portable version of Daedalus in $old_nix, removing it..."
            chmod -R +w "$old_nix"
            chmod -R +w "$old_etc" || true
            rm -rf "$old_nix" "$old_etc" || true
          else
            echo "Found older non-portable versions of Daedalus for multiple networks in $old_nix, you are free to remove the directory manually, if you no longer use them."
          fi
        fi
      '');

      satisfyOldUpdateRunner = genClusters (cluster: let
        tarball = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
          fileName = "tarball";
          contents = [];
          storeContents = [
            {
              symlink = "firstGeneration";
              object = pkgs.buildEnv {
                name = "profile";
                paths = [
                  (pkgs.writeShellScriptBin "daedalus-frontend" ''
                    set -euo pipefail
                    echo -n "$HOME/.daedalus${pkgs.writeScript "escape-and-scrap-chroot" ''
                      #!/bin/sh
                      set -eu
                      nohup setsid ~/.daedalus/${cluster}/bin/daedalus </dev/null >/dev/null 2>/dev/null &
                      sleep 5
                      ${removeOldNixChroot.${cluster}}
                    ''}" >/escape-hatch
                    exit 0
                  '')
                ];
              };
            }
          ];
        };
      in
        pkgs.runCommand "satisfy-old-update-runner" {} ''
          mkdir -p $out/dat${tarball}
          cp -r ${tarball}/. $out/dat${tarball}/
        '');

      # Use pkgs.electron.unwrapped (from nixpkgs) directly.
      electronBin = pkgs.runCommand "electron-${electronVersion}" {} ''
        mkdir -p $out/lib $out/bin
        cp -r ${pkgs.electron.unwrapped}/libexec/electron $out/lib/
        ln -sf $out/lib/electron/electron $out/bin/electron
      '';

      linuxBuild = {
        inherit
          package
          unsignedInstaller
          makeSignedInstaller
          buildRev
          node_modules
          daedalusJs
          electron-loader
          electronBundleExe
          relocatableElectron
          newBundle
          newPackage
          desktopItemTemplate
          selfExtractingArchive
          removeOldNixChroot
          satisfyOldUpdateRunner
          electronBin
          ;
        inherit
          (common)
          nodejs
          yarn
          yarn2nix
          offlineCache
          srcLockfiles
          srcWithoutNix
          electronVersion
          originalPackageJson
          ;
      };
    in {
      _module.args.linuxBuild = linuxBuild;
    });
}
