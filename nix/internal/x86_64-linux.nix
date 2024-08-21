{ inputs, targetSystem }:

assert targetSystem == "x86_64-linux";

let

  common = import ./common.nix { inherit inputs targetSystem; };

  inherit (common) sourceLib pkgs commonSources;
  inherit (sourceLib) installerClusters;
  inherit (pkgs) lib;

  genClusters = lib.genAttrs installerClusters;

in rec {

  inherit common;
  inherit (common) nodejs yarn yarn2nix offlineCache srcLockfiles srcWithoutNix electronVersion electronChromedriverVersion originalPackageJson;

  package = newPackage;

  unsignedInstaller = selfExtractingArchive;

  makeSignedInstaller = genClusters (cluster: pkgs.writeShellScriptBin "make-signed-installer-stub" ''
    echo "We don’t sign native code for ‘${targetSystem}’, please, use unsigned ‘nix build .#installer-${cluster}’"
    exit 1
  '');

  # FIXME: for Tullia/Cicero debugging, remove later:
  inherit (sourceLib) buildRev;

  # The following is used in all `configurePhase`s:
  linuxSpecificCaches = let
    cacheDir = "$HOME/.cache";
  in ''
    mkdir -p ${cacheDir}/electron/${commonSources.electronCacheHash}/
    ln -sf ${commonSources.electronShaSums} ${cacheDir}/electron/${commonSources.electronCacheHash}/SHASUMS256.txt
    ln -sf ${linuxSources.electron} ${cacheDir}/electron/${commonSources.electronCacheHash}/electron-v${electronVersion}-linux-x64.zip

    mkdir -p ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/
    ln -sf ${commonSources.electronChromedriverShaSums} ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/SHASUMS256.txt
    ln -sf ${linuxSources.electronChromedriver} ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/chromedriver-v${electronChromedriverVersion}-linux-x64.zip
  '';

  node_modules = pkgs.stdenv.mkDerivation {
    name = "daedalus-node_modules";
    src = srcLockfiles;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with pkgs; [ python3 pkgconfig jq ]);
    buildInputs = with pkgs; [ libusb ];
    configurePhase = common.setupCacheAndGypDirs + linuxSpecificCaches;
    buildPhase = ''
      # Do not look up in the registry, but in the offline cache:
      ${yarn2nix.fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock

      # Now, install from offlineCache to node_modules/, but do not
      # execute any scripts defined in the project package.json and
      # its dependencies we need to `patchShebangs` first, since even
      # ‘/usr/bin/env’ is not available in the build sandbox
      yarn install --ignore-scripts

      # Remove all prebuilt *.node files extracted from `.tgz`s
      find . -type f -name '*.node' -not -path '*/@swc*/*' -exec rm -vf {} ';'

      patchShebangs . >/dev/null  # a real lot of paths to patch, no need to litter logs

      # And now, with correct shebangs, run the install scripts (we have to do that
      # semi-manually, because another `yarn install` will overwrite those shebangs…):
      find node_modules -type f -name 'package.json' | sort | xargs grep -F '"install":' | cut -d: -f1 | while IFS= read -r dependency ; do
        # The grep pre-filter is not ideal:
        if [ "$(jq .scripts.install "$dependency")" != "null" ] ; then
          echo ' '
          echo "Running the install script for ‘$dependency’:"
          ( cd "$(dirname "$dependency")" ; yarn run install ; )
        fi
      done

      patchShebangs . >/dev/null  # a few new files will have appeared
    '';
    installPhase = ''
      mkdir $out
      cp -r node_modules $out/
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

  daedalusJs = genClusters (cluster: pkgs.stdenv.mkDerivation {
    name = "daedalus-js";
    src = srcWithoutNix;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with pkgs; [ python3 pkgconfig ]);
    buildInputs = with pkgs; [ libusb ];
    CARDANO_WALLET_VERSION = common.cardanoWalletVersion;
    CARDANO_NODE_VERSION = common.cardanoNodeVersion;
    CI = "nix";
    NETWORK = common.launcherConfigs.${cluster}.launcherConfig.networkName;
    BUILD_REV = sourceLib.buildRev;
    BUILD_REV_SHORT = sourceLib.buildRevShort;
    BUILD_COUNTER = sourceLib.buildCounter;
    NODE_ENV = "production";
    BUILDTYPE = "Release";
    configurePhase = common.setupCacheAndGypDirs + linuxSpecificCaches + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
      pkgs.lib.recursiveUpdate originalPackageJson {
        productName = common.launcherConfigs.${cluster}.installerConfig.spacedName;
        main = "dist/main/index.js";
      }
    ));
    buildPhase = ''
      cp -v $patchedPackageJson package.json

      patchShebangs .
      sed -r 's#.*patchElectronRebuild.*#${common.patchElectronRebuild}/bin/*#' -i scripts/rebuild-native-modules.sh
      yarn build:electron

      ${common.temporaryNodeModulesPatches}

      yarn run package -- --name ${lib.escapeShellArg common.launcherConfigs.${cluster}.installerConfig.spacedName}
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
      cp -r node_modules/{\@babel,\@noble,\@protobufjs,regenerator-runtime,node-fetch,\@sinclair,\@trezor,\@fivebinaries,\@emurgo,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib,tr46,ts-mixer,usb,whatwg-url} $out/share/daedalus/node_modules/

      chmod -R +w $out

      # XXX: they increase the closure (i.e. installer) size greatly:
      echo 'Deleting all redundant /nix/store references from to-be-distributed ‘node_modules/’:'
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

      mkdir -p $out/share/daedalus/node_modules/usb-detection/build
      # TODO: we took Release/detection.node before `rebuild-native-modules.sh` ever existed – is this still fine?
      cp node_modules/usb-detection/build/Debug/detection.node $out/share/daedalus/node_modules/usb-detection/build

      find $out/share/daedalus/node_modules -type f -iname '*.node' | while IFS= read -r file ; do
        $STRIP "$file"
        patchelf --set-rpath ${relocatableElectron}/lib/electron/lib "$file"
      done
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  });

  electron-loader = pkgs.glibc.overrideAttrs (drv: {
    patches = (drv.patches or []) ++ [
      ./glibc-electron-loader.patch
    ];
  });

  relocatableElectron = let
    additionalLibs = ''
      additionalLibs=(
        ${pkgs.xorg.libX11}/lib/libX11-xcb.so.1
        ${pkgs.xorg.libxcb}/lib/*.so.?
        ${pkgs.systemd /* patched */}/lib/{libudev.so.1,libsystemd.so.0,libnss_*.so.2}
        ${pkgs.nss}/lib/*.so
        ${pkgs.libusb}/lib/*.so.0
        ${pkgs.nssmdns}/lib/*.so.2
        ${pkgs.numactl}/lib/libnuma.so.1
        ${pkgs.pciutils}/lib/libpci.so.3
        ${pkgs.libva.out}/lib/*.so.2
        ${pkgs.atk}/lib/libatk-bridge-2.0.so
        $(find ${pkgs.glibc}/lib -type l)
      )
    '';
  in (import (pkgs.runCommandNoCC "nix-bundle-exe-patched" {} ''
    cp -r ${inputs.nix-bundle-exe} $out
    chmod -R +w $out
    ${additionalLibs}
    for additionalLib in "''${additionalLibs[@]}" ; do
      sed -r '/bundleExe "\$binary"/a\  bundleLib "'"$additionalLib"'" "lib"' -i $out/bundle-linux.sh
    done
  '') {
    exe_dir = "electron";
    lib_dir = "electron/lib";
    #bin_dir = "electron-bin";
    inherit pkgs;
  } electronBin).overrideAttrs (drv: {
    buildCommand = additionalLibs + (builtins.replaceStrings ["find '"] ["find -L '"] drv.buildCommand) + ''
      chmod -R +w $out

      mkdir -p $out/lib
      cp -R ${electronBin}/lib/electron $out/lib/
      ( cd $out/electron && ${pkgs.rsync}/bin/rsync -Rah . $out/lib/electron/ ; )
      rm -rf $out/electron/
      cp ${electron-loader}/lib/ld-linux-x86-64.so.2 $out/lib/electron/

      rm $out/lib/electron/lib/ld-linux-x86-64.so.2
      ( cd $out/lib/electron && rm libffmpeg.so && ln -s lib/libffmpeg.so libffmpeg.so ; )

      ( cd $out/lib/electron/lib && ln -s libatk-bridge-2.0.so libatk-bridge.so ; )

      patchelf --set-rpath '$ORIGIN/lib:$ORIGIN' $out/lib/electron/electron

      cp ${pkgs.writeScript "electron" ''
        #!/bin/sh
        if [ -z "''${XCURSOR_PATH}" ] && [ -d "/usr/share/icons" ]; then
          # Debians don't set this, and in effect all cursors are 2x too small on HiDPI displays:
          export XCURSOR_PATH="/usr/share/icons"
        fi
        LIB_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")/lib"
        export LD_PLEASE_INTERPRET="$LIB_DIR"/electron/electron
        exec "$LIB_DIR"/electron/ld-linux-x86-64.so.2 "$@"
      ''} $out/bin/electron
    '';
    meta.mainProgram = "electron";
  });

  # A completely portable directory that you can run on _any_ Linux:
  newBundle = genClusters (cluster: pkgs.stdenv.mkDerivation {
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

      chmod -R +w $out/share/applications/
      cp ${desktopItemTemplate.${cluster}}/share/applications/*.desktop $out/share/applications/Daedalus-${cluster}.desktop
    '';
  });

  # A package that will work only on NixOS; the only differences from the bundle above are:
  #   • symlinks instead of copying (to not waste /nix/store space when iterating on something),
  #   • and RPATH of the native *.node modules points to ${relocatableElectron}, not relative to $ORIGIN.
  newPackage = genClusters (cluster: pkgs.stdenv.mkDerivation {
    name = "daedalus";
    meta.mainProgram = "daedalus";
    dontUnpack = true;
    buildCommand = ''
      mkdir -p $out/{bin,libexec,config}

      cp -r ${common.launcherConfigs.${cluster}.configFiles}/. $out/config/

      ln -sf ${import inputs.nix-bundle-exe { inherit pkgs; } common.daedalus-bridge.${cluster}} $out/libexec/bundle-daedalus-bridge
      ( cd $out/libexec/ && ln -sf bundle-daedalus-bridge/bin/* ./ ; )

      ln -sf ${daedalusJs.${cluster}}/share/daedalus $out/libexec/daedalus-js

      ln -sf ${relocatableElectron} $out/libexec/bundle-electron
      ( cd $out/libexec/ && ln -sf bundle-electron/bin/* ./ ; )

      cp ${pkgs.writeText "daedalus" ''
        #!/bin/sh

        if [ -n "$LD_LIBRARY_PATH" ]; then
          echo >&2 'Warning: ‘LD_LIBRARY_PATH’ is set, it’s been known to cause problems in the past, unsetting it.'
          unset LD_LIBRARY_PATH
        fi

        set -ex

        ENTRYPOINT_DIR="$(dirname "$(dirname "$(readlink -f "$0")")")"
        export ENTRYPOINT_DIR
        export PATH="$ENTRYPOINT_DIR/libexec:$PATH"

        XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
        export CLUSTER=${cluster}
        export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
        export DAEDALUS_CONFIG="$ENTRYPOINT_DIR/config"

        mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Logs/pub
        mkdir -p "''${DAEDALUS_DIR}/${cluster}"/Secrets
        cd "''${DAEDALUS_DIR}/${cluster}/"

        if [ -e "''${DAEDALUS_DIR}/${cluster}"/daedalus_lockfile.pre-auto-update ] ; then
          rm "''${DAEDALUS_DIR}/${cluster}"/daedalus_lockfile.pre-auto-update || true
        fi

        exec cardano-launcher --config "$ENTRYPOINT_DIR/config/launcher-config.yaml"
      ''} $out/bin/daedalus

      cp ${pkgs.writeText "daedalus-frontend" ''
        #!/bin/sh
        set -xe

        # `daedalus-frontend` is what `cardano-launcher` restarts during auto-update; let’s detect
        # this case here, and restart the `cardano-launcher` itself, in case we need it to
        # be updated as well:
        if [ -e "''${DAEDALUS_DIR}/${cluster}"/daedalus_lockfile.pre-auto-update ] ; then
          nohup setsid ~/.daedalus/${cluster}/bin/daedalus </dev/null >/dev/null 2>/dev/null &
          exit 0
        fi

        exec electron --disable-setuid-sandbox --no-sandbox "$ENTRYPOINT_DIR"/libexec/daedalus-js "$@"
      ''} $out/libexec/daedalus-frontend

      cp ${pkgs.writeText "update-runner" ''
        #!/bin/sh
        set -xe
        exec "$1"
      ''} $out/libexec/update-runner

      chmod +x $out/bin/* $out/libexec/{daedalus-frontend,update-runner}

      mkdir -p $out/share/applications
      cp ${common.launcherConfigs.${cluster}.installerConfig.iconPath.large} $out/share/icon_large.png
      (
        cd $out/share/applications/
        cp ${desktopItemTemplate.${cluster}}/share/applications/*.desktop ./Daedalus-${cluster}.desktop
        chmod +w *.desktop
        sed -r "s,INSERT_PATH_HERE,$out/bin/daedalus,g" -i *.desktop
        sed -r "s,INSERT_ICON_PATH_HERE,$out/share/icon_large.png,g" -i *.desktop
      )
    '';
  });

  desktopItemTemplate = genClusters (cluster: pkgs.makeDesktopItem {
    name = "Daedalus-${cluster}";
    exec = "INSERT_PATH_HERE";
    desktopName = "Daedalus ${cluster}";
    genericName = "Crypto-Currency Wallet";
    categories = [ "Application" "Network" ];
    icon = "INSERT_ICON_PATH_HERE";
    startupWMClass = common.launcherConfigs.${cluster}.installerConfig.spacedName;
  });

  # On Windows/macOS, auto-update just launches the new installer, and exits the previous Daedalus.
  #
  # On Linux, however, it starts `update-runner` on PATH (updateRunnerBin from launcher-config.yaml),
  # with argv[1] set to the path of the new installer, shows progress, and then exits Daedalus with
  # code 20. Which is a signal for `cardano-launcher` to restart `daedalus-frontend` on PATH
  # (daedalusBin from launcher-config.yaml).
  #
  # The old `update-runner` (≤5.4.0) has certain expectations about what's in the installer, so we
  # have to provide a shim, when our installer is being run with `--extract`.
  #
  # We also have to change `daedalus-frontend` in the old nix-chroot sandbox to use `escape-hatch` to
  # start the new `daedalus` with `xdg-run`, and `exit 0`. But first move the `daedalus_lockfile`
  # to another location, because during the first launch, there will briefly be 2 `cardano-launcher`s.
  #
  # Now, if there’s no previous nix-chroot, i.e. if the upgrade is from ≥5.5.0 to something newer,
  # then TODO

  # XXX: Be *super careful* changing this!!! You WILL DELETE user data if you make a mistake.
  selfExtractingArchive = genClusters (cluster: let
    scriptTemplate = __replaceStrings [
      "@CLUSTER@"
      "@REMOVE_OLD_NIX_CHROOT@"
    ] [
      (lib.escapeShellArg cluster)
      removeOldNixChroot.${cluster}
    ] (__readFile ./linux-self-extracting-archive.sh);
    script = __replaceStrings ["1010101010"] [(toString (1000000000 + __stringLength scriptTemplate))] scriptTemplate;
    version = (builtins.fromJSON (builtins.readFile ../../package.json)).version;
  in pkgs.runCommand "daedalus-${cluster}-installer" {
    inherit script;
    passAsFile = [ "script" ];
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

    # Make it downloadable from Hydra:
    mkdir -p $out/nix-support
    echo "file binary-dist \"$target\"" >$out/nix-support/hydra-build-products
  '');

  # We only want to remove the old nix-chroot, if it contains only one cluster
  # variant – the one we’re updating. Otherwise, we’ll break other cluster
  # installations of that user.
  removeOldNixChroot = genClusters (cluster: ''
    old_nix="$HOME"/.daedalus/nix
    old_etc="$HOME"/.daedalus/etc
    if [ -e "$old_nix" ] ; then
      old_clusters=$(ls "$old_nix"/var/nix/profiles/ | grep '^profile-' | grep -v '[0-9]' || true)
      if [ "$old_clusters" = "profile-${cluster}" ] ; then
        # If the user *only* used Mainnet (most common), we're safe to remove the whole ~/.daedalus/nix:
        echo "Found an older non-portable version of Daedalus in $old_nix, removing it..."
        chmod -R +w "$old_nix"
        chmod -R +w "$old_etc" || true
        rm -rf "$old_nix" "$old_etc" || true
      else
        # But if it contains more Daedaluses for other networks, we can't risk breaking them:
        echo "Found older non-portable versions of Daedalus for multiple networks in $old_nix, you are free to remove the directory manually, if you no longer use them."
      fi
    fi
  '');

  satisfyOldUpdateRunner = genClusters (cluster: let
    tarball = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
      fileName = "tarball"; # don't rename
      contents = [];
      storeContents = [{
        symlink = "firstGeneration";
        object = pkgs.buildEnv {
          name = "profile";
          paths = [
            # We need an auto-update stub to hook into the old auto-update process, and make it launch
            # our new portable Daedalus outside of `nix-chroot`.
            #
            # The previously running `cardano-launcher` (inside the `nix-chroot`) will try to restart
            # `/bin/daedalus-frontend` after a successful update, so we have to hook here: start the new
            # independent (nohup, setsid, don’t inherit fds) Deadalus (and new cardano-launcher) after
            # moving the old cardano-launcher’s lockfile out of the way. The old launcher will exit
            # after our `exit 0` below.
            #
            # And at the very end we get rid of the previous `nix-chroot`.
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
      }];
    };
  in pkgs.runCommandNoCC "satisfy-old-update-runner" {} ''
    mkdir -p $out/dat${tarball}
    cp -r ${tarball}/. $out/dat${tarball}/
  '');

  electronBin = pkgs.stdenv.mkDerivation {
    name = "electron-${electronVersion}";
    src = linuxSources.electron;
    buildInputs = with pkgs; [ unzip makeWrapper ];
    buildCommand = with pkgs; ''
      mkdir -p $out/lib/electron $out/bin
      unzip -d $out/lib/electron $src
      ln -s $out/lib/electron/electron $out/bin

      fixupPhase

      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${atomEnv.libPath}:${lib.makeLibraryPath [ libuuid at-spi2-atk at-spi2-core xorg.libxshmfence libxkbcommon ]}:$out/lib/electron" \
        $out/lib/electron/electron
    '';
  };

  linuxSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-linux-x64.zip";
      hash = "sha256-jXeA3Sr8/l6Uos9XT0+hCiosaRIndx/KSQUcUkrGdRM=";
    };

    electronChromedriver = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/chromedriver-v${electronChromedriverVersion}-linux-x64.zip";
      hash = "sha256-bkeA1l1cBppdsbLISwu8MdC/2E5sjVJx6e+KhLgQ5yA=";
    };
  };

}
