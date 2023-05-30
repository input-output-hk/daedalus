{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-linux";

let

  newCommon = import ./new-common.nix { inherit inputs targetSystem cluster; };

  inherit (newCommon) sourceLib oldCode pkgs commonSources;
  inherit (pkgs) lib;

in rec {

  inherit newCommon oldCode;
  inherit (newCommon) nodejs nodePackages yarn yarn2nix offlineCache srcLockfiles srcWithoutNix electronVersion electronChromedriverVersion originalPackageJson;

  package = mkDaedalus { sandboxed = false; };

  unsignedInstaller = linuxInstaller.wrappedBundle;

  makeSignedInstaller = throw "We don’t sign binary files inside installers for ‘${targetSystem}’, you’re good.";

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
    configurePhase = newCommon.setupCacheAndGypDirs + linuxSpecificCaches;
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

  daedalusJs = pkgs.stdenv.mkDerivation {
    name = "daedalus-js";
    src = srcWithoutNix;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with pkgs; [ python3 pkgconfig ]);
    buildInputs = with pkgs; [ libusb ];
    CARDANO_WALLET_VERSION = oldCode.cardanoWalletVersion;
    CARDANO_NODE_VERSION = oldCode.cardanoNodeVersion;
    CI = "nix";
    NETWORK = oldCode.launcherConfigs.launcherConfig.networkName;
    BUILD_REV = sourceLib.buildRev;
    BUILD_REV_SHORT = sourceLib.buildRevShort;
    BUILD_COUNTER = sourceLib.buildCounter;
    NODE_ENV = "production";
    BUILDTYPE = "Release";
    configurePhase = newCommon.setupCacheAndGypDirs + linuxSpecificCaches + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
      pkgs.lib.recursiveUpdate originalPackageJson {
        productName = oldCode.launcherConfigs.installerConfig.spacedName;
        main = "dist/main/index.js";
      }
    ));
    buildPhase = ''
      cp -v $patchedPackageJson package.json

      patchShebangs .
      sed -r 's#.*patchElectronRebuild.*#${newCommon.patchElectronRebuild}/bin/*#' -i scripts/rebuild-native-modules.sh
      yarn build:electron

      yarn run package -- --name ${lib.escapeShellArg oldCode.launcherConfigs.installerConfig.spacedName}
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
      cp -r node_modules/{\@babel,\@noble,\@protobufjs,regenerator-runtime,node-fetch,\@trezor,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib} $out/share/daedalus/node_modules/

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

      for file in $out/share/daedalus/node_modules/usb/build/usb_bindings.node $out/share/daedalus/node_modules/node-hid/build/HID_hidraw.node $out/share/daedalus/node_modules/usb-detection/build/detection.node; do
        $STRIP $file
        patchelf --shrink-rpath $file
      done
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

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

  mkDaedalus = { sandboxed }: import ../installers/nix/linux.nix {
    inherit (pkgs) stdenv runCommand writeText writeScriptBin coreutils
      utillinux procps gsettings-desktop-schemas gtk3 hicolor-icon-theme xfce;
    inherit (oldCode) daedalus-bridge daedalus-installer;
    inherit cluster sandboxed;

    rawapp = daedalusJs;
    electron = electronBin;

    # FIXME: ???
    inherit (oldCode) launcherConfigs;
    linuxClusterBinName = cluster;
  };

  linuxInstaller = rec {

    installPath = ".daedalus";

    # FIXME: why our own fork?
    nix-bundle-src = pkgs.fetchFromGitHub {
      owner = "input-output-hk"; repo = "nix-bundle";
      rev = "a43e9280628d6e7fcc2f89257106f5262d531bc7";
      sha256 = "10qgincrs8fjdl16mld6lzd69syhyzwx65lcbz4widnkdvhlwh3i";
    };

    nix-bundle = import nix-bundle-src { nixpkgs = pkgs; };
    iconPath = oldCode.launcherConfigs.installerConfig.iconPath;
    linuxClusterBinName = cluster;

    namespaceHelper = pkgs.writeScriptBin "namespaceHelper" ''
      #!/usr/bin/env bash

      set -e

      cd ~/${installPath}/
      mkdir -p etc
      cat /etc/hosts > etc/hosts
      cat /etc/nsswitch.conf > etc/nsswitch.conf
      cat /etc/localtime > etc/localtime
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf

      if [ "x$DEBUG_SHELL" == x ]; then
        exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${linuxClusterBinName}/bin/enter-phase2 daedalus
      else
        exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${linuxClusterBinName}/bin/enter-phase2 bash
      fi
    '';

    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus-${linuxClusterBinName}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus ${linuxClusterBinName}";
      genericName = "Crypto-Currency Wallet";
      categories = [ "Application" "Network" ];
      icon = "INSERT_ICON_PATH_HERE";
    };

    postInstall = pkgs.writeScriptBin "post-install" ''
      #!${pkgs.stdenv.shell}

      set -ex


      test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus/${cluster}"
      mkdir -pv $DAEDALUS_DIR/Logs/pub

      exec 2>&1 > $DAEDALUS_DIR/Logs/pub/post-install.log

      echo "in post-install hook"

      cp -f ${iconPath.large} $DAEDALUS_DIR/icon_large.png
      cp -f ${iconPath.small} $DAEDALUS_DIR/icon.png
      cp -Lf ${namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper
      mkdir -pv ~/.local/bin ''${XDG_DATA_HOME}/applications
      cp -Lf ${namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${linuxClusterBinName}

      cat ${desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon_large.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus-${linuxClusterBinName}.desktop"
    '';

    xdg-open = pkgs.writeScriptBin "xdg-open" ''
      #!${pkgs.stdenv.shell}

      echo -n "xdg-open \"$1\"" > /escape-hatch
    '';

    preInstall = pkgs.writeText "pre-install" ''
      if grep sse4 /proc/cpuinfo -q; then
        echo 'SSE4 check pass'
      else
        echo "ERROR: your cpu lacks SSE4 support, cardano will not work"
        exit 1
      fi
    '';

    newBundle = let
      daedalus' = mkDaedalus { sandboxed = true; };
    in (import ../installers/nix/nix-installer.nix {
      inherit postInstall preInstall linuxClusterBinName;
      rawapp = daedalusJs;
      inherit pkgs;
      installationSlug = installPath;
      installedPackages = [ daedalus' postInstall namespaceHelper daedalus'.cfg oldCode.daedalus-bridge daedalus'.daedalus-frontend xdg-open ];
      nix-bundle = nix-bundle;
    }).installerBundle;

    wrappedBundle = let
      version = (builtins.fromJSON (builtins.readFile ../package.json)).version;
      fn = "daedalus-${version}.${toString sourceLib.buildCounter}-${linuxClusterBinName}-${sourceLib.buildRevShort}-x86_64-linux.bin";
    in pkgs.runCommand fn {} ''
      mkdir -p $out
      cp ${newBundle} $out/${fn}
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
