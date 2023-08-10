{ inputs, targetSystem }:

assert targetSystem == "x86_64-windows";

let

  common = import ./common.nix { inherit inputs targetSystem; };

  inherit (common) sourceLib pkgs srcWithoutNix yarn nodejs originalPackageJson commonSources electronVersion;
  inherit (sourceLib) installerClusters;
  inherit (pkgs) lib;

  genClusters = lib.genAttrs installerClusters;

in rec {

  inherit common;

  package = preSigning;  # XXX: this is slightly wrong, as not all files are in their final relative paths

  # XXX: Please, use ‘nix run -L .#packages.x86_64-windows.makeSignedInstaller.mainnet’,
  # as the process cannot be done purely, as it requires passing files
  # through `ssh` at the HSM server
  makeSignedInstaller = genClusters (cluster: makeInstaller { signed = true; inherit cluster; });

  unsignedInstaller = genClusters (cluster: pkgs.runCommand "win64-installer-${cluster}" {} ''
    ${makeInstaller { signed = false; inherit cluster; }}/bin/make-signed-installer
    mkdir $out
    cp -v installers/daedalus-*-*.exe $out/

    # Make it downloadable from Hydra:
    mkdir -p $out/nix-support
    echo "file binary-dist \"$(echo $out/*.exe)\"" >$out/nix-support/hydra-build-products
  '');

  # They’re initially the same as Linux when cross-compiling for Windows:
  node_modules = inputs.self.internal.x86_64-linux.node_modules;

  electron-cache = pkgs.runCommand "electron-cache" {} ''
    # newer style
    mkdir -p $out/${commonSources.electronCacheHash}/
    ln -sv ${windowsSources.electron} $out/${commonSources.electronCacheHash}/electron-v${electronVersion}-win32-x64.zip
    mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip
    ln -s ${windowsSources.electron} $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip/electron-v${electronVersion}-win32-x64.zip
  '';

  daedalusJs = genClusters (cluster: pkgs.stdenv.mkDerivation {
    name = "daedalus-js";
    src = srcWithoutNix;
    nativeBuildInputs = [ yarn nodejs wine64 ]
      ++ (with pkgs; [ python3 pkgconfig unzip ]);
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
    configurePhase = common.setupCacheAndGypDirs + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
      pkgs.lib.recursiveUpdate originalPackageJson {
        productName = common.launcherConfigs.${cluster}.installerConfig.spacedName;
      }
    ));
    buildPhase = ''
      # old style
      export ELECTRON_CACHE=${electron-cache}
      # new style
      mkdir -pv $HOME/.cache/
      ln -sv ${electron-cache} $HOME/.cache/electron

      cp $patchedPackageJson package.json

      rm -r installers/icons/
      cp -r ${windowsIcons.${cluster}} installers/icons
      chmod -R +w installers/icons

      # TODO: why are the following 2 lines needed?
      mkdir -p installers/icons/${cluster}/${cluster}
      cp ${windowsIcons.${cluster}}/${cluster}/* installers/icons/${cluster}/${cluster}/

      export DEBUG=electron-packager
      yarn --verbose --offline package --win64 --dir $(pwd) --icon installers/icons/${cluster}/${cluster}
    '';
    installPhase = ''
      set -x

      ls -ltrh release/win32-x64/Daedalus*-win32-x64/
      cp -r release/win32-x64/Daedalus*-win32-x64 $out

      # XXX: the webpack utils embed the original source paths into map files, which causes the derivation
      # to depend on the original inputs at the nix layer, and double the size of the linux installs.
      # this will just replace all storepaths with an invalid one:
      (
        cd $out/resources/app/dist
        for x in {main,renderer}/{0.,}index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
          ${pkgs.nukeReferences}/bin/nuke-refs $x
        done
      )

      rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}

      mkdir -pv $out/resources/app/node_modules
      cp -r node_modules/{\@babel,\@noble,\@protobufjs,regenerator-runtime,node-fetch,\@trezor,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib} $out/resources/app/node_modules

      chmod -R +w $out

      # XXX: remove redundant native modules, and point bindings.js to C:/Program\ Files/Daedalus/*.node instead:
      echo 'Deleting all redundant ‘*.node’ files under to-be-distributed ‘node_modules/’:'
      (
        cd $out/
        find resources/ -name '*.node' -exec rm -vf '{}' ';'
        find resources/app/node_modules -type f '(' -name '*.o' -o -name '*.o.d' -o -name '*.target.mk' -o -name '*.Makefile' -o -name 'Makefile' -o -name 'config.gypi' ')' -exec rm -vf '{}' ';'
        sed -r 's#try: \[#\0 [process.env.DAEDALUS_INSTALL_DIRECTORY, "bindings"],#' -i resources/app/node_modules/bindings/bindings.js
      )

      # TODO: build the distributed ones from source:
      (
        cd $(mktemp -d)
        cp ${nativeModules}/build/Debug/*.node $out/
      )
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  });

  fresherPkgs = import (pkgs.fetchFromGitHub {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "17a689596b72d1906883484838eb1aaf51ab8001"; # nixos-unstable on 2023-05-15T08:29:41Z
    hash = "sha256-YPLMeYE+UzxxP0qbkBzv3RBDvyGR5I4d7v2n8dI3+fY=";
  }) { inherit (pkgs) system; };

  msvc-wine = pkgs.stdenv.mkDerivation {
    name = "msvc-wine";
    src = pkgs.fetchFromGitHub {
      owner = "mstorsjo";
      repo = "msvc-wine";
      rev = "c4fd83d53689f30ae6cfd8e9ef1ea01712907b59";  # 2023-05-09T21:52:05Z
      hash = "sha256-hA11dIOIL9sta+rwGb2EwWrEkRm6nvczpGmLZtr3nHI=";
    };
    buildInputs = [
      (pkgs.python3.withPackages (ps: with ps; [ six ]))
    ];
    configurePhase = ":";
    buildPhase = ":";
    installPhase = ''
      sed -r 's,msiextract,${pkgs.msitools}/bin/\0,g' -i vsdownload.py
      mkdir -p $out/libexec
      cp -r . $out/libexec/.
    '';
  };

  msvc-cache = let
    version = "16";   # There doesn’t seem to be an easy way to specify a more stable full version, 16.11.26
  in pkgs.stdenv.mkDerivation {
    name = "msvc-cache-${version}";
    inherit version;
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = "sha256-7+vNhYbrizqhoIDL6vN7vE+Gq2duoYW5adMgOpJgw2w=";
    buildInputs = [];
    dontUnpack = true;
    dontConfigure = true;
    NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    buildPhase = ''
      mkdir -p $out
      ${msvc-wine}/libexec/vsdownload.py --accept-license --major ${version} \
        --save-manifest \
        --only-download --cache $out --dest ./
      cp -v *.manifest $out/.
    '';
    dontInstall = true;
  };

  msvc-installed = pkgs.stdenv.mkDerivation {
    name = "msvc-installed-${msvc-cache.version}";
    inherit (msvc-cache) version;
    dontUnpack = true;
    dontConfigure = true;
    buildPhase = ''
      mkdir -p $out
      ${msvc-wine}/libexec/vsdownload.py --accept-license --major ${msvc-cache.version} \
        --manifest ${msvc-cache}/*.manifest \
        --keep-unpack --cache ${msvc-cache} --dest $out/
      mv $out/unpack/MSBuild $out/
    '';
    dontInstall = true;
  };

  electronHeadersWithNodeLib = pkgs.runCommandLocal "electron-headers" {
    # XXX: don’t use fetchzip, we need the raw .tar.gz in `patchElectronRebuild` below
    inherit (commonSources.electronHeaders) src;
  } ''
    tar -xf $src
    mv node_headers $out
    echo 9 >$out/installVersion
    mkdir -p $out/Release
    ln -s ${windowsSources.node-lib} $out/Release/node.lib
  '';

  nativeModules = pkgs.stdenv.mkDerivation {
    name = "daedalus-native-modules";
    src = common.srcLockfiles;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with fresherPkgs; [ wineWowPackages.stableFull fontconfig winetricks samba /* samba for bin/ntlm_auth */ ])
      ++ (with pkgs; [ python3 pkgconfig jq file procps ]);
    buildInputs = with pkgs; [ libusb ];
    configurePhase = common.setupCacheAndGypDirs + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    FONTCONFIG_FILE = fresherPkgs.makeFontsCache {
      fontDirectories = with fresherPkgs; [
        dejavu_fonts freefont_ttf gyre-fonts liberation_ttf noto-fonts-emoji
        unifont winePackages.fonts xorg.fontcursormisc xorg.fontmiscmisc
      ];
    };
    buildPhase = let
      mkSection = title: ''
        echo ' '
        echo ' '
        echo ' '
        echo ' '
        echo ' '
        echo '===================== '${pkgs.lib.escapeShellArg title}' ====================='
      '';
      completeHack = "rebuild-complete-hack-bnlzMmdjbXB5emozNWFndGx1bnd5dnh5";
    in ''
      ${pkgs.xvfb-run}/bin/xvfb-run \
        --server-args="-screen 0 1920x1080x24 +extension GLX +extension RENDER -ac -noreset" \
        ${pkgs.writeShellScript "wine-setup-inside-xvfb" ''
          set -euo pipefail

          ${mkSection "Setting Windows system version"}
          winetricks -q win81

          ${mkSection "Setting up env and symlinks in standard locations"}

          # Symlink Windows SDK in a standard location:
          lx_program_files="$HOME/.wine/drive_c/Program Files (x86)"
          mkdir -p "$lx_program_files"
          ln -svf ${msvc-installed}/kits "$lx_program_files/Windows Kits"

          # Symlink VC in a standard location:
          vc_versionYear="$(jq -r .info.productLineVersion <${msvc-cache}/*.manifest)"
          lx_VSINSTALLDIR="$lx_program_files/Microsoft Visual Studio/$vc_versionYear/Community"
          mkdir -p "$lx_VSINSTALLDIR"
          ln -svf ${msvc-installed}/VC "$lx_VSINSTALLDIR"/
          ln -svf ${msvc-installed}/MSBuild "$lx_VSINSTALLDIR"/

          export VCINSTALLDIR="$(winepath -w "$lx_VSINSTALLDIR/VC")\\"
          export VCToolsVersion="$(ls ${msvc-installed}/VC/Tools/MSVC | head -n1)"
          export VCToolsInstallDir="$(winepath -w "$lx_VSINSTALLDIR/VC/Tools/MSVC/$VCToolsVersion")\\"
          export VCToolsRedistDir="$(winepath -w "$lx_VSINSTALLDIR/VC/Redist/MSVC/$VCToolsVersion")\\"

          export ClearDevCommandPromptEnvVars=false

          export VSINSTALLDIR="$(winepath -w "$lx_VSINSTALLDIR")\\"

          lx_WindowsSdkDir=("$lx_program_files/Windows Kits"/*)
          export WindowsSdkDir="$(winepath -w "$lx_WindowsSdkDir")\\"

          set -x

          # XXX: this can break, as `v10.0` is not determined programmatically;
          # XXX: the path is taken from `${msvc-installed}/MSBuild/Microsoft/VC/v160/Microsoft.Cpp.WindowsSDK.props`
          wine reg ADD 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SDKs\Windows\v10.0' \
            /v 'InstallationFolder' /t 'REG_SZ' /d "$WindowsSdkDir" /f

          # XXX: This path is taken from `${msvc-installed}/unpack/Common7/Tools/vsdevcmd/core/winsdk.bat`
          wine reg ADD 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows Kits\Installed Roots' \
            /v 'KitsRoot10' /t 'REG_SZ' /d "$WindowsSdkDir" /f

          set +x

          ${mkSection "Preparing the ‘info’ structure"}
          jq --null-input \
            --arg msBuild      "$(winepath -w "$lx_VSINSTALLDIR/MSBuild/Current/Bin/MSBuild.exe")" \
            --arg path         "$VCINSTALLDIR" \
            --arg sdk          "$(ls ${msvc-installed}/kits/10/Include | head -n1)" \
            --arg toolset      "$(ls "$lx_VSINSTALLDIR/VC/Redist/MSVC" | grep -E '^v[0-9]+$')" \
            --arg version      "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest)" \
            --arg versionMajor "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest | cut -d. -f1)" \
            --arg versionMinor "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest | cut -d. -f2)" \
            --arg versionYear  "$(jq -r .info.productLineVersion    <${msvc-cache}/*.manifest)" \
            '{$msBuild,$path,$sdk,$toolset,$version,$versionMajor,$versionMinor,$versionYear}' \
            > vs-info.json

          ${mkSection "Stubbing node_modules/node-gyp/lib/find-visualstudio.js"}
          (
            cat <<<${pkgs.lib.escapeShellArg ''
              'use strict'
              function findVisualStudio (nodeSemver, configMsvsVersion, callback) {
                process.nextTick(() => callback(null,
            ''}
            cat vs-info.json
            cat <<<${pkgs.lib.escapeShellArg ''
                ));
              }
              module.exports = findVisualStudio
            ''}
          ) >node_modules/node-gyp/lib/find-visualstudio.js
          cat node_modules/node-gyp/lib/find-visualstudio.js

          ${mkSection "Setting WINEPATH"}
          export WINEPATH="$(winepath -w ${native.nodejs});$(winepath -w ${native.python})"

          ${mkSection "Removing all symlinks to /nix/store (mostly python3)"}
          find node_modules -type l >all-symlinks.lst
          paste all-symlinks.lst <(xargs <all-symlinks.lst readlink) | grep -F /nix/store | cut -f1 | xargs rm -v
          rm all-symlinks.lst

          ${mkSection "Patching node_modules"}
          # Point electron-rebuild to the correct Node (Electron) headers location:
          ${common.patchElectronRebuild}/bin/* \
            "$(winepath -w ${electronHeadersWithNodeLib.src} | sed -r 's,\\,\\\\\\\\,g')" \
            "$(winepath -w ${electronHeadersWithNodeLib}     | sed -r 's,\\,\\\\\\\\,g')"

          ${mkSection "Running @electron/rebuild"}
          # XXX: we need to run the command with the Node.js env set correcty, `npm.cmd` does that:
          lx_electron_rebuild_bin="$(readlink -f node_modules/.bin/electron-rebuild)"
          export electron_rebuild_bin="$(winepath -w "$lx_electron_rebuild_bin")"

          # XXX: for some reason the build hangs (only on Cicero!) after outputting "Rebuild Complete", so let's hack around that:
          sed -r '/Rebuild Complete/a require("fs").writeFileSync("${completeHack}", "");' -i "$lx_electron_rebuild_bin"

          # XXX: re-enable this if you need to simulate Cicero hanging locally:
          # sed -r 's/rebuildSpinner.succeed\(\);/setTimeout(function(){rebuildSpinner.succeed();},10000);/g' -i "$lx_electron_rebuild_bin"

          (
            while true ; do
              if [ -e ${completeHack} ] ; then
                echo "Found ${completeHack}, killing node.exe among:"

                ps aux | cat

                pkill -9 node.exe || true
                break
              else
                sleep 2
              fi
            done
          ) &
          wine_killer_pid=$!

          cp ${pkgs.writeText "package.json" (builtins.toJSON (
            pkgs.lib.recursiveUpdate originalPackageJson {
              scripts = {
                "build:electron:windows" = "node.exe %electron_rebuild_bin% -f -w usb";
              };
            }
          ))} package.json
          wine npm.cmd run build:electron:windows || {
            real_ec=$?
            if [ -e ${completeHack} ] ; then
              echo "Wine would return $real_ec, but ${completeHack} exists"
              return 0
            else
              return $real_ec
            fi
          }
          kill $wine_killer_pid || true

          # XXX: We’re running in a separate namespace, so this is fine.
          while pgrep wine >/dev/null ; do
            ${mkSection "Wine is still running in the background, will try to kill it"}
            echo 'All remaining processes:'
            ps aux | cat

            sleep 1
            pkill -9 wine || true
            sleep 4
          done
        ''}
    '';
    installPhase = ''
      #find -iname '*.node' | xargs file -L

      mkdir -p $out/build/Release
      cp node_modules/usb-detection/build/Release/detection.node $out/build/Release/
      cp node_modules/usb/build/Release/usb_bindings.node        $out/build/Release/
      cp node_modules/node-hid/build/Release/HID.node            $out/build/Release/

      # make sure they’re for Windows
      find $out -iname '*.node' | while IFS= read -r ext ; do
        file "$ext" | grep -F 'MS Windows' || {
          echo "fatal: $ext is not built for MS Windows (shouldn’t happen)"
          exit 2
        }
      done

      # Is that needed?
      cp -r $out/build/Release $out/build/Debug
    '';
  };

  native = rec {
    nodejs = pkgs.fetchzip {
      url = "https://nodejs.org/dist/v${common.nodejs.version}/node-v${common.nodejs.version}-win-x64.zip";
      hash = "sha256-n8ux67xrq3Rta1nE715y1m040oaLxUI2bIt12RaJdeM=";
    };

    python = pkgs.fetchzip {
      url = "https://www.python.org/ftp/python/3.10.11/python-3.10.11-embed-amd64.zip";
      hash = "sha256-p83yidrRg5Rz1vQpyRuZCb5F+s3ddgHt+JakPjgFgUc=";
      stripRoot = false;
    };
  };

  windowsIcons = genClusters (cluster: let
    buildInputs = with pkgs; [ imagemagick ];
    # Allow fallback to `mainnet` if cluster’s icons don’t exist:
    srcCluster = if builtins.pathExists (../../installers/icons + "/${cluster}") then cluster else "mainnet";
  in pkgs.runCommand "windows-icons-${cluster}" { inherit buildInputs; } ''
    mkdir -p $out/${cluster} $out
    cp -r ${../../installers/icons + "/${srcCluster}"}/. $out/${cluster}/.
    cp ${../../installers/icons/installBanner.bmp} $out/installBanner.bmp
    cd $out/${cluster}
    rm *.ico *.ICO || true   # XXX: just in case
    for f in *.png ; do
      # XXX: these sizes are too large for the ICO format:
      if [ "$f" == 1024x1024.png ] || [ "$f" == 512x512.png ] ; then continue ; fi
      convert "$f" "''${f%.png}.ico"
    done
    convert 16x16.png 24x24.png 32x32.png 48x48.png 64x64.png 128x128.png 256x256.png ${cluster}.ico
  '');

  nsisFiles = genClusters (cluster: pkgs.runCommand "nsis-files" {
    buildInputs = [ common.daedalus-installer pkgs.glibcLocales ];
  } ''
    mkdir installers
    cp -vir ${../../package.json} package.json
    cd installers

    export LANG=en_US.UTF-8
    cp -v ${common.launcherConfigs.${cluster}.configFiles}/* .
    make-installer --cardano dummy \
      --os win64 \
      -o $out \
      --cluster ${cluster} \
      --build-rev-short ${sourceLib.buildRevShort} \
      --build-counter ${toString sourceLib.buildCounter} \
      buildkite-cross

    mkdir $out
    cp -v daedalus.nsi uninstaller.nsi $out/
    cp -v ${common.launcherConfigs.${cluster}.configFiles}/* $out/
    ls -lR $out
  '');

  # the native makensis binary, with cross-compiled windows stubs
  nsis = let
    # TODO, nsis can't cross-compile with the nixpkgs daedalus currently uses
    nsisNixpkgs = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "nixpkgs";
      rev = "be445a9074f139d63e704fa82610d25456562c3d";
      hash = "sha256-ivcmGg01aeeod0rzjMJ86exUNHHRJu4526rGq9s7rJU=";
    };
    nsisPkgs = import nsisNixpkgs { system = "x86_64-linux"; };
  in nsisPkgs.callPackage ./nsis.nix {};

  wine = pkgs.wine.override { wineBuild = "wine32"; };
  wine64 = pkgs.wine.override { wineBuild = "wineWow"; };

  unsignedUninstaller = genClusters (cluster: pkgs.runCommand "uninstaller" { buildInputs = [ nsis wine ]; } ''
    mkdir home
    export HOME=$(realpath home)

    ln -sv ${../../installers/nsis_plugins} nsis_plugins
    cp ${nsisFiles.${cluster}}/uninstaller.nsi .

    makensis uninstaller.nsi -V4

    wine tempinstaller.exe /S
    mkdir $out
    mv -v $HOME/.wine/drive_c/uninstall.exe $out/uninstall.exe
  '');

  # a cross-compiled fastlist for the ps-list package
  fastlist = pkgs.pkgsCross.mingwW64.callPackage ./fastlist.nix {};

  preSigning = genClusters (cluster: let
    installDir = common.launcherConfigs.${cluster}.installerConfig.spacedName;
  in pkgs.runCommand "pre-signing" { buildInputs = [ pkgs.unzip ]; } ''
    mkdir $out
    cd $out

    echo '~~~   Preparing files for installer'
    mkdir installers
    cp -vir ${windowsIcons.${cluster}} installers/icons
    cp -vir ${../../package.json} package.json
    chmod -R +w installers
    cd installers
    mkdir -pv ../release/win32-x64/
    cp -rv ${daedalusJs.${cluster}} "../release/win32-x64/${installDir}-win32-x64"
    chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
    cp -v ${fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
    ln -s ${../../installers/nsis_plugins} nsis_plugins

    cp -vr ${common.daedalus-bridge.${cluster}}/bin/* .
    cp -v ${nsisFiles.${cluster}}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
    cp ${unsignedUninstaller.${cluster}}/uninstall.exe .
    if [ -f ${nsisFiles.${cluster}}/block-0.bin ]; then
      cp -v ${nsisFiles.${cluster}}/block-0.bin .
    fi
  '');

  makeInstaller = { signed ? false, cluster }: pkgs.writeShellScriptBin "make-signed-installer" ''
    set -euo pipefail

    ${if signed then ''
      # We have to do it impurely:
      cd $(mktemp -d)
      echo "~~~ We’re signing in $PWD:"

      sign_cmd() {
        echo "Signing: ‘$1’…"
        ssh HSM <"$1" >"$1".signed
        mv "$1".signed "$1"
      }
    '' else ''
      sign_cmd() {
        echo "Would sign: ‘$1’"
      }
    ''}

    cp -r ${preSigning.${cluster}}/. ./
    chmod -R +w .

    find . '(' -iname '*.exe' -o -iname '*.dll' -o -iname '*.node' ')' | sort | while IFS= read -r binaryToSign ; do
      sign_cmd "$binaryToSign"
    done

    echo '~~~ Generating installer'
    (
      cd installers/
      ${nsis}/bin/makensis daedalus.nsi -V4
    )

    sign_cmd installers/daedalus-*-*.exe

    echo "Final installer: ‘$(realpath installers/daedalus-*-*.exe)’"
  '';

  windowsSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-win32-x64.zip";
      hash = "sha256-xYQml960qP3sB/Rp3uEMU7s9aT2Ma4A5VHHzuUx8r9c=";
    };

    # XXX: normally, node-gyp would download it only for Windows,
    # XXX: see `resolveLibUrl()` in `node-gyp/lib/process-release.js`
    node-lib = pkgs.fetchurl {
      name = "node.lib-${electronVersion}"; # cache invalidation
      url = "https://electronjs.org/headers/v${electronVersion}/win-x64/node.lib";
      hash = "sha256-JhIFzgm+Oig7FsHk1TP85H6PDD3drC7wXpVDfq8hIC4=";
    };
  };

}
