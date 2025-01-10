{ inputs, targetSystem }:

assert targetSystem == "x86_64-darwin" || targetSystem == "aarch64-darwin";

let

  common = import ./common.nix { inherit inputs targetSystem; };

  inherit (common) sourceLib pkgs;
  inherit (pkgs) lib;

  inherit (common)
    launcherConfigs mock-token-metadata-server
    cardanoNodeVersion cardanoWalletVersion;

  inherit (common) originalPackageJson electronVersion electronChromedriverVersion commonSources;

  archSuffix = if pkgs.system == "aarch64-darwin" then "arm64" else "x64";
  packageVersion = originalPackageJson.version;
  installerName = cluster: "daedalus-${packageVersion}-${toString sourceLib.buildCounter}-${cluster}-${sourceLib.buildRevShort}-${pkgs.system}";

  genClusters = lib.genAttrs sourceLib.installerClusters;

in rec {

  inherit common;
  inherit (common) nodejs yarn yarn2nix offlineCache srcLockfiles srcWithoutNix;

  # The following is used in all `configurePhase`s:
  darwinSpecificCaches = let
    cacheDir = "$HOME/Library/Caches";
  in ''
    mkdir -p ${cacheDir}/electron/${commonSources.electronCacheHash}/
    ln -sf ${commonSources.electronShaSums} ${cacheDir}/electron/${commonSources.electronCacheHash}/SHASUMS256.txt
    ln -sf ${darwinSources.electron} ${cacheDir}/electron/${commonSources.electronCacheHash}/electron-v${electronVersion}-darwin-${archSuffix}.zip

    mkdir -p ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/
    ln -sf ${commonSources.electronChromedriverShaSums} ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/SHASUMS256.txt
    ln -sf ${darwinSources.electronChromedriver} ${cacheDir}/electron/${commonSources.electronChromedriverCacheHash}/chromedriver-v${electronChromedriverVersion}-darwin-${archSuffix}.zip
  '';

  # XXX: we don't use `autoSignDarwinBinariesHook` for ad-hoc signing,
  # because it takes too long (minutes) for all the JS/whatnot files we
  # have. Instead, we locate targets in a more clever way.
  signAllBinaries = pkgs.writeShellScript "signAllBinaries" ''
    set -o nounset
    echo 'Searching for binaries to ad-hoc sign…'
    source ${pkgs.darwin.signingUtils}
    ${pkgs.findutils}/bin/find "$1" -type f -not '(' -name '*.js' -o -name '*.ts' -o -name '*.ts.map' -o -name '*.js.map' -o -name '*.json' ')' -exec ${pkgs.file}/bin/file '{}' ';' | grep -F ': Mach-O' | cut -d: -f1 | while IFS= read -r target ; do
      echo "ad-hoc signing ‘$target’…"
      signIfRequired "$target"
    done
  '';

  # XXX: Whenever changing `yarn.lock`, make sure this still builds
  # without network. I.e. since there is no network sanbox in Nix on
  # Darwin, you have to first build `package` with network (to populate
  # /nix/store with pure dependencies), then add a newline in the middle
  # of `package.json`, and then build the `package` again, only this time
  # with network turned off system-wise.
  node_modules = pkgs.stdenv.mkDerivation {
    name = "daedalus-node_modules";
    src = srcLockfiles;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with pkgs; [ python3 perl pkgconfig jq darwin.cctools xcbuild ]);
    buildInputs = (with pkgs.darwin; [
      apple_sdk.frameworks.CoreServices
      apple_sdk.frameworks.AppKit
    ]);
    configurePhase = common.setupCacheAndGypDirs + darwinSpecificCaches;
    buildPhase = ''
      # Do not look up in the registry, but in the offline cache:
      ${yarn2nix.fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock

      # Now, install from ${offlineCache} to node_modules/
      yarn install --ignore-scripts

      # Remove all prebuilt *.node files extracted from `.tgz`s
      find . -type f -name '*.node' -not -path '*/@swc*/*' -exec rm -vf {} ';'

      patchShebangs . >/dev/null  # a real lot of paths to patch, no need to litter logs

      ${builtins.path { path = inputs.self + "/scripts/darwin-no-x-compile.sh"; }}

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

      ${signAllBinaries} $out
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

  darwin-launcher = pkgs.callPackage ./darwin-launcher.nix {};

  # TODO: don’t use cardano-bridge.nix

  # TODO: compare runtime-nodejs-deps.json with Linux and Windows, and re-use it there

  nix-bundle-exe-same-dir = pkgs.runCommand "nix-bundle-exe-same-dir" {} ''
    cp -R ${inputs.nix-bundle-exe} $out
    chmod -R +w $out
    sed -r 's+@executable_path/\$relative_bin_to_lib/\$lib_dir+@executable_path+g' -i $out/bundle-macos.sh
  '';

  mkBundle = exes: let
    unbundled = pkgs.linkFarm "exes" (lib.mapAttrsToList (name: path: {
      name = "bin/" + name;
      inherit path;
    }) exes);
  in (import nix-bundle-exe-same-dir {
    inherit pkgs;
    bin_dir = "bundle";
    exe_dir = "_unused_";
    lib_dir = "bundle";
  } unbundled).overrideAttrs (drv: {
      buildCommand = (
        builtins.replaceStrings
          ["'${unbundled}/bin'"]
          ["'${unbundled}/bin' -follow"]
          drv.buildCommand
      ) + ''
        mv $out/bundle/* $out/
        rmdir $out/bundle
      '';
  });

  # XXX: cardano-launcher cannot be a symlink, because it looks at its own
  # `realpath` to determine DAEDALUS_INSTALL_DIRECTORY:
  bundle-cardano-launcher = (mkBundle {
    "cardano-launcher" = common.cardano-shell.haskellPackages.cardano-launcher.components.exes.cardano-launcher + "/bin/cardano-launcher";
  }).overrideAttrs (drv: {
    buildCommand = let exeName = "cardano-launcher"; in drv.buildCommand + ''
      (
        cd $out
        mkdir -p bundle-${exeName}
        mv *.dylib bundle-${exeName}/
        otool -L ${exeName} \
          | grep -E '^\s*@executable_path' \
          | sed -r 's/^\s*//g ; s/ \(.*//g' \
          | while IFS= read -r lib ; do
          install_name_tool -change "$lib" "$(sed <<<"$lib" -r 's,@executable_path/,@executable_path/bundle-${exeName}/,g')" ${exeName}
        done
      )
    '';
  });

  bundle-cardano-node     = mkBundle { "cardano-node"     = lib.getExe common.cardano-node; };
  bundle-cardano-cli      = mkBundle { "cardano-cli"      = lib.getExe common.cardano-cli; };
  bundle-cardano-address  = mkBundle { "cardano-address"  = lib.getExe common.cardano-address; };
  bundle-cardano-wallet   = pkgs.runCommandNoCC "bundle-cardano-wallet" {} ''cp -r ${common.cardano-wallet}/bin $out'';  # upstream bundles it
  bundle-mock-token-metadata-server = mkBundle { "mock-token-metadata-server"  = lib.getExe common.mock-token-metadata-server; };
  bundle-local-cluster              = mkBundle { "local-cluster"               = lib.getExe common.walletPackages.local-cluster; };

  # HID.node and others depend on `/nix/store`, we have to bundle them, too:
  bundleNodeJsNativeModule = pkgs.writeShellScript "bundleNodeJsNativeModule" ''
    #!/usr/bin/env bash
    set -euo pipefail
    target="$1"
    export bin_dir="bundle"
    export exe_dir="_unused_"
    export lib_dir="bundle"
    export PATH=${lib.makeBinPath (with pkgs; [ darwin.cctools darwin.binutils darwin.sigtool nukeReferences ])}:"$PATH"
    tmpdir=$(mktemp -d)
    cp ${nix-bundle-exe-same-dir}/bundle-macos.sh "$tmpdir"/
    chmod -R +w "$tmpdir"
    sed -r 's/@executable_path/@loader_path/g' -i "$tmpdir"/bundle-macos.sh
    bash "$tmpdir"/bundle-macos.sh "$tmpdir" "$target"
    rm "$tmpdir"/bundle-macos.sh
    mv "$tmpdir/bundle" "$(dirname "$target")/bundle-$(basename "$target")"
    rmdir "$tmpdir"
    rm "$target"
    ln -s "bundle-$(basename "$target")/$(basename "$target")" "$target"
  '';

  package = genClusters (cluster: let
    pname = "daedalus";
  in pkgs.stdenv.mkDerivation {
    name = pname;
    src = srcWithoutNix;
    nativeBuildInputs = [ yarn nodejs ]
      ++ (with pkgs; [ python3 perl pkgconfig darwin.cctools xcbuild jq ]);
    buildInputs = (with pkgs.darwin; [
      apple_sdk.frameworks.CoreServices
      apple_sdk.frameworks.AppKit
      libobjc
    ]) ++ [
      darwin-launcher
      mock-token-metadata-server
    ];
    NETWORK = cluster;
    BUILD_REV = sourceLib.buildRev;
    BUILD_REV_SHORT = sourceLib.buildRevShort;
    BUILD_COUNTER = sourceLib.buildCounter;
    CARDANO_WALLET_VERSION = cardanoWalletVersion;
    CARDANO_NODE_VERSION = cardanoNodeVersion;
    configurePhase = common.setupCacheAndGypDirs + darwinSpecificCaches + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    outputs = [ "out" "futureInstaller" ];
    buildPhase = ''
      patchShebangs .
      sed -r 's#.*patchElectronRebuild.*#${common.patchElectronRebuild}/bin/*#' -i scripts/rebuild-native-modules.sh

      ${common.temporaryNodeModulesPatches}

      (
        cd installers/
        cp -r ${launcherConfigs.${cluster}.configFiles}/. ./.

        echo "Creating icons ..."
        /usr/bin/iconutil --convert icns --output icons/electron.icns "icons/${cluster}.iconset"
      )

      mkdir -p release
      echo "Installing nodejs dependencies..."
      echo "Running electron packager script..."
      export "NODE_ENV" "production"
      yarn build:electron
      yarn run package -- --name ${lib.escapeShellArg common.launcherConfigs.${cluster}.installerConfig.spacedName}
      echo "Size of Electron app is $(du -sh release)"
      find -name '*.node'

      pathtoapp=release/darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}-darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}.app
      mkdir -p "$pathtoapp"/Contents/Resources/app/node_modules
      jq -r '.[]' ${./runtime-nodejs-deps.json} | sed -r 's,^,node_modules/,' | xargs -d '\n' cp -r -t "$pathtoapp"/Contents/Resources/app/node_modules/

      mkdir -p "$pathtoapp"/Contents/Resources/app/build

      for f in \
        "usb/build/Release/usb_bindings.node" \
        "node-hid/build/Release/HID.node" \
        "usb-detection/build/Release/detection.node" \
        ; do
        cp node_modules/"$f" "$pathtoapp"/Contents/Resources/app/build/
      done

      jq --arg name "xxx" '.productName = $name' "$pathtoapp/Contents/Resources/app/package.json" >tmp-package.json
      mv tmp-package.json "$pathtoapp/Contents/Resources/app/package.json"

      dir="$pathtoapp/Contents/MacOS"
      dataDir="$pathtoapp/Contents/Resources"

      mkdir -p "$dir" "$dataDir"

      echo "Preparing files ..."
      cp installers/launcher-config.yaml "$dataDir"/

      cp -r ${bundle-cardano-launcher}/. "$dir"/

      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (exe: bundle: ''
        cp -r ${bundle} "$dir"/bundle-${exe}
        ln -s bundle-${exe}/${exe} "$dir"/${exe}
      '') ({
        "cardano-node" = bundle-cardano-node;
        "cardano-cli" = bundle-cardano-cli;
        "cardano-address" = bundle-cardano-address;
        "cardano-wallet" = bundle-cardano-wallet;
      } // (lib.optionalAttrs (cluster == "selfnode") {
        "mock-token-metadata-server" = bundle-mock-token-metadata-server;
        "local-cluster" = bundle-local-cluster;
      })))}

      cp installers/{config.yaml,genesis.json,topology.yaml} "$dataDir"/
      ${if (cluster != "selfnode") then ''
        cp installers/{genesis-byron.json,genesis-shelley.json,genesis-alonzo.json} "$dataDir"/
        cp installers/genesis-conway.json "$dataDir"/ || true
      '' else ''
        cp installers/{signing.key,delegation.cert} "$dataDir"/
        cp -f ${./../../utils/cardano/selfnode}/token-metadata.json "$dir"/
      ''}

      chmod -R +w "$dir"
      rm -r "$dataDir/app/installers"

      for f in "usb_bindings.node" "detection.node" "HID.node" ; do
        (
          cd "$dataDir"/app/build/
          mv "$f" ../../../MacOS/"$f"
          # TODO: why is/was this "reverse" symlink needed? does it make sense?
          ln -s   ../../../MacOS/"$f" ./
        )
        ${bundleNodeJsNativeModule} "$dir/$f"
      done

      # TODO: why is/was this "reverse" symlink needed? does it make sense?
      (
        cd "$dataDir"/app/node_modules/usb-detection/build/Release/
        rm detection.node
        ln -sfn ../../../../../../MacOS/detection.node
      )

      mv "$dir"/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName} "$dir"/Frontend
      chmod +x "$dir"/Frontend

      cat ${pkgs.writeText "helper" ''
        #!/usr/bin/env bash
        mkdir -p "${launcherConfigs.${cluster}.installerConfig.dataDir}/Secrets-1.0"
        mkdir -p "${launcherConfigs.${cluster}.installerConfig.dataDir}/Logs/pub"
      ''} >"$dataDir"/helper
      chmod +x "$dataDir"/helper

      cp ${darwin-launcher}/bin/darwin-launcher "$dir"/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}
    '';
    installPhase = ''
      mkdir -p $out/Applications/
      cp -r release/darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}-darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}.app $out/Applications/

      # XXX: remove redundant native modules, and point bindings.js to Contents/MacOS/*.node instead:
      echo 'Deleting all redundant ‘*.node’ files under to-be-distributed ‘node_modules/’:'
      (
        cd $out/Applications/*/Contents/
        find Resources/ -name '*.node' -exec rm -vf '{}' ';'
        find Resources/app/node_modules -type f '(' -name '*.o' -o -name '*.o.d' -o -name '*.target.mk' -o -name '*.Makefile' -o -name 'Makefile' -o -name 'config.gypi' ')' -exec rm -vf '{}' ';'
        sed -r 's#try: \[#\0 [process.env.DAEDALUS_INSTALL_DIRECTORY, "bindings"],#' -i Resources/app/node_modules/bindings/bindings.js
      )

      # XXX: For `nix run`, unfortunately, we cannot use symlinks, because then golang’s `os.Executable()`
      # will not return the target, but the symlink, and all paths will break… :sigh:
      mkdir -p $out/bin/
      cat >$out/bin/${pname} << EOF
      #!/bin/sh
      exec $out/Applications/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}.app/Contents/MacOS/${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}
      EOF
      chmod +x $out/bin/${pname}

      # Further signing for notarization (cannot be done purely):
      mkdir -p $futureInstaller/
      chmod +x installers/codesign.sh
      cp installers/{codesign.sh,entitlements.xml} $futureInstaller/

      ${signAllBinaries} $out
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  });

  unsignedInstaller = genClusters (cluster: pkgs.stdenv.mkDerivation {
    name = "daedalus-unsigned-darwin-installer";
    dontUnpack = true;
    buildPhase = ''
      ${makeSignedInstaller.${cluster}}/bin/* | tee make-installer.log
    '';
    installPhase = ''
      mkdir -p $out
      cp $(tail -n 1 make-installer.log) $out/

      # Make it downloadable from Hydra:
      mkdir -p $out/nix-support
      echo "file binary-dist \"$(echo $out/*.pkg)\"" >$out/nix-support/hydra-build-products
    '';
  });

  makeSignedInstaller = genClusters (cluster: pkgs.writeShellScriptBin "make-signed-installer" (let

    # FIXME: in the future this has to be done better, now let’s reuse the Buildkite legacy:
    credentials = "/var/lib/buildkite-agent/signing.sh";
    codeSigningConfig = "/var/lib/buildkite-agent/code-signing-config.json";
    signingConfig = "/var/lib/buildkite-agent/signing-config.json";
    shallSignPredicate = "[ -f ${credentials} ] && [ -f ${codeSigningConfig} ] && [ -f ${signingConfig} ]";
    bashSetup = ''
      set -o errexit
      set -o pipefail
      set -o nounset
      export PATH="${lib.makeBinPath [ pkgs.coreutils pkgs.jq ]}:$PATH"
    '';
    readConfigs = pkgs.writeShellScript "read-signing-config" ''
      ${bashSetup}
      if ${shallSignPredicate} ; then
        echo "codeSigningIdentity='$(jq -r .codeSigningIdentity ${codeSigningConfig})'"
        echo "codeSigningKeyChain='$(jq -r .codeSigningKeyChain ${codeSigningConfig})'"
        echo "signingIdentity='$(jq -r .signingIdentity ${signingConfig})'"
        echo "signingKeyChain='$(jq -r .signingKeyChain ${signingConfig})'"
      else
        echo >&2 "Warning: code signing configs not found, installer will be unsigned, check" ${lib.escapeShellArg shallSignPredicate}
      fi
    '';

    packAndSign = pkgs.writeShellScript "pack-and-sign" ''
      ${bashSetup}

      eval $(${readConfigs})

      workDir=$(mktemp -d)
      appName=${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName}.app
      appDir=${package.${cluster}}/Applications/"$appName"

      echo "Info: workDir = $workDir"
      cd "$workDir"

      echo "Copying…"
      cp -r "$appDir" ./.
      chmod -R +w .

      if ${shallSignPredicate} ; then
        echo "Signing code…"
        ${package.${cluster}.futureInstaller}/codesign.sh "$codeSigningIdentity" "$codeSigningKeyChain" \
          "$appName" ${package.${cluster}.futureInstaller}/entitlements.xml
      fi

      echo "Making installer…"
      /usr/bin/pkgbuild \
        --identifier ${lib.escapeShellArg ("org." + launcherConfigs.${cluster}.installerConfig.macPackageName + ".pkg")} \
        --component "$workDir/$appName" \
        --install-location /Applications \
        ${lib.escapeShellArg ((installerName cluster) + ".phase1.pkg")}
      rm -r "$workDir/$appName"
      /usr/bin/productbuild --product ${../../installers/data/plist} \
        --package *.phase1.pkg \
        ${lib.escapeShellArg ((installerName cluster) + ".phase2.pkg")}
      rm *.phase1.pkg

      if ${shallSignPredicate} ; then
        echo "Signing installer…"

        # FIXME: this doesn’t work outside of `buildkite-agent`, it seems:
        #(
        #  source ${credentials}
        #  security unlock-keychain -p "$SIGNING" "$signingKeyChain"
        #)

        productsign --sign "$signingIdentity" --keychain "$signingKeyChain" \
          *.phase2.pkg \
          ${lib.escapeShellArg ((installerName cluster) + ".pkg")}
        rm *.phase2.pkg
      else
        mv *.phase2.pkg ${lib.escapeShellArg ((installerName cluster) + ".pkg")}
      fi

      echo "Done, you can submit it for notarization now:"
      echo "$workDir"/${lib.escapeShellArg ((installerName cluster) + ".pkg")}
    '';

  in ''
    cd /
    ${bashSetup}
    if ${shallSignPredicate} && [ "$USER" == "root" ]; then
      exec sudo -u buildkite-agent ${packAndSign}
    else
      exec ${packAndSign}
    fi
  ''));

  darwinSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-darwin-${archSuffix}.zip";
      hash =
        if archSuffix == "x64"
        then "sha256-I/d/vecsrYMV59Nw2SnNzrVAj1UzSUJB/F3VA9itDNw="
        else "sha256-Up0HRemSeMZvYxyB7b7yKlrYhxMyNmAC7dNxtAmFCyQ=";
    };

    electronChromedriver = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/chromedriver-v${electronChromedriverVersion}-darwin-${archSuffix}.zip";
      hash =
        if archSuffix == "x64"
        then "sha256-avLZdXPkcZx5SirO2RVjxXN2oRfbUHs5gEymUwne3HI="
        else "sha256-jehYm1nMlHjQha7AFzMUvKZxfSbedghODhBUXk1qKB4=";
    };
  };
}
