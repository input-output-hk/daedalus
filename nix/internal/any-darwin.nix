{
  inputs,
  targetSystem,
}:
assert targetSystem == "x86_64-darwin" || targetSystem == "aarch64-darwin"; let
  common = import ./common.nix {inherit inputs targetSystem;};

  inherit (common) sourceLib pkgs;
  inherit (pkgs) lib;

  inherit
    (common)
    launcherConfigs
    mock-token-metadata-server
    cardanoNodeVersion
    cardanoWalletVersion
    ;

  inherit (common) originalPackageJson electronVersion electronChromedriverVersion commonSources;

  archSuffix =
    if pkgs.stdenv.hostPlatform.system == "aarch64-darwin"
    then "arm64"
    else "x64";
  packageVersion = originalPackageJson.version;
  installerName = cluster: "daedalus-${packageVersion}-${toString sourceLib.buildCounter}-${cluster}-${sourceLib.buildRevShort}-${pkgs.stdenv.hostPlatform.system}";

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
      echo "ad-hoc signing '$target'…"
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
    nativeBuildInputs =
      [yarn nodejs]
      ++ (with pkgs; [perl pkg-config jq])
      ++ (with pkgs; [darwin.cctools xcbuild python3]);
    buildInputs = [];
    # Disable strict enum checking for cross-compilation compatibility
    NIX_CFLAGS_COMPILE = "-Wno-enum-constexpr-conversion";
    configurePhase = common.setupCacheAndGypDirs + darwinSpecificCaches;
    buildPhase = ''
            # Do not look up in the registry, but in the offline cache:
            ${yarn2nix.fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock

            # Now, install from ${offlineCache} to node_modules/
            yarn install --ignore-scripts

            # Remove prebuilt *.node files that need Electron-ABI-specific compilation.
            # Preserve N-API prebuilt binaries bundled inside npm tarballs — they are
            # ABI-stable and work across all Node.js/Electron versions without recompiling:
            #   node-hid:   prebuilds/HID-darwin-{x64,arm64}/node-napi-v4.node  (N-API v4)
            #   usb:        prebuilds/darwin-x64+arm64/node.napi.node            (N-API v8, universal fat binary)
            #   blake-hash: prebuilds/darwin-x64/node.napi.node                  (N-API, x64 only; arm64 compiled below)
            find . -type f -name '*.node' \
              -not -path '*/@swc*/*' \
              -not -path '*/node-hid/prebuilds/*' \
              -not -path '*/usb/prebuilds/*' \
              -not -path '*/blake-hash/prebuilds/*' \
              -exec rm -vf {} ';'

            patchShebangs . >/dev/null  # a real lot of paths to patch, no need to litter logs

            ${builtins.path {path = inputs.self + "/scripts/darwin-no-x-compile.sh";}}

            # Patch all node-addon-api napi.h files: change 'static const' to 'static inline const'
            # (C++17) to defer the initializer to program startup, removing the constant-expression
            # requirement that Apple Clang 15+ enforces on static_cast<enum>(-1) in in-class members.
            # Needed on both x86_64 and aarch64: the package derivation's electron-rebuild rebuilds
            # all native modules from source (--force), so the patched napi.h must be present for
            # any darwin target that uses Apple Clang 15+.
            python3 -c "
      import sys
      for path in sys.argv[1:]:
          with open(path) as f:
              content = f.read()
          content = content.replace(
              'static const napi_typedarray_type unknown_array_type = static_cast<napi_typedarray_type>(-1);',
              'static inline const napi_typedarray_type unknown_array_type = static_cast<napi_typedarray_type>(-1);'
          )
          with open(path, 'w') as f:
              f.write(content)
      " $(find node_modules -name 'napi.h' -path '*/node-addon-api/*' 2>/dev/null)

            # And now, with correct shebangs, run the install scripts (we have to do that
            # semi-manually, because another `yarn install` will overwrite those shebangs…):
            find node_modules -type f -name 'package.json' | sort | xargs grep -F '"install":' | cut -d: -f1 | while IFS= read -r dependency ; do
              # The grep pre-filter is not ideal:
              if [ "$(jq .scripts.install "$dependency")" != "null" ] ; then
                # Skip packages that download binaries from GitHub — we provide them via Nix instead.
                # electron: binary comes from pkgs.electron.unwrapped (ELECTRON_SKIP_BINARY_DOWNLOAD=1 also set)
                # electron-chromedriver: version mismatch with our electron (v12 vs v41); skip, not needed for build
                case "$dependency" in
                  */electron/package.json | */electron-chromedriver/package.json)
                    echo "Skipping binary-download install script for $dependency (binary provided by Nix)"
                    continue ;;
                  # node-hid (N-API v4) and usb (N-API v8, universal fat binary) bundle
                  # ABI-stable prebuilt .node files inside their npm tarballs. Running their
                  # install scripts would invoke node-gyp which fails on Darwin due to
                  # Apple Clang 15+ rejecting static_cast<enum>(-1) in napi.h.
                  */node-hid/package.json | */usb/package.json)
                    echo "Skipping $dependency — using bundled N-API prebuilt binary"
                    continue ;;
                  # fsevents v1.x (optional dep of chokidar v2) uses NAN which calls
                  # v8::Object::GetIsolate() — removed in Electron 41+ v8 headers.
                  # Not used at runtime by Daedalus; skip compilation entirely.
                  */fsevents/package.json)
                    echo "Skipping $dependency — uses old v8 API incompatible with Electron 41; not needed at runtime"
                    continue ;;
                  ${lib.optionalString (targetSystem == "x86_64-darwin") ''
        # blake-hash ships a darwin-x64 N-API prebuilt — skip the install-script
        # compilation here. electron-rebuild in the package phase will rebuild it
        # from source (using the patched napi.h above).
        */blake-hash/package.json)
          echo "Skipping blake-hash install script — will be rebuilt by electron-rebuild"
          continue ;;
      ''}
                esac
                echo ' '
                echo "Running the install script for '$dependency':"

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

  nix-bundle-exe-same-dir = pkgs.runCommand "nix-bundle-exe-same-dir" {} ''
    cp -R ${inputs.nix-bundle-exe} $out
    chmod -R +w $out
    sed -r 's+@executable_path/\$relative_bin_to_lib/\$lib_dir+@executable_path+g' -i $out/bundle-macos.sh
  '';

  # XXX: they cannot be a symlinks, because:
  #   1) cardano-launcher looks at its own `realpath` to determine DAEDALUS_INSTALL_DIRECTORY,
  #   2) symlinks under Contents/MacOS break code signing, and notarization.
  mkBundle = exeName: target: let
    unbundled = pkgs.linkFarm "exes" {"bin/${exeName}" = target;};
  in
    (import nix-bundle-exe-same-dir {
        inherit pkgs;
        bin_dir = "bundle";
        exe_dir = "_unused_";
        lib_dir = "bundle";
      }
      unbundled).overrideAttrs (drv: {
      buildCommand =
        (
          builtins.replaceStrings
          ["'${unbundled}/bin'"]
          ["'${unbundled}/bin' -follow"]
          drv.buildCommand
        )
        + ''
          mv $out/bundle/* $out/
          rmdir $out/bundle
          ${moveDylibsToSubdir exeName}
        '';
    });

  moveDylibsToSubdir = exeName: ''
    (
      export PATH=${lib.makeBinPath (with pkgs; [darwin.cctools darwin.binutils darwin.sigtool nukeReferences])}:"$PATH"

      cd $out
      mkdir -p ${exeName}-lib
      for f in *.dylib *.so; do [ -f "$f" ] && mv "$f" ${exeName}-lib/; done
      otool -L ${exeName} \
        | { grep -E '^\s*@executable_path' || true ; } \
        | sed -r 's/^\s*//g ; s/ \(.*//g' \
        | while IFS= read -r lib ; do
            install_name_tool -change "$lib" "$(sed <<<"$lib" -r 's,@executable_path/,@executable_path/${exeName}-lib/,g')" ${exeName}
          done
      nuke-refs ${exeName}
      codesign -f -s - ${exeName} || true

      cd ${exeName}-lib
      for dylib in *.dylib *.so; do
        [ -f "$dylib" ] || continue
        otool -L "$dylib" \
          | { grep -E '^\s*@executable_path' || true ; } \
          | sed -r 's/^\s*//g ; s/ \(.*//g' \
          | while IFS= read -r lib ; do
              install_name_tool -change "$lib" "$(sed <<<"$lib" -r 's,@executable_path/,@loader_path/,g')" "$dylib"
            done
        nuke-refs "$dylib"
        codesign -f -s - "$dylib" || true
      done
    )
  '';

  bundle-cardano-launcher = mkBundle "cardano-launcher" (common.cardano-launcher + "/bin/cardano-launcher");
  bundle-cardano-node = mkBundle "cardano-node" (lib.getExe common.cardano-node);
  bundle-cardano-cli = mkBundle "cardano-cli" (lib.getExe common.cardano-cli);
  bundle-cardano-address = mkBundle "cardano-address" (lib.getExe common.cardano-address);
  bundle-mithril-client = mkBundle "mithril-client" (common.mithril-client + "/bin/mithril-client");
  bundle-snapshot-converter = mkBundle "snapshot-converter" (lib.getExe common.snapshot-converter);
  bundle-mock-token-metadata-server = mkBundle "mock-token-metadata-server" (lib.getExe common.mock-token-metadata-server);
  bundle-local-cluster = mkBundle "local-cluster" (lib.getExe common.walletPackages.local-cluster);

  # Unfortunately they bundle it upstream, but not in a subdir:
  bundle-cardano-wallet = pkgs.runCommand "bundle-cardano-wallet" {} ''
    cp -r ${common.cardano-wallet}/bin $out
    chmod -R +w $out
    ${moveDylibsToSubdir "cardano-wallet"}
  '';

  # HID.node and others depend on `/nix/store`, we have to bundle them, too:
  bundleNodeJsNativeModule = pkgs.writeShellScript "bundleNodeJsNativeModule" ''
    #!/usr/bin/env bash
    set -euo pipefail
    target="$1"
    export bin_dir="bundle"
    export exe_dir="_unused_"
    export lib_dir="bundle"
    export PATH=${lib.makeBinPath (with pkgs; [darwin.cctools darwin.binutils darwin.sigtool nukeReferences])}:"$PATH"
    tmpdir=$(mktemp -d)
    cp ${nix-bundle-exe-same-dir}/bundle-macos.sh "$tmpdir"/
    chmod -R +w "$tmpdir"
    sed -r 's/@executable_path/@loader_path/g' -i "$tmpdir"/bundle-macos.sh
    bash "$tmpdir"/bundle-macos.sh "$tmpdir" "$target"
    rm "$tmpdir"/bundle-macos.sh

    # We can’t have dots in directory names, or they’re interpreted as bundles, and code signing fails:
    libDirName="$(basename "$target" | tr . -)-lib"

    mv "$tmpdir/bundle" "$(dirname "$target")/$libDirName"
    rmdir "$tmpdir"
    rm "$target"
    mv "$(dirname "$target")/$libDirName/$(basename "$target")" "$target"

    otool -L "$target" \
      | { grep -E '^\s*@loader_path' || true ; } \
      | sed -r 's/^\s*//g ; s/ \(.*//g' \
      | while IFS= read -r lib ; do
          install_name_tool -change "$lib" "$(sed <<<"$lib" -r 's,@loader_path/,@loader_path/'"$libDirName"'/,g')" "$target"
        done
  '';

  package = genClusters (cluster: let
    pname = "daedalus";
  in
    pkgs.stdenv.mkDerivation {
      name = pname;
      src = srcWithoutNix;
      nativeBuildInputs =
        [yarn nodejs]
        ++ (with pkgs; [perl pkg-config darwin.cctools xcbuild jq])
        ++ [pkgs.python3];
      buildInputs = [
        darwin-launcher
        mock-token-metadata-server
      ];
      NETWORK = cluster;
      BUILD_REV = sourceLib.buildRev;
      BUILD_REV_SHORT = sourceLib.buildRevShort;
      BUILD_COUNTER = sourceLib.buildCounter;
      CARDANO_WALLET_VERSION = cardanoWalletVersion;
      CARDANO_NODE_VERSION = cardanoNodeVersion;
      configurePhase =
        common.setupCacheAndGypDirs
        + darwinSpecificCaches
        + ''
          # Grab all cached `node_modules` from above:
          cp -r ${node_modules}/. ./
          chmod -R +w .
        '';
      outputs = ["out" "futureInstaller"];
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
        # Copy runtime deps, handling scoped packages (@scope/pkg) correctly:
        # plain `cp -r -t dest/ node_modules/@scope/pkg` would strip the @scope
        # parent and create dest/pkg instead of dest/@scope/pkg.
        jq -r '.[]' ${./runtime-nodejs-deps.json} | while IFS= read -r pkg; do
          dest="$pathtoapp/Contents/Resources/app/node_modules"
          if [[ "$pkg" == @*/* ]]; then
            mkdir -p "$dest/$(dirname "$pkg")"
          fi
          cp -r "node_modules/$pkg" "$dest/$(dirname "$pkg")/"
        done

        mkdir -p "$pathtoapp"/Contents/Resources/app/build

        for f in \
          "usb/build/Release/usb_bindings.node" \
          "node-hid/build/Release/HID.node" \
          "usb-detection/build/Release/detection.node" \
          ; do
          cp node_modules/"$f" "$pathtoapp"/Contents/Resources/app/build/
        done

        jq >tmp-package.json <"$pathtoapp/Contents/Resources/app/package.json" \
          --arg name ${lib.escapeShellArg launcherConfigs.${cluster}.installerConfig.spacedName} \
          '.productName = $name'
        mv tmp-package.json "$pathtoapp/Contents/Resources/app/package.json"

        dir="$pathtoapp/Contents/MacOS"
        dataDir="$pathtoapp/Contents/Resources"

        mkdir -p "$dir" "$dataDir"

        echo "Preparing files ..."
        cp installers/launcher-config.yaml "$dataDir"/

        cp -r ${bundle-cardano-launcher}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-cardano-node}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-cardano-cli}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-cardano-address}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-cardano-wallet}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-mithril-client}/. "$dir"/ && chmod -R +w "$dir/"
        cp -r ${bundle-snapshot-converter}/. "$dir"/ && chmod -R +w "$dir/"

        ${lib.optionalString (cluster == "selfnode") ''
          cp -r ${bundle-mock-token-metadata-server}/. "$dir"/ && chmod -R +w "$dir/"
          cp -r ${bundle-local-cluster}/. "$dir"/ && chmod -R +w "$dir/"
        ''}

        cp installers/{config.yaml,genesis.json,topology.yaml} "$dataDir"/
        ${
          if (cluster != "selfnode")
          then ''
            cp installers/{genesis-byron.json,genesis-shelley.json,genesis-alonzo.json} "$dataDir"/
            cp installers/genesis-conway.json "$dataDir"/ || true
            cp installers/genesis-dijkstra.json "$dataDir"/ || true
            cp installers/checkpoints.json "$dataDir"/ || true
          ''
          else ''
            cp installers/{signing.key,delegation.cert} "$dataDir"/
            cp -f ${./../../utils/cardano/selfnode}/token-metadata.json "$dir"/
          ''
        }

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

        # usb v2+ uses node-gyp-build (not the 'bindings' npm package), so it looks for
        # its native module in node_modules/usb/build/Release/ — not via DAEDALUS_INSTALL_DIRECTORY.
        # Bundle Nix store dylibs in place so the path survives outside the Nix sandbox.
        ${bundleNodeJsNativeModule} "$pathtoapp/Contents/Resources/app/node_modules/usb/build/Release/usb_bindings.node"

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
          find Resources/ -name '*.node' \
            -not -path '*/usb/build/Release/*.node' \
            -exec rm -vf '{}' ';'
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

  unsignedInstaller = genClusters (cluster:
    pkgs.stdenv.mkDerivation {
      name = "daedalus-unsigned-darwin-installer";
      meta.mainProgram = "daedalus-${cluster}-installer";
      dontUnpack = true;
      buildPhase = ''
        ${makeSignedInstaller.${cluster}}/bin/* | tee make-installer.log
      '';
      installPhase = ''
        mkdir -p $out
        cp $(tail -n 1 make-installer.log) $out/

        # Create bin directory with hard link for nix run
        mkdir -p $out/bin
        ln $out/*.pkg $out/bin/daedalus-${cluster}-installer

        # Make it downloadable from Hydra:
        mkdir -p $out/nix-support
        echo "file binary-dist \"$(echo $out/*.pkg)\"" >$out/nix-support/hydra-build-products
      '';
    });

  makeSignedInstaller = genClusters (cluster:
    pkgs.writeShellScriptBin "make-signed-installer" (let
      # FIXME: in the future this has to be done better, now let’s reuse the Buildkite legacy:
      credentials = "/var/lib/buildkite-agent-default/signing.sh";
      codeSigningConfig = "/var/lib/buildkite-agent-default/code-signing-config.json";
      signingConfig = "/var/lib/buildkite-agent-default/signing-config.json";
      shallSignPredicate = "[ -f ${credentials} ] && [ -f ${codeSigningConfig} ] && [ -f ${signingConfig} ]";
      bashSetup = ''
        set -o errexit
        set -o pipefail
        set -o nounset
        export PATH="${lib.makeBinPath [pkgs.coreutils pkgs.jq]}:$PATH"
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

          source ${credentials}
          security unlock-keychain -p "$KEYCHAIN" "$signingKeyChain"
          security list-keychains -d user -s "$signingKeyChain" $(security list-keychains -d user | tr -d '"')

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
        exec sudo -u buildkite-agent-default ${packAndSign}
      else
        exec ${packAndSign}
      fi
    ''));

  darwinSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-darwin-${archSuffix}.zip";
      hash =
        if archSuffix == "x64"
        then "sha256-nQrMwhV98+s6FcefJ6nddjCZ2WqihgQcwbqaPOau1zc="
        else "sha256-epj0f0xPSTmdOoOOkVC4go0vW/iqfbdpZXroL6sh+dA=";
    };

    electronChromedriver = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/chromedriver-v${electronChromedriverVersion}-darwin-${archSuffix}.zip";
      hash =
        if archSuffix == "x64"
        then "sha256-S2Vq0oflr+PvvLNxWE/IpgLIr6MyLUsZumVQtC8V0+k="
        else "sha256-9lTlzUcf8J+GH9e8aKGIZogogdq4a3HlOqrJOawobxY=";
    };
  };
}
