{ pkgs, lib, inputsSelf
, nodejs, nodePackages, yarn
, cluster, daedalus-bridge, daedalus-installer, darwin-launcher, launcherConfigs, mock-token-metadata-server
, sourceLib, cardanoNodeVersion, cardanoWalletVersion
, ... }:

let

  archSuffix = if pkgs.system == "aarch64-darwin" then "arm64" else "x64";
  originalPackageJson = builtins.fromJSON (builtins.readFile ../package.json);
  electronVersion = originalPackageJson.dependencies.electron;
  packageVersion = originalPackageJson.version;
  chromedriverVersion = "12.0.0"; # FIXME: obtain programmatically
  installerName = "daedalus-${packageVersion}.${toString sourceLib.buildRevCount}-${cluster}-${sourceLib.buildRevShort}-${pkgs.system}";

in rec {

  inherit nodejs nodePackages yarn;

  yarn2nix = let
    # Nixpkgs master @ 2022-07-18
    # Why → newer `yarn2nix` uses `deep-equal` to see if anything changed in the lockfile, we need that.
    source = pkgs.fetchzip {
      url = "https://github.com/NixOS/nixpkgs/archive/qe4d49de45a3b5dbcb881656b4e3986e666141ea9.tar.gz";
      sha256 = "0y0c9ybkcfmjgrl93wzzlk7ii95kh2fb4v5ac5w6rmcsq2ff3yaz";
    };
    subdir = builtins.path { path = source + "/pkgs/development/tools/yarn2nix-moretea/yarn2nix"; };
    in
    import subdir {
      inherit pkgs nodejs yarn;
      allowAliases = true;
    };

  # To better cache node_modules, let’s only depend on package.json, and yarn.lock:
  srcLockfiles = lib.cleanSourceWith {
    src = inputsSelf;
    name = "daedalus-lockfiles";
    filter = name: type: let b = baseNameOf (toString name); in (b == "package.json" || b == "yarn.lock");
  };

  srcWithoutNix = lib.cleanSourceWith {
    src = inputsSelf;
    filter = name: type: !(type == "regular" && (
      lib.hasSuffix ".nix" name ||
      lib.hasSuffix ".hs" name ||
      lib.hasSuffix ".cabal" name
    ));
  };

  # This is the only thing we use the original `yarn2nix` for:
  offlineCache = yarn2nix.importOfflineCache (yarn2nix.mkYarnNix {
    yarnLock = srcLockfiles + "/yarn.lock";
  });

  # The following is used in all `configurePhase`s:
  setupCacheAndGypDirs = ''
    # XXX: `HOME` (for various caches) cannot be under our source root, that confuses `electron-packager`:
    export HOME=$(realpath $NIX_BUILD_TOP/home)
    mkdir -p $HOME

    # Do not look up in the registry, but in the offline cache, cf. <https://classic.yarnpkg.com/en/docs/yarnrc>:
    echo '"--offline" true' >>$HOME/.yarnrc
    yarn config set yarn-offline-mirror ${offlineCache}

    # These are sometimes useful:
    #
    # npm config set loglevel verbose
    # echo '"--verbose" true' >>$HOME/.yarnrc
    # export NODE_OPTIONS='--trace-warnings'
    # export DEBUG='*'
    # export DEBUG='node-gyp @electron/get:* electron-rebuild'

    # Don’t try to download prebuilded packages:
    echo 'buildFromSource=true' >$HOME/.prebuild-installrc

    ${lib.concatMapStringsSep "\n" (cacheDir: ''

      # Node.js headers for building native `*.node` extensions with node-gyp:
      # TODO: learn why installVersion=9 – where does it come from? see node-gyp
      mkdir -p ${cacheDir}/node-gyp/${nodejs.version}
      echo 9 > ${cacheDir}/node-gyp/${nodejs.version}/installVersion
      ln -sf ${nodejs}/include ${cacheDir}/node-gyp/${nodejs.version}

      mkdir -p ${cacheDir}/electron/${darwinSources.electronCacheHash}/
      ln -sf ${darwinSources.electronShaSums} ${cacheDir}/electron/${darwinSources.electronCacheHash}/SHASUMS256.txt
      ln -sf ${darwinSources.electron} ${cacheDir}/electron/${darwinSources.electronCacheHash}/electron-v${electronVersion}-darwin-${archSuffix}.zip

      mkdir -p ${cacheDir}/electron/${darwinSources.chromedriverCacheHash}/
      ln -sf ${darwinSources.chromedriverShaSums} ${cacheDir}/electron/${darwinSources.chromedriverCacheHash}/SHASUMS256.txt
      ln -sf ${darwinSources.chromedriver} ${cacheDir}/electron/${darwinSources.chromedriverCacheHash}/chromedriver-v${chromedriverVersion}-darwin-${archSuffix}.zip

    '') [
      "$HOME/.cache"          # Linux, Windows (cross-compiled)
      "$HOME/Library/Caches"  # Darwin
    ]}

    mkdir -p $HOME/.electron-gyp/
    ln -sf ${darwinSources.electronHeaders} $HOME/.electron-gyp/${electronVersion}
  '';

  # XXX: Whenever changing `yarn.lock`, make sure this still builds
  # without network. I.e. since there is no network sanbox in Nix on
  # Darwin, you have to first build `package` with network (to populate
  # /nix/store with pure dependencies), then add a newline in the middle
  # of `package.json`, and then build the `package` again, only this time
  # with network turned off system-wise.
  #
  # TODO: probably run `electron-rebuild` here, not in `package` (in `MacInstaller.hs`)
  node_modules = pkgs.stdenv.mkDerivation {
    name = "daedalus-node_modules";
    src = srcLockfiles;
    nativeBuildInputs = [ yarn nodejs ] ++ (with pkgs; [ python3 pkgconfig ]) ++
      [(pkgs.runCommandLocal "system-path-exports" {} ''
        mkdir -p $out/bin
        ln -sf /usr/bin/{libtool,install_name_tool} $out/bin/
      '')];
    buildInputs = (with pkgs.darwin; [
      apple_sdk.frameworks.IOKit
      apple_sdk.frameworks.AppKit
    ]);
    configurePhase = setupCacheAndGypDirs;
    buildPhase = ''
      # Do not look up in the registry, but in the offline cache:
      ${yarn2nix.fixup_yarn_lock}/bin/fixup_yarn_lock yarn.lock

      # The original is only used in devShells:
      mkdir scripts && touch scripts/postinstall.sh && chmod +x scripts/postinstall.sh

      # Now, install from ${offlineCache} to node_modules/
      yarn install --frozen-lockfile

      patchShebangs . >/dev/null  # a real lot of paths to patch, no need to litter logs
    '';
    installPhase = ''
      mkdir $out
      cp -r node_modules $out/
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

  package = let
    pname = "daedalus";
  in pkgs.stdenv.mkDerivation {
    name = pname;
    src = srcWithoutNix;
    nativeBuildInputs = ([ yarn nodejs daedalus-installer ]) ++ (with pkgs; [ python3 ]) ++
      [(pkgs.runCommandLocal "system-path-exports" {} ''
        mkdir -p $out/bin
        ln -sf /usr/bin/{iconutil,xcrun,xcodebuild,libtool,pkgbuild,productbuild} $out/bin/
      '')];
    buildInputs = (with pkgs.darwin; [
      apple_sdk.frameworks.CoreServices
      apple_sdk.frameworks.AppKit
      libobjc
    ]) ++ [
      daedalus-bridge
      darwin-launcher
      mock-token-metadata-server
    ];
    NETWORK = cluster;
    BUILD_REV = sourceLib.buildRev;
    BUILD_REV_SHORT = sourceLib.buildRevShort;
    BUILD_REV_COUNT = sourceLib.buildRevCount;
    CARDANO_WALLET_VERSION = cardanoWalletVersion;
    CARDANO_NODE_VERSION = cardanoNodeVersion;
    configurePhase = setupCacheAndGypDirs + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    outputs = [ "out" "futureInstaller" ];
    buildPhase = ''
      export DEVX_FIXME_DONT_YARN_INSTALL=1
      (
        cd installers/
        cp -r ${launcherConfigs.configFiles}/. ./.

        # make-installer needs to see `bin/nix-store` to break all references to dylibs inside /nix/store:
        export PATH="${lib.makeBinPath [ pkgs.nixUnstable ]}:$PATH"

        make-installer --cardano ${daedalus-bridge} \
          --build-rev-short ${sourceLib.buildRevShort} \
          --build-rev-count ${toString sourceLib.buildRevCount} \
          --cluster ${cluster} \
          --out-dir doesnt-matter \
          --dont-pkgbuild
      )
    '';
    installPhase = ''
      mkdir -p $out/Applications/
      cp -r release/darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.installerConfig.spacedName}-darwin-${archSuffix}/${lib.escapeShellArg launcherConfigs.installerConfig.spacedName}.app $out/Applications/

      # XXX: For `nix run`, unfortunately, we cannot use symlinks, because then golang’s `os.Executable()`
      # will not return the target, but the symlink, and all paths will break… :sigh:
      mkdir -p $out/bin/
      cat >$out/bin/${pname} << EOF
      #!/bin/sh
      exec $out/Applications/${lib.escapeShellArg launcherConfigs.installerConfig.spacedName}.app/Contents/MacOS/${lib.escapeShellArg launcherConfigs.installerConfig.spacedName}
      EOF
      chmod +x $out/bin/${pname}

      # Further signing for notarization (cannot be done purely):
      mkdir -p $futureInstaller/
      chmod +x installers/codesign.sh
      cp installers/{codesign.sh,entitlements.xml} $futureInstaller/
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

  unsignedInstaller = pkgs.stdenv.mkDerivation {
    name = "daedalus-unsigned-darwin-installer";
    dontUnpack = true;
    buildPhase = ''
      ${makeSignedInstaller}/bin/* | tee make-installer.log
    '';
    installPhase = ''
      mkdir -p $out
      cp $(tail -n 1 make-installer.log) $out/
    '';
  };

  makeSignedInstaller = pkgs.writeShellScriptBin "make-signed-installer" (let

    # FIXME: in the future this has to be done better, now let’s reuse the Buildkite legacy:
    credentials = "/var/lib/buildkite-agent/signing.sh";
    codeSigningConfig = "/var/lib/buildkite-agent/code-signing-config.json";
    signingConfig = "/var/lib/buildkite-agent/signing-config.json";
    shallSignPredicate = "[ -f ${credentials} ] && [ -f ${codeSigningConfig} ] && [ -f ${signingConfig} ]";
    readConfigs = pkgs.writeShellScript "read-signing-config" ''
      set -o errexit
      set -o pipefail
      set -o nounset
      if ${shallSignPredicate} ; then
        export PATH="${lib.makeBinPath [ pkgs.jq ]}:$PATH"
        echo "codeSigningIdentity='$(jq -r .codeSigningIdentity ${codeSigningConfig})'"
        echo "codeSigningKeyChain='$(jq -r .codeSigningKeyChain ${codeSigningConfig})'"
        echo "signingIdentity='$(jq -r .signingIdentity ${signingConfig})'"
        echo "signingKeyChain='$(jq -r .signingKeyChain ${signingConfig})'"
      else
        echo >&2 "Warning: code signing configs not found, installer will be unsigned, check" ${lib.escapeShellArg shallSignPredicate}
      fi
    '';

    packAndSign = pkgs.writeShellScript "pack-and-sign" ''
      set -o errexit
      set -o pipefail
      set -o nounset

      eval $(${readConfigs})

      workDir=$(mktemp -d)
      appName=${lib.escapeShellArg launcherConfigs.installerConfig.spacedName}.app
      appDir=${package}/Applications/"$appName"

      echo "Info: workDir = $workDir"
      cd "$workDir"

      echo "Copying…"
      cp -r "$appDir" ./.
      chmod -R +w .

      if ${shallSignPredicate} ; then
        echo "Signing code…"
        ${package.futureInstaller}/codesign.sh "$codeSigningIdentity" "$codeSigningKeyChain" \
          "$appName" ${package.futureInstaller}/entitlements.xml
      fi

      echo "Making installer…"
      /usr/bin/pkgbuild \
        --identifier ${lib.escapeShellArg ("org." + launcherConfigs.installerConfig.macPackageName + ".pkg")} \
        --component "$workDir/$appName" \
        --install-location /Applications \
        ${lib.escapeShellArg (installerName + ".phase1.pkg")}
      rm -r "$workDir/$appName"
      /usr/bin/productbuild --product ${../installers/data/plist} \
        --package *.phase1.pkg \
        ${lib.escapeShellArg (installerName + ".phase2.pkg")}
      rm *.phase1.pkg

      if ${shallSignPredicate} ; then
        echo "Signing installer…"
        (
          source ${credentials}
          security unlock-keychain -p "$SIGNING" "$signingKeyChain"
        )
        productsign --sign "$signingIdentity" --keychain "$signingKeyChain" \
          *.phase2.pkg \
          ${lib.escapeShellArg (installerName + ".pkg")}
        rm *.phase2.pkg
      else
        mv *.phase2.pkg ${lib.escapeShellArg (installerName + ".pkg")}
      fi

      echo "Done, you can submit it for notarization now:"
      echo "$workDir"/${lib.escapeShellArg (installerName + ".pkg")}
    '';

  in ''
    if ${shallSignPredicate} ; then
      exec sudo -u buildkite-agent ${packAndSign}
    else
      exec ${packAndSign}
    fi
  '');

  darwinSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-darwin-${archSuffix}.zip";
      sha256 =
        if archSuffix == "x64"
        then "0bz0a0g59fc1xc1lavv4jqy7n4r5l66pb9q6z2mbn87hssa9gw3b"
        else "06igqaayqsmcrzznpr606b097091z5r830h0x7p32jvrh42xyk9p";
    };

    electronShaSums = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/SHASUMS256.txt";
      sha256 = "081sw9hkkzsl8z4jsbnf4b6j98sn16wzfkb63yknranxzya2j99n";
    };

    electronCacheHash = builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${electronVersion}";

    electronHeaders = pkgs.runCommandLocal "electron-headers" {
      src = pkgs.fetchzip {
        url = "https://electronjs.org/headers/v${electronVersion}/node-v${electronVersion}-headers.tar.gz";
        sha256 = "0rfidarpb2y0ibx22jnd06781sx47aqsw22ksi4zxhqhjh8rqf1q";
      };
    } ''
      mkdir -p $out
      cp -r $src/. $out/.
      echo 9 >$out/installVersion
    '';

    chromedriver = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${chromedriverVersion}/chromedriver-v${chromedriverVersion}-darwin-${archSuffix}.zip";
      sha256 =
        if archSuffix == "x64"
        then "0wnwvq4m79jch0wpnl6v2yhpcwy5ccaxkkia99wrqwg4fdsxkwka"
        else "07i8d96mwm0h1r70hxny4ryp39mw2hrigh5fhp87i56cb6dmis4d";
    };

    chromedriverShaSums = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${chromedriverVersion}/SHASUMS256.txt";
      sha256 = "07xxam8dvn1aixvx39gd5x3yc1bs6i599ywxwi5cbkpf957ilpcx";
    };

    chromedriverCacheHash = builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${chromedriverVersion}";
  };
}
