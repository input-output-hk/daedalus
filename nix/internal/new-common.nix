# Things common between all OS-es, that build on all platforms.

{ inputs, targetSystem, cluster }:

rec {

  sourceLib = import ./source-lib.nix { inherit inputs; };

  oldCode = import ./old-default.nix {
    target = targetSystem;
    inherit inputs cluster;
    devShell = false;
  };

  inherit (oldCode) pkgs;

  originalPackageJson = builtins.fromJSON (builtins.readFile ../../package.json);

  # nodejs = let
  #   njPath = pkgs.path + "/pkgs/development/web/nodejs";
  #   buildNodeJs = pkgs.callPackage (import (njPath + "/nodejs.nix")) {
  #     python = pkgs.python39;
  #     icu = pkgs.icu68; # can’t build against ICU 69: <https://chromium-review.googlesource.com/c/v8/v8/+/2477751>
  #   };
  # in
  #   buildNodeJs {
  #     enableNpm = true;
  #     version = "14.17.0";
  #     sha256 = "1vf989canwcx0wdpngvkbz2x232yccp7fzs1vcbr60rijgzmpq2n";
  #     patches = pkgs.lib.optional pkgs.stdenv.isDarwin (njPath + "/bypass-xcodebuild.diff");
  #   };

  nodejs = let
    base = pkgs.nodejs-18_x;
  in if !(pkgs.lib.hasInfix "-darwin" targetSystem) then base else base.overrideAttrs (drv: {
    # XXX: we don’t want `bypass-xcodebuild.diff` or `bypass-darwin-xcrun-node16.patch`, rather we supply
    # the pure `xcbuild` – without that, `blake2` doesn’t build,
    # cf. <https://github.com/NixOS/nixpkgs/blob/29ae6a1f3d7a8886b3772df4dc42a13817875c7d/pkgs/development/web/nodejs/bypass-xcodebuild.diff>
    patches = pkgs.lib.filter (patch: !(
      pkgs.lib.hasInfix "bypass-xcodebuild" patch ||
      pkgs.lib.hasInfix "bypass-darwin-xcrun" patch
    )) drv.patches;
  });

  nodePackages = pkgs.nodePackages.override { inherit nodejs; };

  yarn = (pkgs.yarn.override { inherit nodejs; }).overrideAttrs (drv: {
    # XXX: otherwise, unable to run our package.json scripts in Nix sandbox (patchShebangs doesn’t catch this)
    postFixup = (drv.postFixup or "") + ''
      sed -r 's,#!/bin/sh,#!${pkgs.bash}/bin/sh,g' -i $out/libexec/yarn/lib/cli.js
    '';
  });

  yarn2nix = let
    # Nixpkgs master @ 2022-07-18
    # Why → newer `yarn2nix` uses `deep-equal` to see if anything changed in the lockfile, we need that.
    source = pkgs.fetchzip {
      url = "https://github.com/NixOS/nixpkgs/archive/e4d49de45a3b5dbcb881656b4e3986e666141ea9.tar.gz";
      hash = "sha256-X/nhnMCa1Wx4YapsspyAs6QYz6T/85FofrI6NpdPDHg=";
    };
    subdir = builtins.path { path = source + "/pkgs/development/tools/yarn2nix-moretea/yarn2nix"; };
    in
    import subdir {
      inherit pkgs nodejs yarn;
      allowAliases = true;
    };

  # To better cache node_modules, let’s only depend on package.json, and yarn.lock:
  srcLockfiles = pkgs.lib.cleanSourceWith {
    src = inputs.self;
    name = "daedalus-lockfiles";
    filter = name: type: let b = baseNameOf (toString name); in (b == "package.json" || b == "yarn.lock");
  };

  srcWithoutNix = pkgs.lib.cleanSourceWith {
    src = inputs.self;
    filter = name: type: !(type == "regular" && (
      pkgs.lib.hasSuffix ".nix" name ||
      pkgs.lib.hasSuffix ".hs" name ||
      pkgs.lib.hasSuffix ".cabal" name
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
    echo '"--frozen-lockfile" true' >>$HOME/.yarnrc
    yarn config set yarn-offline-mirror ${offlineCache}

    # Don’t try to download prebuilded packages (with prebuild-install):
    export npm_config_build_from_source=true
    ( echo 'buildFromSource=true' ; echo 'compile=true' ; ) >$HOME/.prebuild-installrc

    ${pkgs.lib.concatMapStringsSep "\n" (cacheDir: ''

      # Node.js headers for building native `*.node` extensions with node-gyp:
      # TODO: learn why installVersion=9 – where does it come from? see node-gyp
      mkdir -p ${cacheDir}/node-gyp/${nodejs.version}
      echo 9 > ${cacheDir}/node-gyp/${nodejs.version}/installVersion
      ln -sf ${nodejs}/include ${cacheDir}/node-gyp/${nodejs.version}

    '') [
      "$HOME/.cache"          # Linux, Windows (cross-compiled)
      "$HOME/Library/Caches"  # Darwin
    ]}

    mkdir -p $HOME/.electron-gyp/
    ln -sf ${commonSources.electronHeaders} $HOME/.electron-gyp/${electronVersion}

    # These are sometimes useful:
    #
    # npm config set loglevel verbose
    # echo '"--verbose" true' >>$HOME/.yarnrc
    # export NODE_OPTIONS='--trace-warnings'
    # export DEBUG='*'
    # export DEBUG='node-gyp @electron/get:* electron-rebuild'
  '';

  electronVersion = originalPackageJson.dependencies.electron;

  versionInOfflineCache = safeName:
    __unsafeDiscardStringContext (__readFile (pkgs.runCommandLocal "electron-chromedriver-version" {} ''
      ls ${offlineCache} | grep -F ${pkgs.lib.escapeShellArg (safeName + "___" + safeName)} | grep -Po '\d+(\.\d+)*' | tr -d '\n' >$out
    ''));

  electronChromedriverVersion = versionInOfflineCache "electron_chromedriver";

  commonSources = {
    electronHeaders = pkgs.runCommandLocal "electron-headers" {
      # XXX: don’t use fetchzip, we need the raw .tar.gz in `patchElectronRebuild` below
      src = pkgs.fetchurl {
        url = "https://electronjs.org/headers/v${electronVersion}/node-v${electronVersion}-headers.tar.gz";
        hash = "sha256-er08CKt3fwotSjYxqdzpm8Q0YjvD1PhfNBDZ3Jozsvk=";
      };
    } ''
      tar -xf $src
      mv node_headers $out
      echo 9 >$out/installVersion
    '';

    electronShaSums = pkgs.fetchurl {
      name = "electronShaSums-${electronVersion}"; # cache invalidation
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/SHASUMS256.txt";
      hash = "sha256-75bNqt2c7u/fm0P2Ha6NvkbGThEifIHXl2x5UCdy4fM=";
    };

    electronCacheHash = builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${electronVersion}";

    electronChromedriverShaSums = pkgs.fetchurl {
      name = "electronChromedriverShaSums-${electronChromedriverVersion}"; # cache invalidation
      url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/SHASUMS256.txt";
      hash = "sha256-nV0aT0nuzsVK5J37lEo0egXmRy/tpdF3jyrY3VBVvR8=";
    };

    electronChromedriverCacheHash = builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}";
  };

  # We patch `node_modules/electron-rebuild` to force specific Node.js
  # headers to be used when building native extensions for
  # Electron. Electron’s Node.js ABI differs from the same version of
  # Node.js, because different libraries are used in Electon,
  # e.g. BoringSSL instead of OpenSSL,
  # cf. <https://www.electronjs.org/docs/latest/tutorial/using-native-node-modules>
  #
  # We also use this same code in `shell.nix`, since for some reason
  # `electron-rebuild` there determines incorrect headers to use
  # automatically, and we keep getting ABI errors. TODO: investigate
  # why…
  #
  # TODO: That `sed` is rather awful… Can it be done better? – @michalrus
  patchElectronRebuild = pkgs.writeShellScriptBin "patch-electron-rebuild" ''
    echo 'Patching electron-rebuild to force our Node.js headers…'

    tarball="''${1:-${commonSources.electronHeaders.src}}"
    nodedir="''${2:-${commonSources.electronHeaders}}"

    echo "  → tarball=$tarball"
    echo "  → nodedir=$nodedir"

    nodeGypJs="node_modules/@electron/rebuild/lib/module-type/node-gyp/node-gyp.js"

    # Patch idempotently (matters in repetitive shell.nix):
    if ! grep -qF "$tarball" $nodeGypJs ; then
      sed -r "s|const extraNodeGypArgs.*|\0 extraNodeGypArgs.push('--tarball', '$tarball', '--nodedir', '$nodedir');|" -i $nodeGypJs
    fi

    echo "  → result=$(grep -F "const extraNodeGypArgs" $nodeGypJs)"
  '';

}
