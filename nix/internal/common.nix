# Things common between all OS-es, that build on all platforms.
{
  inputs,
  targetSystem,
}: rec {
  flakeLock = builtins.fromJSON (builtins.readFile (inputs.self + "/flake.lock"));

  sourceLib = import ./source-lib.nix {inherit inputs;};

  pkgs = let
    # Windows can only be cross-built from Linux now
    system =
      if targetSystem == "x86_64-windows"
      then "x86_64-linux"
      else targetSystem;
  in
    inputs.nixpkgs.legacyPackages.${system};
  # Note: The systemd patch for Ledger detection was removed during nixos-25.11 upgrade
  # TODO: Verify Ledger hardware wallet detection still works without the patch
  # If not, the patch needs to be updated for systemd 258.3

  flake-compat = import inputs.flake-compat;

  walletFlake =
    (flake-compat {
      src = inputs.cardano-wallet;
    }).defaultNix;

  nodeFlake =
    (flake-compat {
      src = inputs.cardano-node;
    }).defaultNix;

  walletPackages =
    {
      x86_64-windows = walletFlake.packages.x86_64-linux.windowsPackages;
      x86_64-linux = walletFlake.packages.x86_64-linux.staticPackages;
      x86_64-darwin = walletFlake.packages.x86_64-darwin;
      aarch64-darwin = walletFlake.packages.aarch64-darwin;
    }.${
      targetSystem
    };

  nodePackages =
    {
      x86_64-windows = nodeFlake.legacyPackages.x86_64-linux.hydraJobs.windows; # a bug in ${cardano-node}/flake.nix
      x86_64-linux = nodeFlake.hydraJobs.x86_64-linux.musl;
      x86_64-darwin = nodeFlake.packages.x86_64-darwin;
      aarch64-darwin = nodeFlake.packages.aarch64-darwin;
    }.${
      targetSystem
    };

  mithrilPackages = {
    x86_64-windows = inputs.mithril.packages.x86_64-linux.mithril-client-cli-windows;
    x86_64-linux = inputs.mithril.packages.x86_64-linux.mithril-client-cli;
    x86_64-darwin = inputs.mithril.packages.x86_64-darwin.mithril-client-cli;
    aarch64-darwin = inputs.mithril.packages.aarch64-darwin.mithril-client-cli;
  };

  inherit (walletFlake.legacyPackages.${pkgs.stdenv.hostPlatform.system}.pkgs) cardanoLib;

  daedalus-bridge = pkgs.lib.genAttrs sourceLib.installerClusters (cluster:
    import ./cardano-bridge.nix {
      target = targetSystem;
      inherit (pkgs) lib runCommandCC darwin;
      inherit cardano-wallet cardano-node cardano-launcher cardano-cli cardano-address mock-token-metadata-server mithril-client snapshot-converter;
      local-cluster =
        if cluster == "selfnode"
        then walletPackages.local-cluster
        else null;
    });

  inherit (walletPackages) cardano-wallet cardano-address mock-token-metadata-server;

  inherit (nodePackages) cardano-node cardano-cli snapshot-converter;

  mithril-client = mithrilPackages.${targetSystem};

  cardano-shell =
    (flake-compat {
      src = inputs.cardano-shell;
    }).defaultNix;

  cardano-launcher = cardano-shell.hydraJobs.cardano-launcher.${targetSystem};

  cardanoNodeVersion = cardano-node.identifier.version + "-" + builtins.substring 0 9 nodeFlake.rev;

  cardanoWalletVersion = daedalus-bridge.mainnet.wallet-version + "-" + builtins.substring 0 9 walletFlake.rev;

  mkLauncherConfigs = {
    devShell ? false,
    cluster,
  }:
    import ./launcher-config.nix {
      inherit devShell;
      inherit cardanoLib;
      inherit (pkgs) runCommand lib jq;
      system = pkgs.stdenv.hostPlatform.system;
      inherit (inputs) cardano-playground;
      network = cluster;
      os =
        {
          x86_64-windows = "windows";
          x86_64-linux = "linux";
          x86_64-darwin = "macos64";
          aarch64-darwin = "macos64-arm";
        }.${
          targetSystem
        };
    };

  launcherConfigs = pkgs.lib.genAttrs sourceLib.installerClusters (cluster:
    mkLauncherConfigs {
      devShell = false;
      inherit cluster;
    });

  tests = {
    runShellcheck = import ../tests/shellcheck.nix {src = ../.;};
  };

  originalPackageJson = builtins.fromJSON (builtins.readFile ../../package.json);

  # nodejs = let
  #   njPath = pkgs.path + "/pkgs/development/web/nodejs";
  #   buildNodeJs = pkgs.callPackage (import (njPath + "/nodejs.nix")) {
  #     python = pkgs.python39;
  #     icu = pkgs.icu68; # can't build against ICU 69: <https://chromium-review.googlesource.com/c/v8/v8/+/2477751>
  #   };
  # in
  #   buildNodeJs {
  #     enableNpm = true;
  #     version = "14.17.0";
  #     sha256 = "1vf989canwcx0wdpngvkbz2x232yccp7fzs1vcbr60rijgzmpq2n";
  #     patches = pkgs.lib.optional pkgs.stdenv.isDarwin (njPath + "/bypass-xcodebuild.diff");
  #   };

  nodejs = let
    base = pkgs.nodejs_24;
  in
    if !(pkgs.lib.hasInfix "-darwin" targetSystem)
    then base
    else
      base.overrideAttrs (drv: {
        # XXX: we don't want `bypass-xcodebuild.diff` or `bypass-darwin-xcrun-node16.patch`, rather we supply
        # the pure `xcbuild` – without that, `blake2` doesn't build,
        # cf. <https://github.com/NixOS/nixpkgs/blob/29ae6a1f3d7a8886b3772df4dc42a13817875c7d/pkgs/development/web/nodejs/bypass-xcodebuild.diff>
        patches = pkgs.lib.filter (patch:
          !(
            pkgs.lib.hasInfix "bypass-xcodebuild" patch
            || pkgs.lib.hasInfix "bypass-darwin-xcrun" patch
          ))
        drv.patches;
      });

  yarn = (pkgs.yarn.override {inherit nodejs;}).overrideAttrs (drv: {
    # XXX: otherwise, unable to run our package.json scripts in Nix sandbox (patchShebangs doesn't catch this)
    postFixup =
      (drv.postFixup or "")
      + ''
        sed -r 's,#!/bin/sh,#!${pkgs.bash}/bin/sh,g' -i $out/libexec/yarn/lib/cli.js
      '';
  });

  yarn2nix = import "${inputs.nixpkgs}/pkgs/development/tools/yarn2nix-moretea" {
    inherit pkgs;
    inherit nodejs yarn;
    allowAliases = true;
  };

  # To better cache node_modules, let's only depend on package.json, and yarn.lock:
  srcLockfiles = pkgs.lib.cleanSourceWith {
    src = inputs.self;
    name = "daedalus-lockfiles";
    filter = name: type: let b = baseNameOf (toString name); in (b == "package.json" || b == "yarn.lock");
  };

  srcWithoutNix = pkgs.lib.cleanSourceWith {
    src = inputs.self;
    filter = name: type: let
      baseName = baseNameOf (toString name);
      relPath = pkgs.lib.removePrefix (toString inputs.self + "/") (toString name);
    in
      # Exclude nix files
      !(type
        == "regular"
        && (
          pkgs.lib.hasInfix "-source/nix/" name
          || pkgs.lib.hasSuffix ".nix" name
          || pkgs.lib.hasSuffix ".hs" name
          || pkgs.lib.hasSuffix ".cabal" name
        ))
      # Exclude directories that shouldn't trigger rebuilds
      && !(type
        == "directory"
        && (
          baseName
          == ".direnv"
          || baseName == ".agent"
          || baseName == "node_modules"
          || baseName == "dist"
          || baseName == "release"
          || baseName == ".git"
        ))
      # Exclude specific files
      && !(baseName == ".envrc")
      # Exclude markdown docs but keep terms-of-use .md files (runtime assets loaded by webpack)
      && !(pkgs.lib.hasSuffix ".md" name && type == "regular" && !(pkgs.lib.hasInfix "/terms-of-use/" name));
  };

  # This is the only thing we use the original `yarn2nix` for:
  offlineCache = yarn2nix.importOfflineCache (yarn2nix.mkYarnNix {
    yarnLock = srcLockfiles + "/yarn.lock";
  });

  # installVersion must match the `installVersion` field in the node-gyp bundled
  # with our Node.js (e.g. 9 for node-gyp 9.x, 11 for node-gyp 11.x).
  # If stale, node-gyp treats the cached headers as invalid and tries to download them.
  nodegypInstallVersion =
    (pkgs.lib.importJSON "${nodejs}/lib/node_modules/npm/node_modules/node-gyp/package.json").installVersion;

  # The following is used in all `configurePhase`s:
  setupCacheAndGypDirs = ''
    # XXX: `HOME` (for various caches) cannot be under our source root, that confuses `electron-packager`:
    export HOME=$(realpath $NIX_BUILD_TOP/home)
    mkdir -p $HOME

    # Do not look up in the registry, but in the offline cache, cf. <https://classic.yarnpkg.com/en/docs/yarnrc>:
    echo '"--offline" true' >>$HOME/.yarnrc
    echo '"--frozen-lockfile" true' >>$HOME/.yarnrc
    yarn config set yarn-offline-mirror ${offlineCache}

    # Don't try to download prebuilded packages (with prebuild-install):
    export npm_config_build_from_source=true
    ( echo 'buildFromSource=true' ; echo 'compile=true' ; ) >$HOME/.prebuild-installrc

    # Skip electron binary download in install scripts (we use pkgs.electron.unwrapped instead):
    export ELECTRON_SKIP_BINARY_DOWNLOAD=1

    ${pkgs.lib.concatMapStringsSep "\n" (cacheDir: ''

        # Node.js headers for building native `*.node` extensions with node-gyp:
        mkdir -p ${cacheDir}/node-gyp/${nodejs.version}
        echo ${toString nodegypInstallVersion} > ${cacheDir}/node-gyp/${nodejs.version}/installVersion
        ln -sf ${nodejs}/include ${cacheDir}/node-gyp/${nodejs.version}

      '') [
        "$HOME/.cache" # Linux, Windows (cross-compiled)
        "$HOME/Library/Caches" # Darwin
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

  # FIXME: this has to be done better…
  temporaryNodeModulesPatches = ''
    sed -r "s/'127\.0\.0\.1'/undefined/g" -i node_modules/cardano-launcher/dist/src/cardanoNode.js

    # Has to be idempotent:
    if ! grep -qF "'-N'" node_modules/cardano-launcher/dist/src/cardanoWallet.js ; then
      sed -r "s/'serve'/\0, '+RTS', '-N', '-RTS'/g" -i node_modules/cardano-launcher/dist/src/cardanoWallet.js
    fi

    # Has to be idempotent:
    if ! grep -qF "'-N'" node_modules/cardano-launcher/dist/src/cardanoNode.js ; then
      sed -r "s/config.rtsOpts/(\0 || []).concat(['-N'])/g" -i node_modules/cardano-launcher/dist/src/cardanoNode.js
    fi
  '';

  electronVersion = originalPackageJson.dependencies.electron;

  # Chromedriver is released alongside electron with a matching version number.
  electronChromedriverVersion = electronVersion;

  commonSources = let
    # Use nixpkgs electron headers instead of fetching from upstream.
    # A tarball is needed for patchElectronRebuild's --tarball arg (--nodedir takes priority).
    electronHeadersTarball = pkgs.runCommand "electron-headers-${electronVersion}.tar.gz" {
      nativeBuildInputs = [pkgs.gnutar];
    } "tar czf $out -C ${pkgs.electron.headers} .";
  in {
    electronHeaders = pkgs.runCommandLocal "electron-headers" {src = electronHeadersTarball;} ''
      cp -r ${pkgs.electron.headers}/. $out
      chmod -R +w $out
      echo ${toString nodegypInstallVersion} >$out/installVersion
    '';

    electronShaSums = pkgs.fetchurl {
      name = "electronShaSums-${electronVersion}"; # cache invalidation
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/SHASUMS256.txt";
      hash = "sha256-+tI8kWgYS9VrI+DRiXkhN0Nt1CT3yAWxcw8N72XrUE8=";
    };

    electronCacheHash =
      builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${electronVersion}";

    electronChromedriverShaSums = pkgs.fetchurl {
      name = "electronChromedriverShaSums-${electronChromedriverVersion}"; # cache invalidation
      url = "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}/SHASUMS256.txt";
      hash = "sha256-+tI8kWgYS9VrI+DRiXkhN0Nt1CT3yAWxcw8N72XrUE8=";
    };

    electronChromedriverCacheHash =
      builtins.hashString "sha256"
      "https://github.com/electron/electron/releases/download/v${electronChromedriverVersion}";
  };

  # We patch `node_modules/electron-rebuild` to force specific Node.js
  # headers to be used when building native extensions for
  # Electron. Electron's Node.js ABI differs from the same version of
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
    echo 'Patching electron-rebuild to force our Node.js headers and CXXFLAGS…'

    tarball="''${1:-${commonSources.electronHeaders.src}}"
    nodedir="''${2:-${commonSources.electronHeaders}}"

    echo "  → tarball=$tarball"
    echo "  → nodedir=$nodedir"
    echo "  → forcing CXXFLAGS=-Wno-error for Darwin builds"

    nodeGypJs="node_modules/@electron/rebuild/lib/module-type/node-gyp/node-gyp.js"

    # Patch idempotently (matters in repetitive shell.nix):
    if ! grep -qF "$tarball" $nodeGypJs ; then
      sed -r "s|const extraNodeGypArgs.*|\0 extraNodeGypArgs.push('--tarball', '$tarball', '--nodedir', '$nodedir'); process.env.CXXFLAGS='-Wno-error'; process.env.npm_config_cxxflags='-Wno-error';|" -i $nodeGypJs
    fi

    echo "  → result=$(grep -F "const extraNodeGypArgs" $nodeGypJs)"
  '';
}
