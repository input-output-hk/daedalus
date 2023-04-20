# Things common between all OS-es, that build on all platforms.

{ inputs, targetSystem, cluster }:

rec {

  sourceLib = import ./source-lib.nix { inherit inputs; };

  oldCode = import ./old-default.nix {
    target = targetSystem;
    inherit inputs cluster sourceLib;
  };

  inherit (oldCode) pkgs;

  originalPackageJson = builtins.fromJSON (builtins.readFile ../package.json);

  electronVersion = originalPackageJson.dependencies.electron;

  electronHeaders = pkgs.runCommandLocal "electron-headers" {
    # XXX: don’t use fetchzip, we need the raw .tar.gz in `patchElectronRebuild` below
    src = pkgs.fetchurl {
      url = "https://electronjs.org/headers/v${electronVersion}/node-v${electronVersion}-headers.tar.gz";
      hash = "sha256-+FZ1EYV6tiZZUFulFYtq1pr861EhBaMlHRgP5H9ENmw=";
    };
  } ''
    tar -xf $src
    mv node_headers $out
    echo 9 >$out/installVersion
  '';

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

    nodeGypJs=lib/src/module-type/node-gyp.js
    if [ ! -e $nodeGypJs ] ; then
      # makes it work both here, and in shell.nix:
      nodeGypJs="node_modules/electron-rebuild/$nodeGypJs"
    fi
    if [ ! -e $nodeGypJs ] ; then
      echo >&2 'fatal: shouldn’t happen unless electron-rebuild changes'
      exit 1
    fi

    # Patch idempotently (matters in repetitive shell.nix):
    if ! grep -qF ${electronHeaders.src} $nodeGypJs ; then
      sed -r 's|const extraNodeGypArgs.*|\0 extraNodeGypArgs.push("--tarball", "${electronHeaders.src}", "--nodedir", "${electronHeaders}");|' -i $nodeGypJs
    fi
  '';

}
