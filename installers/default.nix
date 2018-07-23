let
  localLib = import ../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

with pkgs;
with haskell.lib;

let
  haskellPackages = haskell.packages.ghc822.override {
    overrides = self: super: {
      dhall-json = self.callPackage ./dhall-json.nix {};
      dhall = doJailbreak (self.callPackage ./dhall-haskell.nix {});
      github = self.callPackage ./github.nix {};
    };
  };

  inherit (import ../default.nix { inherit system config pkgs; }) daedalus-bridge;

  addTestStubs = pkg: overrideCabal pkg (drv: {
    testToolDepends = [
      coreutils
      (writeShellScriptBin "daedalus-bridge" "echo ${daedalus-bridge}")
      (buildEnv {
        name = "test-stubs";
        paths = let
          stub = prog: script: writeShellScriptBin prog ''
            set -e
            (>&2 echo $0 "$@")
            ${script}
          '';
        in [
          (stub "sudo" "exec $@")
          (stub "npm" ''
            mkdir -p dist release/darwin-x64/Daedalus-darwin-x64/Daedalus.app/Contents/MacOS
            touch release/darwin-x64/Daedalus-darwin-x64/Daedalus.app/Contents/MacOS/Daedalus
            touch dist/index.html
          '')
          (stub "iconutil" "exit 0")
          (stub "pkgbuild" ''
            touch "''${@: -1}"  # last argument on command line
          '')
          (stub "productbuild" ''
            test -f $4
            touch $5
          '')
          (stub "pkgutil" "exit 0")
          (stub "security" "exit 0")
          (stub "productsign" ''
            set -x
            cp "''${@: -2}"
          '')
          (stub "otool" "echo")
          (stub "nix-store" "exit 0")
          (stub "install_name_tool" "exit 0")
        ];
      })
    ];
  });

  withFilteredSource = drv: drv.overrideAttrs (oldAttrs: {
    src = localLib.cleanSourceTree oldAttrs.src;
  });

  drv = haskellPackages.callPackage ./daedalus-installer.nix {};

in

  justStaticExecutables (addTestStubs (withFilteredSource drv))
