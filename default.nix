let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, version ? (builtins.fromJSON (builtins.readFile (./. + "/package.json"))).version
, buildNum ? null
}:

let
  src = localLib.cleanSourceTree ./.;
  cardanoPkgs = import ./cardano-sl.nix {
    inherit system config;
  };

  networks = localLib.splitString " " (builtins.replaceStrings ["\n"] [""] (builtins.readFile ./installer-clusters.cfg));
  forNetworks = localLib.genAttrs networks;

  packages = self: ({
    inherit pkgs;
    inherit (cardanoPkgs) daedalus-bridge;
    version = version + localLib.versionSuffix buildNum;

    tests = {
      runFlow = self.callPackage ./tests/flow.nix { inherit src; };
      runLint = self.callPackage ./tests/lint.nix { inherit src; };
      runShellcheck = self.callPackage ./tests/shellcheck.nix { inherit src; };
    };

    # Just the frontend javascript, built with npm and webpack.
    frontend = self.callPackage ./installers/yarn2nix.nix {
      inherit version;
      src = localLib.npmSourceTree ./.;
      # Backend/API is always Cardano SL at the moment
      backend = {
        api = "ada";
        version = self.daedalus-bridge.version;
      };
    };

    # Daedalus app for nix.
    daedalus = self.callPackage ./installers/daedalus.nix {
      daedalus-configs = forNetworks (network: self.${network}.daedalus-config);
    };

    # Haskell scripts to assist with generating installer files and configs
    daedalus-installer = self.callPackage ./installers {};
    dhall = "${self.daedalus-installer.src}/dhall";

    # Function to create an AppImage with AppImageKit runtime
    makeAppImage = self.callPackage ./installers/make-appimage.nix {};

    # Pre-built releases of electron for different platforms
    electronBinaries = self.callPackage ./installers/electron-binaries.nix {};

  } // forNetworks (network: let
    # These are packages specialised to a network
    packages = super: self: {
      inherit network;

      # Cardano node and launcher config, generated from dhall sources.
      daedalus-config = self.callPackage ./installers/daedalus-config.nix {};

      # Daedalus app for Linux, with a desktop launcher and icon.
      daedalus-desktop = self.callPackage ./installers/desktop.nix {};
      desktopItem = self.callPackage ./installers/desktop-item.nix {};

      # Self-contained AppImages of Daedalus suitable for other distros.
      appImage' = self.callPackage ./installers/appimage.nix {
        daedalus = self.daedalus-desktop;
      };
      appImage = localLib.wrapPackage buildNum self.appImage';

      # nix-bundle based installer for linux
      linuxInstaller' = import ./installers/linux-installer.nix {
        inherit self pkgs;
      };
      linuxInstaller = localLib.wrapPackage buildNum self.linuxInstaller';
    };
  in self.overrideScope packages));

in pkgs.lib.makeScope pkgs.newScope packages
