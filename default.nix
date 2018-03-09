{ cluster ? "mainnet" }:
let
  pkgs = import (import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))) { config = {}; overlays = []; };
  packages = self: {
    master_config = {
      daedalus_darwin_url = "https://update-cardano-mainnet.iohk.io/Daedalus-installer-1.1.0.408.pkg";
      cardano_rev = "eef095";
      daedalus_hash = "1b0iy6c5nlsa2n2ylcc2kfm8ykkmdp6ncnx4lwwvc6f0662cf175";
      cardano_hash = "151qjqswrcscr2afsc6am4figw09hyxr80nd3dv3c35dvp2xx4rp";
    };
    inherit cluster;
    cardanoSrc = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "cardano-sl";
      rev = self.master_config.cardano_rev;
      sha256 = self.master_config.cardano_hash;
    };
    cardanoPkgs = import self.cardanoSrc { gitrev = self.cardanoSrc.rev; };
    daedalus = self.callPackage ./linux.nix {};
    bundle = self.callPackage ./nix-bundle.nix {};
    rawapp = self.callPackage ./rawapp.nix {};
    tarball = self.callPackage ./tarball.nix {};
    tarballInstaller = self.callPackage ./tarball-installer.nix {};
    nix-bundle = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "nix-bundle";
      rev = "630e89d1d16083";
      sha256 = "1s9vzlsfxd2ym8jzv2p64j6jlwr9cmir45mb12yzzjr4dc91xk8x";
    }) { nixpkgs = pkgs; };
    newBundle = (import ./nix-installer.nix { installedPackages = [ self.daedalus pkgs.strace ]; }).installerBundle;
    configFiles = with self; pkgs.runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/../log-configs/daedalus.yaml"} daedalus.yaml
      cp -vi ${topologyFile} topology.yaml
    '';
    topologyFile = self.topologies.${cluster};
    topologies.mainnet = pkgs.writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: relays.cardano-mainnet.iohk.io }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
  };
in pkgs.lib.makeScope pkgs.newScope packages
