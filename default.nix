{ cluster ? "mainnet" }:
let
  pkgs = import (import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))) { config = {}; overlays = []; };
  packages = self: {
    master_config = {
      daedalus_build_number = "3619";
      cardano_rev = "0c1fab91";
      daedalus_hash = "0w0pz93yz380xrizlsbzm9wisnf99s6z2jq0ph9xqap9cpjlyr7x";
      cardano_hash = "1z2yjkm0qwhf588qnxcbz2d5mxvhqdxawwl8dczfnl47rb48jm52";
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
    newBundle = (import ./nix-installer.nix { installedPackages = [ self.daedalus ]; }).installerBundle;
    configFiles = with self; pkgs.runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/../log-config-prod.yaml"} log-config-prod.yaml
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
