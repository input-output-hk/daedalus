{ cardanoLib }:

{

  # XXX: `iohk-nix` no longer supports the new `topology.json` format,
  # so these are taken from:
  # <https://book.world.dev.cardano.org/environments.html> –
  # @michalrus

  # ----------------------------- vasil-dev ----------------------------- #

  vasil-dev = let
    configJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/vasil-dev/config.json";
      sha256 = "1s9c2rq8kzxbw5p368csgalr8gdxnwj3i7w6vw9jmqlf7zys8a2z";
    };

    configJson' = cardanoLib.defaultLogConfig //
    builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile (configJson))) // {
      ByronGenesisFile = byronGenesisJson;
      ShelleyGenesisFile = shelleyGenesisJson;
      AlonzoGenesisFile = alonzoGenesisJson;
      TestEnableDevelopmentNetworkProtocols = true;
    };

    topologyJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/vasil-dev/topology.json";
      sha256 = "0flh1pckqcma8kx53s9pxrhqarywaipi3azn2g5bq83sycsn66b6";
    };

    topologyJson' = builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile (topologyJson)));

    topologyFirstAccessPoint = builtins.head (builtins.head topologyJson'.PublicRoots).publicRoots.accessPoints;

    byronGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/vasil-dev/byron-genesis.json";
      sha256 = "0d8qhaxwzwjv49zwfjb8zvp80sy0bs01ijvyx62bxp07a5pnxifd";
    };

    shelleyGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/vasil-dev/shelley-genesis.json";
      sha256 = "1gd9xsphw8gyg9q87qs3yhaa52iab04533nxp9rniv35d2505lbj";
    };

    alonzoGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/vasil-dev/alonzo-genesis.json";
      sha256 = "05b37z5cxipa1grgli7l9qvgkydr3432hb2q4lxy832521k1w47d";
    };
  in rec {
    useByronWallet = false;  # TODO: right?
    private = false;
    relaysNew = topologyFirstAccessPoint.address;  # TODO: right?
    explorerUrl = "https://explorer.vasil-dev.dev.cardano.org";  # FIXME: wrong
    smashUrl = "https://smash.vasil-dev.dev.cardano.org";  # FIXME: wrong
    metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";  # FIXME: wrong
    networkConfig = nodeConfig;
    consensusProtocol = networkConfig.Protocol;
    nodeConfig = configJson';
    edgePort = topologyFirstAccessPoint.port;  # TODO: right?
    explorerConfig = throw "unused"; # cardanoLib.mkExplorerConfig "vasil-dev" nodeConfig;
    usePeersFromLedgerAfterSlot = topologyJson'.useLedgerAfterSlot;
    topologyOverride = topologyJson';  # let’s use the official topology.json
  };

  # ----------------------------- preprod ----------------------------- #

  preprod = let
    configJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/preprod/config.json";
      sha256 = "16ldgb6jd2lx2czmj3symrd84lwdx5bm92502hxpy1bybx7lgvwy";
    };

    configJson' = cardanoLib.defaultLogConfig //
    builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile (configJson))) // {
      ByronGenesisFile = byronGenesisJson;
      ShelleyGenesisFile = shelleyGenesisJson;
      AlonzoGenesisFile = alonzoGenesisJson;
      TestEnableDevelopmentNetworkProtocols = true;
    };

    topologyJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/preprod/topology.json";
      sha256 = "0yxgjpi08dfihng8261vx7lln7kv9qhns4g0v413b4saali520yk";
    };

    topologyJson' = builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile (topologyJson)));

    topologyFirstAccessPoint = builtins.head (builtins.head topologyJson'.PublicRoots).publicRoots.accessPoints;

    byronGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/preprod/byron-genesis.json";
      sha256 = "1vqcq28kypdmxw51993gq0kyag6781cfj17mvpxcraldyzyvz3yq";
    };

    shelleyGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/preprod/shelley-genesis.json";
      sha256 = "0v3mc8ghnlssb7agkigg6n2rys3hlzlih77r68a1pv9j02p0i0c6";
    };

    alonzoGenesisJson = builtins.fetchurl {
      url = "https://book.world.dev.cardano.org/environments/preprod/alonzo-genesis.json";
      sha256 = "05b37z5cxipa1grgli7l9qvgkydr3432hb2q4lxy832521k1w47d";
    };
  in rec {
    useByronWallet = false;  # TODO: right?
    private = false;
    relaysNew = topologyFirstAccessPoint.address;  # TODO: right?
    explorerUrl = "https://explorer.preprod.dev.cardano.org";  # FIXME: wrong
    smashUrl = "https://smash.preprod.dev.cardano.org";  # FIXME: wrong
    metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";  # FIXME: wrong
    networkConfig = nodeConfig;
    consensusProtocol = networkConfig.Protocol;
    nodeConfig = configJson';
    edgePort = topologyFirstAccessPoint.port;  # TODO: right?
    explorerConfig = throw "unused"; # cardanoLib.mkExplorerConfig "preprod" nodeConfig;
    usePeersFromLedgerAfterSlot = topologyJson'.useLedgerAfterSlot;
    topologyOverride = topologyJson';  # let’s use the official topology.json
  };

}
