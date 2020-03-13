// Example of launching the wallet for mainnet.
// (Byron rewrite of cardano-node)

var cardanoLauncher = require("cardanoLauncher");

var launcher = new cardanoLauncher.Launcher({
  networkName: "mainnet",
  stateDir: "/tmp/state-launcher",
  nodeConfig: {
    kind: "byron",
    configurationDir: "/home/rodney/iohk/cardano-node/configuration",
    network: {
      configFile: "configuration-mainnet.yaml",
      genesisFile: "mainnet-genesis.json",
      genesisHash: "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
      topologyFile: "mainnet-topology.json"
    }
  }
});

launcher.start().then(function(api) {
  console.log("*** cardano-wallet backend is ready, base URL is " + api.baseUrl);
  return launcher.stop();
}).then(function() {
  console.log("*** the cardano-wallet backend has finished");
}).catch(function(exitStatus) {
  console.log("*** there was an error starting cardano-wallet backend:\n" +
              cardanoLauncher.exitStatusMessage(exitStatus));
});
