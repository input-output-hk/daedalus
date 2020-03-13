// Example of launching the wallet for incentivized testnet.
// (Jörmungandr backend)

var cardanoLauncher = require("cardanoLauncher");

var launcher = new cardanoLauncher.Launcher({
  networkName: "itn_rewards_v1",
  stateDir: "/tmp/state-launcher",
  nodeConfig: {
    kind: "jormungandr",
    // nix-build <iohk_nix/release.nix> -A jormungandrConfigs.itn_rewards_v1
    configurationDir: "/nix/store/im8zdvg17f32fhpv3y4sm0ar9wrgpmrj-jormungandr-config",
    network: {
      configFile: 'config.yaml',
      genesisBlock: {
        hash: '8e4d2a343f3dcf9330ad9035b3e8d168e6728904262f2c434a4f8f934ec7b676',
      },
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
