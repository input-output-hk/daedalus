import { Application } from 'spectron';
import electronPath from 'electron';
import child_process from 'child_process';

const context = {};

export default function () {
  // Boot up the electron app before all features
  this.registerHandler('BeforeFeatures', { timeout: 20 * 1000 }, async function() {
    const app = new Application({
      path: electronPath,
      args: ['./electron/main.testing'],
      env: {
        HOT: 1,
        NODE_ENV: 'test'
      },
      waitTimeout: 10000
    });
    await app.start();
    await app.client.waitUntilWindowLoaded();
    context.app = app;
  });
  // And tear it down after all features
  this.registerHandler('AfterFeatures', function() {
    return context.app.stop();
  });

  // Make the electron app accessible in each scenario context
  this.Before({ timeout: 30 * 1000 }, async function() {
    this.client = context.app.client;
    this.browserWindow = context.app.browserWindow;
    this.client.timeoutsAsyncScript(30000);
    await new Promise(async (resolve) => {
      const bridgePath = '/Users/dominik/work/projects/input-output/daedalus/cardano-sl';

      // Killing and restarting backend
      const commands = [
        'pkill cardano-node',
        'rm -rf run/* wallet-db/ *key',
        "export WALLET_TEST='1'; ./scripts/launch.sh",
      ];
      commands.forEach(cmd => child_process.execSync(cmd, { cwd: bridgePath }));

      this.client.execute(function() {
        daedalus.environment.current = daedalus.environment.TEST;
        daedalus.reset();
      });

      // Waiting until we are connected to backend
      await this.client.executeAsync(function(done) {
        const connectToBackend = () => {
          // Wait until we are reconnected & wallets are loaded
          if (daedalus.stores.networkStatus.isCardanoConnected){
            done();
          } else {
            setTimeout(connectToBackend, 1000);
          }
        };
        connectToBackend();
      });
      resolve()
    });
  });
}
