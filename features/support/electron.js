import { Application } from 'spectron';
import electronPath from 'electron';
import path from 'path';

const context = {};
let isFirstScenario = true;

export default function () {
  // Boot up the electron app before all features
  this.registerHandler('BeforeFeatures', { timeout: 5 * 60 * 1000 }, async function() {
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
  this.Before({ timeout: 5 * 60 * 1000 }, async function() {
    this.client = context.app.client;
    this.browserWindow = context.app.browserWindow;
    this.client.url('/');
    this.client.timeouts('script', 30 * 1000);

    await this.client.executeAsync(function(isFirst, rootDir, done) {
      daedalus.environment.current = daedalus.environment.TEST;
      // Patch parts of the cardano api in the test environment
      if (daedalus.environment.CARDANO_API) {
        daedalus.test.patchCardanoApi(daedalus.api);
        // Reset the stores so that the patched api is used instead
        daedalus.reset();
      }
      if (!isFirst) daedalus.reset();
      const connectToBackend = () => {
        if (daedalus.stores.networkStatus.isSynced) {
          daedalus.api.testReset().then(() => {
            if (isFirst) {
              daedalus.actions.networkStatus.isSyncedAndReady.once(done);
            } else {
              done();
            }
          });
        } else {
          setTimeout(connectToBackend, 100);
        }
      };
      connectToBackend();
    }, isFirstScenario, path.join(__dirname, '../..'));
    isFirstScenario = false;
  });
}
