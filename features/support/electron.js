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
    this.client.url('/');

    await this.client.executeAsync(function(done) {
      daedalus.environment.current = daedalus.environment.TEST;
      const connectToBackend = () => {
        if (daedalus.stores.networkStatus.isSynced){
          daedalus.api.testReset();
          daedalus.actions.networkStatus.isSyncedAndReady.once(done);
        }
        else {
          setTimeout(connectToBackend, 100);
        }
      };
      connectToBackend();
    });
  });
}
