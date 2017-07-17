import { Application } from 'spectron';
import electronPath from 'electron';
import path from 'path';

const context = {};
let isFirstScenario = true;

const DEFAULT_TIMEOUT = 30000;

export default function () {

  // The cucumber timeout should be high (and never reached in best case)
  // because the errors thrown by webdriver.io timeouts are more descriptive
  // and helpful than "this step timed out after 5 seconds" messages
  this.setDefaultTimeout(DEFAULT_TIMEOUT * 10);

  // Boot up the electron app before all features
  this.registerHandler('BeforeFeatures', { timeout: 5 * 60 * 1000 }, async function() {
    const app = new Application({
      path: electronPath,
      args: ['./electron/main.testing'],
      env: {
        HOT: 1,
        NODE_ENV: 'test'
      },
      waitTimeout: DEFAULT_TIMEOUT
    });
    await app.start();
    await app.client.waitUntilWindowLoaded();
    context.app = app;
  });

  // And tear it down after all features
  this.registerHandler('AfterFeatures', function() {
    // Since we can have multiple instances of Daedalus running,
    // it is easier to keep them open after running tests locally.
    // TODO: this must be improved for CI testing though (i guess).
    // return context.app.stop();
  });

  // Make the electron app accessible in each scenario context
  this.Before({ timeout: 5 * 60 * 1000 }, async function() {
    this.client = context.app.client;
    this.browserWindow = context.app.browserWindow;
    this.client.url('/');

    // Set timeouts of various operations:

    // Determines when to interrupt a script that is being evaluated.
    this.client.timeouts('script', DEFAULT_TIMEOUT);
    // Provides the timeout limit used to interrupt navigation of the browsing context.
    this.client.timeouts('pageLoad', DEFAULT_TIMEOUT);
    // Do not set 'implicit' timeout here because of this issue:
    // https://github.com/webdriverio/webdriverio/issues/974

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
