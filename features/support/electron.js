import { Application } from 'spectron';
import { defineSupportCode } from 'cucumber';
import electronPath from 'electron';
import path from 'path';

const context = {};

const DEFAULT_TIMEOUT = 20000;

defineSupportCode(({ AfterAll, BeforeAll, Before, setDefaultTimeout }) => {
  // The cucumber timeout should be high (and never reached in best case)
  // because the errors thrown by webdriver.io timeouts are more descriptive
  // and helpful than "this step timed out after 5 seconds" messages
  setDefaultTimeout(DEFAULT_TIMEOUT + 1000);

  // Boot up the electron app before all features
  BeforeAll({ timeout: 5 * 60 * 1000 }, async () => {
    const app = new Application({
      path: electronPath,
      args: ['./electron/main.testing'],
      env: {
        HOT: process.env.HOT,
        NODE_ENV: 'test'
      },
      waitTimeout: DEFAULT_TIMEOUT
    });
    await app.start();
    await app.client.waitUntilWindowLoaded();
    context.app = app;
  });

  // And tear it down after all features
  AfterAll(() => {
    // Since we can have multiple instances of Daedalus running,
    // it is easier to keep them open after running tests locally.
    // TODO: this must be improved for CI testing though (i guess).
    // return context.app.stop();
  });

  // Make the electron app accessible in each scenario context
  Before({ timeout: DEFAULT_TIMEOUT * 2 }, async function () {
    this.client = context.app.client;
    this.browserWindow = context.app.browserWindow;

    // Set timeouts of various operations:

    // Determines when to interrupt a script that is being evaluated.
    this.client.timeouts('script', DEFAULT_TIMEOUT);
    // Provides the timeout limit used to interrupt navigation of the browsing context.
    this.client.timeouts('pageLoad', DEFAULT_TIMEOUT);
    // Do not set 'implicit' timeout here because of this issue:
    // https://github.com/webdriverio/webdriverio/issues/974

    // Reset backend
    await this.client.executeAsync((done) => {
      const resetBackend = () => {
        if (daedalus.stores.networkStatus.isConnected) {
          daedalus.api.ada.testReset()
            .then(() => daedalus.api.localStorage.reset())
            .then(done)
            .catch((error) => done(error));
        } else {
          setTimeout(resetBackend, 50);
        }
      };
      resetBackend();
    });

    // Load fresh root url with test environment for each test case
    await this.client.url('file://' + path.join(__dirname, '../../app/index.html?test=true'));

    // Ensure that frontend is synced and ready before test case
    await this.client.executeAsync((done) => {
      const waitUntilSyncedAndReady = () => {
        if (daedalus.stores.networkStatus.isSynced) {
          done();
        } else {
          setTimeout(waitUntilSyncedAndReady, 50);
        }
      };
      waitUntilSyncedAndReady();
    });
  });
});
