// @flow
import path from 'path';
import fs from 'fs';
import { Application } from 'spectron';
import {
  BeforeAll,
  Before,
  After,
  AfterAll,
  setDefaultTimeout,
} from 'cucumber';
import electronPath from 'electron';
import fakeDialog from 'spectron-fake-dialog';
import { includes } from 'lodash';
import {
  generateScreenshotFilePath,
  getTestNameFromTestFile,
  saveScreenshot,
  waitAndClick,
  waitAndGetText,
  waitAndSetValue,
  skippablePromise,
} from './common/e2e/steps/helpers';
import { DEFAULT_TIMEOUT } from './common/e2e/steps/config';
import { setNewsFeedIsOpen, resetTestNews } from './news/e2e/steps/newsfeed-steps';
import { refreshClient } from './nodes/e2e/steps/helpers';
import { TEST } from '../source/common/types/environment.types';
import type { Daedalus } from './types';

/* eslint-disable consistent-return */

declare var daedalus: Daedalus;
const context = {};
let scenariosCount = 0;

const printMainProcessLogs = () =>
  context.app.client.getMainProcessLogs().then(logs => {
    // eslint-disable-next-line no-console
    console.log('========= DAEDALUS LOGS =========');
    // eslint-disable-next-line no-console
    logs.forEach(log => console.log(log));
    // eslint-disable-next-line no-console
    console.log('=================================');
    return true;
  });

const defaultWalletKeyFilePath = path.resolve(
  __dirname,
  './wallets/e2e/documents/default-wallet.key'
);

const startApp = async () => {
  const app = new Application({
    path: electronPath,
    args: ['./dist/main/index.js'],
    requireName: 'spectronRequire',
    env: Object.assign({}, process.env, {
      NODE_ENV: TEST,
    }),
    startTimeout: DEFAULT_TIMEOUT,
    waitTimeout: DEFAULT_TIMEOUT,
    chromeDriverLogPath: path.join(
      __dirname,
      '../logs/chrome-driver.log'
    ),
    webdriverLogPath: path.join(__dirname, '../logs/webdriver'),
  });
  fakeDialog.apply(app);
  await app.start();
  // TODO: develop mock that accept custom value to return
  fakeDialog.mock([
    { method: 'showOpenDialog', value: [defaultWalletKeyFilePath] },
  ]);
  await app.client.waitUntilWindowLoaded();
  return app;
};

// The cucumber timeout should be high (and never reached in best case)
// because the errors thrown by webdriver.io timeouts are more descriptive
// and helpful than "this step timed out after 5 seconds" messages
setDefaultTimeout(DEFAULT_TIMEOUT + 1000);

function getTagNames(testCase) {
  return testCase.pickle.tags.map(t => t.name);
}

// Boot up the electron app before all features
BeforeAll({ timeout: 5 * 60 * 1000 }, async () => {
  context.app = await startApp();
});

// Skip / Execute test depending on node integration
Before(async function(testCase) {
  const tags = getTagNames(testCase);
  const isByronTest = includes(tags, '@byron');
  const isShelleyTest = includes(tags, '@shelley');
  const isByronTestWip = includes(tags, '@api-wip-byron');
  const isShelleyTestWip = includes(tags, '@api-wip-shelley');
  const isGlobalWip = includes(tags, '@wip');

  // Check if ITN set globally
  const isIncentivizedTestnet = await context.app.client.execute(() => global.isIncentivizedTestnet);
  // Skip all Byron related tests or Shelley WIP
  if (isIncentivizedTestnet.value && ((isByronTest && !isShelleyTest) || isShelleyTestWip)) return 'skipped';
  // Skip all Shelley related tests or Byron WIP
  if (!isIncentivizedTestnet.value && ((isShelleyTest && !isByronTest) || isByronTestWip)) return 'skipped';
  if (isGlobalWip) return 'skipped';
});

// Make the electron app accessible in each scenario context
Before({ tags: '@e2e', timeout: DEFAULT_TIMEOUT * 2 }, async function(testCase) {
  const tags = getTagNames(testCase);
  this.app = context.app;
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
  await this.client.executeAsync(done => {
    const resetBackend = () => {
      if (daedalus.stores.networkStatus.isConnected) {
        daedalus.stores.wallets
          ._pausePolling()
          .then(() => daedalus.stores.wallets.resetWalletsData())
          .then(() => daedalus.api.ada.testReset())
          .then(() => daedalus.api.ada.resetTestOverrides())
          .then(() => daedalus.api.localStorage.reset())
          .then(() => daedalus.stores.wallets._resumePolling())
          .then(() => daedalus.stores.wallets.refreshWalletsData())
          .then(done)
          .catch(error => done(error));
      } else {
        setTimeout(resetBackend, 50);
      }
    };
    resetBackend();
  });

  // Load fresh root url with test environment for each test case
  if (!tags.includes('@noReload')) {
    await refreshClient(this.client);
  }

  // Ensure that frontend is synced and ready before test case
  await this.client.executeAsync(done => {
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

Before({ tags: '@newsfeed' }, function() {
  setNewsFeedIsOpen(this.client, false);
  resetTestNews(this.client);
});

// adds waitAndClick method to webdriver
Before(function(testCase) {
  const { name } = testCase.pickle;
  this.skippablePromise = skippablePromise.bind(this, name);
  this.waitAndClick = waitAndClick.bind(this);
  this.waitAndGetText = waitAndGetText.bind(this);
  this.waitAndSetValue = waitAndSetValue.bind(this);
});

// ads intl method to webdriver
Before({ tags: '@e2e' }, function() {
  this.intl = async (translationId, translationValues = {}) => {
    const translation = await this.client.execute(
      (id, values) => {
        const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line
        const locale = daedalus.stores.profile.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider(
          { locale, messages: messages[locale] },
          {}
        );
        return intlProvider
          .getChildContext()
          .intl.formatMessage({ id }, values);
      },
      translationId,
      translationValues
    );
    return translation.value;
  };
});

// this ensures that the spectron instance of the app restarts
// after the node update acceptance test shuts it down via 'kill-process'
// eslint-disable-next-line prefer-arrow-callback
After({ tags: '@restartApp' }, async function() {
  context.app = await startApp();
});

// this ensures that the reset-backend call successfully executes
// after the app version difference test sets the app to disconnected state
// eslint-disable-next-line prefer-arrow-callback
After({ tags: '@reconnectApp' }, async function() {
  await this.client.executeAsync(done => {
    daedalus.api.ada.resetTestOverrides();
    daedalus.stores.networkStatus
      ._updateNetworkStatus()
      .then(done)
      .catch(error => done(error));
  });
});

// eslint-disable-next-line prefer-arrow-callback
After({ tags: '@e2e' }, async function({ sourceLocation, result }) {
  scenariosCount++;
  if (result.status === 'failed') {
    const testName = getTestNameFromTestFile(sourceLocation.uri);
    const file = generateScreenshotFilePath(testName);
    await saveScreenshot(context.app, file);
    await printMainProcessLogs();
  }
});

After({ tags: '@rewardsCsv' }, async function() {
  // Remove exported rewards csv
  const file = 'tests/delegation/e2e/documents/rewards_exported.csv';
  fs.unlink(file, err => {
    if (err) throw err;
  });
});

// eslint-disable-next-line prefer-arrow-callback
AfterAll(async function() {
  const allWindowsClosed = (await context.app.client.getWindowCount()) === 0;
  if (allWindowsClosed || !context.app.running) return;
  if (scenariosCount === 0) {
    await printMainProcessLogs();
  }
  if (process.env.KEEP_APP_AFTER_TESTS === 'true') {
    return;
  }
  return context.app.stop();
});
