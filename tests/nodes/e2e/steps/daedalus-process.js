// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import { refreshClient, waitForCardanoNodeToExit, waitForDaedalusToExit } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;
const CONNECTING_TITLE = '.SyncingConnectingTitle_connecting h1';

Given(/^Daedalus is running$/, function() {
  expect(this.app.isRunning()).to.equal(true);
});

// Given('im on the syncing screen', async function() {
//   this.client.executeAsync(done => {
//     // Simulate that syncing is necessary
//     const adaApi = daedalus.api.ada;
//     adaApi.setNetworkBlockHeight(10);
//     adaApi.setLocalBlockHeight(1);
//     daedalus.stores.networkStatus._updateNetworkStatus().then(done);
//   });
//   await this.client.waitForVisible('.SyncingConnecting_is-syncing');
// });

// Given('im on the connecting screen', async function() {
//   this.client.executeAsync(done => {
//     // Simulate that there is no connection to cardano node
//     daedalus.api.ada.setSubscriptionStatus({});
//     daedalus.stores.networkStatus._updateNetworkStatus().then(done);
//   });
//   await this.client.waitForVisible('.SyncingConnecting_is-connecting');
// });

When(/^I refresh the main window$/, async function() {
  await refreshClient(this.client);
});

When(/^I close the main window$/, async function() {
  await this.client.execute(() => daedalus.stores.window.closeWindow());
});

Then(/^Daedalus process is not running$/, async function() {
  await waitForDaedalusToExit(this.client);
});

Then(/^Daedalus should quit$/, { timeout: 70000 }, async function() {
  await waitForCardanoNodeToExit(this.client);
  await waitForDaedalusToExit(this.client);
});

Then(/^I should see the loading screen with "([^"]*)"$/, async function(
  message
) {
  await waitUntilTextInSelector(this.client, {
    selector: CONNECTING_TITLE,
    text: message,
  });
});

Then(/^I should see the main ui/, function() {
  return this.client.waitForVisible('.SidebarLayout_component');
});
