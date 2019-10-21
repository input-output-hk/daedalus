// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../setup/global-types';
import { waitUntilTextInSelector } from '../helpers/shared-helpers';
import { waitForCardanoNodeToExit } from '../helpers/cardano-node-helpers';
import { refreshClient, waitForDaedalusToExit } from '../helpers/app-helpers';

declare var daedalus: Daedalus;

Given(/^Daedalus is running$/, function() {
  expect(this.app.isRunning()).to.equal(true);
});

Given('im on the syncing screen', async function() {
  this.client.executeAsync(done => {
    // Simulate that syncing is necessary
    daedalus.api.ada.setSyncProgress(10);
    daedalus.stores.networkStatus._updateNetworkStatus().then(done);
  });
  await this.client.waitForVisible('.SyncingConnecting_is-syncing');
});

Given('im on the connecting screen', async function() {
  this.client.executeAsync(done => {
    // Simulate that there is no connection to cardano node
    daedalus.api.ada.setSubscriptionStatus({});
    daedalus.stores.networkStatus._updateNetworkStatus().then(done);
  });
  await this.client.waitForVisible('.SyncingConnecting_is-connecting');
});

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
    selector: '.SyncingConnectingTitle_connecting h1',
    text: message,
  });
});

Then(/^I should see the main ui/, function() {
  return this.client.waitForVisible('.SidebarLayout_component');
});
