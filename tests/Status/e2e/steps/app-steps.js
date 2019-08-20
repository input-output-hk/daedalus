// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { waitUntilTextInSelector } from '../helpers/shared-helpers';
import { refreshClient, waitForCardanoNodeToExit, waitForDaedalusToExit } from '../helpers';
import type { Daedalus } from '../../types';

declare var daedalus: Daedalus;

Given(/^Daedalus is running$/, function() {
  expect(this.app.isRunning()).to.equal(true);
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
