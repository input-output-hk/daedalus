// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../support/global-types';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';
import { waitForCardanoNodeToExit } from '../support/helpers/cardano-node-helpers';
import { waitForDaedalusToExit } from '../support/helpers/app-helpers';

declare var daedalus: Daedalus;

Given(/^Daedalus is running$/, function () {
  expect(this.app.isRunning()).to.be.true;
});

When(/^I refresh the main window$/, async function () {
  return this.client.refresh();
});

When(/^I close the main window$/, async function () {
  await this.client.execute(() => daedalus.stores.window.closeWindow());
});

Then(/^Daedalus process is not running$/, async function () {
  await waitForDaedalusToExit(this.client);
});

Then(/^Daedalus should quit$/, { timeout: 70000 }, async function () {
  await waitForCardanoNodeToExit(this.client);
  await waitForDaedalusToExit(this.client);
});

Then(/^I should see the loading screen with "([^"]*)"$/, async function (message) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Loading_connecting h1',
    text: message
  });
});
