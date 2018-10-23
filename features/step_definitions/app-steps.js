// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import type { Daedalus } from '../support/global-types';
import { getProcessesByName } from '../../source/main/utils/processes';

declare var daedalus: Daedalus;

Given(/^Daedalus is running$/, function () {
  expect(this.app.isRunning()).to.be.true;
});

When(/^I refresh the main window$/, async function () {
  return this.client.refresh();
});

When(/^I close the main window$/, async function () {
  await this.client.execute(() => daedalus.stores.window.closeWindow());
  await this.client.waitUntil(async () => (await this.client.getWindowCount()) === 0);
});

Then(/^Daedalus process is not running$/, async function () {
  return await this.client.waitUntil(async () => (
    (await getProcessesByName('Electron')).length === 0
  ));
});
