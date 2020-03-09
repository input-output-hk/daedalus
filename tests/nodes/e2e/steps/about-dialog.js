// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import packageJson from '../../../../package.json';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;
const SELECTORS = {
  CONTAINER: '.About_container',
  VERSION: '.About_daedalusVersion',
};

Given(/^I open the About dialog$/, async function() {
  this.client.execute(() => daedalus.actions.app.openAboutDialog.trigger());
});

When(/^I close the About dialog$/, function() {
  this.client.execute(() => daedalus.actions.app.closeAboutDialog.trigger());
});

Then(/^the About dialog is (hidden|visible)/, async function(state) {
  const isVisible = state === 'visible';
  return this.client.waitForVisible(SELECTORS.CONTAINER, null, !isVisible);
});

Then(
  /^the About dialog and package.json show the same Daedalus version/,
  async function() {
    const { version: packageJsonVersion } = packageJson;
    const aboutVersion = await this.waitAndGetText.call(this, SELECTORS.VERSION);
    expect(aboutVersion).to.equal(packageJsonVersion);
  }
);
