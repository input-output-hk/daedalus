import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import packageJson from "../../../../package.json";

const SELECTORS = {
  CONTAINER: '.About_container',
  VERSION: '.About_daedalusVersion'
};
Given(/^I open the About dialog$/, async function () {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  this.client.execute(() => daedalus.actions.app.openAboutDialog.trigger());
});
When(/^I close the About dialog$/, function () {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  this.client.execute(() => daedalus.actions.app.closeAboutDialog.trigger());
});
Then(/^the About dialog is (hidden|visible)/, async function (state) {
  const isVisible = state === 'visible';
  return this.client.waitForVisible(SELECTORS.CONTAINER, null, !isVisible);
});
Then(/^the About dialog and package.json show the same Daedalus version/, async function () {
  const {
    version: packageJsonVersion
  } = packageJson;
  const aboutVersion = await this.waitAndGetText(SELECTORS.VERSION);
  expect(aboutVersion).to.equal(packageJsonVersion);
});