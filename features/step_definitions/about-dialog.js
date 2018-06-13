import { Given, When, Then } from 'cucumber';
import packageJson from '../../package.json';
import { expect } from 'chai';

Given(/^I open the About dialog$/, async function () {
  this.client.execute(() => {
    daedalus.stores.app._openAboutDialog();
  });
  await this.client.waitForExist('.About_container');
});

When(/^I close the About dialog$/, function () {
  this.client.execute(() => {
    daedalus.stores.app._closeAboutDialog();
  });
});

Then(/^the About dialog is (hidden|visible)/, async function (state) {
  const isVisible = state === 'visible';
  return this.client.waitForExist('.About_container', null, !isVisible);
});

Then(/^the About dialog and package.json have the same version/, async function () {
  const { version:packageJsonVersion } = packageJson;
  const aboutVersion = await this.client.getText('.About_daedalusVersion');
  expect(aboutVersion).to.equal(packageJsonVersion);
})
