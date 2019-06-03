import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { environment } from '../../source/main/environment';

Given(/^I set app in connecting state$/, async function () {
  await this.client.execute(() => daedalus.api.ada.unsubscribeNode());
});

When(/^I check for available app updates$/, async function () {
  await this.client.execute(() => daedalus.actions.nodeUpdate.getLatestAvailableAppVersion.trigger());
});

When(/^I get available app version greater than current$/, async function () {
  const availableAppVersion = await this.client.execute(() => (
    daedalus.stores.nodeUpdate.availableAppVersion
  ));
  expect(parseFloat(availableAppVersion.value)).to.be.gt(parseFloat(environment.version));
});

Then(/^I should see Manual Update overlay$/, function () {
  return this.client.waitForVisible('.ManualUpdateOverlay_content');
});
