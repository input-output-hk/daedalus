import { Given, When, Then } from 'cucumber';

const IFRAME_ID = '#webWidget';

Given(/^I click on the support request button$/, async function () {
  this.waitAndClick('.SupportSettings_component button');
});

Then(/^The Support Window should open$/, { timeout: 40000 }, async function () {
  await this.client.waitUntil(async () => {
    const windows = await this.client.getTabIds();
    return windows.length > 1;
  });
  const windows = await this.client.getTabIds();
  return this.client.switchTab(windows[1]);
});

Then(/^The Zendesk form should appear$/, async function () {
  await this.client.waitForExist(IFRAME_ID);
  const iframe = await this.client.element(IFRAME_ID);
  return this.client.frame(iframe.value);
});

When(/^I click the cancel button$/, async function () {
  return this.waitAndClick('footer button');
});

Then(/^The window should close$/, async function () {
  return await this.client.waitUntil(async () => {
    const windows = await this.client.getTabIds();
    return windows.length === 1;
  });
});
