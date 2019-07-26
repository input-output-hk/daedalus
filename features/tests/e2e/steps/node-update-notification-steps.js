import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { environment } from '../../../../source/main/environment';
import { getVisibleTextsForSelector } from '../helpers/shared-helpers';

const currentAppVersion = environment.version;
const currentAppVersionChunks = currentAppVersion.split('.');
const nextAppVersion = [
  currentAppVersionChunks[0],
  parseInt(currentAppVersionChunks[1], 10) + 1,
  currentAppVersionChunks[2],
].join('.');

const SELECTORS = {
  currentAppVersionInfo:
    '.AutomaticUpdate_content .AutomaticUpdate_description p span b:nth-child(1)',
  newAppVersionInfo:
    '.AutomaticUpdate_content .AutomaticUpdate_description p span b:nth-child(2)',
};

When(/^I make newer application version available$/, async function() {
  await this.client.execute(version => {
    daedalus.api.ada.setLatestAppVersion(version);
  }, nextAppVersion);
});

Then('I should see the node update notification overlay', async function () {
   return this.client.waitForVisible('.AutomaticUpdate_overlay');
 });

Then(
  /^Overlay should display the versions info$/,
  async function() {
    const [newAppVersionInfo] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.newAppVersionInfo
    );

    const [currentAppVersionInfo] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.currentAppVersionInfo
    );

    expect(newAppVersionInfo).to.equal(nextAppVersion);
    expect(currentAppVersionInfo).to.equal(currentAppVersion);
  }
);

Then('I should see the accept update button', async function () {
   return this.client.waitForVisible('.AutomaticUpdate_acceptButton');
 });

Then('I should see the postpone update button', async function() {
  return this.client.waitForVisible('.AutomaticUpdate_postponeButton');
});

When(/^I click the postpone update button$/, function() {
  return this.waitAndClick('.AutomaticUpdate_postponeButton');
});

When(/^I click the close notification button$/, function() {
  return this.waitAndClick('.AutomaticUpdate_closeButton');
});

When(/^I click the accept update button$/, function() {
  return this.waitAndClick('.AutomaticUpdate_acceptButton');
});

Then(/^I should not see the notification component anymore$/, function() {
  return this.client.waitForVisible(
    '.AutomaticUpdate_overlay',
    null,
    true
  );
});
