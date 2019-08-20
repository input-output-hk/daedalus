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
    '.AutomaticUpdateNotification_description p span b:nth-child(1)',
  newAppVersionInfo:
    '.AutomaticUpdateNotification_description p span b:nth-child(2)',
};

Then('I should see the node update notification overlay', async function() {
  return this.client.waitForVisible('.AutomaticUpdateNotification_dialog');
});

When(
  /^Overlay should display "([^"]*)" as available version and actions$/,
  async function(nextVersion) {
    const [newAppVersionInfo] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.newAppVersionInfo
    );

    const [currentAppVersionInfo] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.currentAppVersionInfo
    );

    expect(newAppVersionInfo.replace('v ', '')).to.equal(nextVersion);
    expect(currentAppVersionInfo.replace('v ', '')).to.equal(currentAppVersion);
    this.client.waitForVisible('.AutomaticUpdateNotification_acceptButton');
    this.client.waitForVisible('.AutomaticUpdateNotification_postponeButton');
  }
);

Then('I should see the accept update button', async function() {
  return this.client.waitForVisible(
    '.AutomaticUpdateNotification_acceptButton'
  );
});

Then('I should see the postpone update button', async function() {
  return this.client.waitForVisible(
    '.AutomaticUpdateNotification_postponeButton'
  );
});

When(/^I click the postpone update button$/, function() {
  return this.waitAndClick('.AutomaticUpdateNotification_postponeButton');
});

When(/^I click the accept update button$/, function() {
  return this.waitAndClick('.AutomaticUpdateNotification_acceptButton');
});

Then(/^I should not see the notification component anymore$/, function() {
  return this.client.waitForVisible(
    '.AutomaticUpdateNotification_overlay',
    null,
    true
  );
});
