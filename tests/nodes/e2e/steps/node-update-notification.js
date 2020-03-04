import { When, Then } from 'cucumber';
import { expect } from 'chai';
import { environment } from '../../../../source/main/environment';
import { getVisibleTextsForSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const currentAppVersion = environment.version;

const SELECTORS = {
  currentAppVersionInfo:
    '.AutomaticUpdateNotification_description p span b:nth-child(1)',
  newAppVersionInfo:
    '.AutomaticUpdateNotification_description p span b:nth-child(2)',
  acceptButton: '.AutomaticUpdateNotification_acceptButton',
  postponeButton: '.AutomaticUpdateNotification_postponeButton',
  nodeUpdateOverlay: '.AutomaticUpdateNotification_dialog',
  nodeUpdateComponent: '.AutomaticUpdateNotification_overlay',
};

Then('I should see the node update notification overlay', async function() {
  return this.waitForVisible(SELECTORS.nodeUpdateOverlay);
});

When(/^I set next update version to "([^"]*)"$/, async function(applicationVersion) {
  await this.client.executeAsync((applicationVersion, done) => {
    daedalus.api.ada
      .setNextUpdate(parseInt(applicationVersion))
      .then(done)
      .catch(e => {
        throw e;
      });
  }, applicationVersion);
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
    this.waitForVisible(SELECTORS.acceptButton);
    this.waitForVisible(SELECTORS.postponeButton);
  }
);

When(/^I set next application version to "([^"]*)"$/, async function(
  applicationVersion
) {
  await this.client.execute(version => {
    daedalus.api.ada.setApplicationVersion(parseInt(version, 10));
  }, applicationVersion);
});

When(/^I click the postpone update button$/, function() {
  return this.waitAndClick(SELECTORS.postponeButton);
});

When(/^I click the accept update button$/, function() {
  return this.waitAndClick(SELECTORS.acceptButton);
});

Then(/^I should not see the notification component anymore$/, function() {
  return this.waitForVisible(
    SELECTORS.nodeUpdateComponent,
    null,
    true
  );
});
