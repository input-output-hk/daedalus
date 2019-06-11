import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { environment } from '../../source/main/environment';
import { getVisibleTextsForSelector } from '../support/helpers/shared-helpers';
import i18n from '../support/helpers/i18n-helpers';

const SELECTORS = {
  MANUAL_UPDATE_VERSION_INFO:
    '.ManualUpdateOverlay_content .ManualUpdateOverlay_description p:nth-child(2)',
};

const currentAppVersion = environment.version;
const currentAppVersionChunks = currentAppVersion.split('.');
const nextAppVersion = [
  currentAppVersionChunks[0],
  parseInt(currentAppVersionChunks[1], 10) + 1,
  currentAppVersionChunks[2],
].join('.');

Given(/^There is a newer application version available$/, async function () {
  await this.client.execute(version => {
    daedalus.api.ada.setLatestAppVersion(version);
  }, nextAppVersion);
});

When(/^Daedalus is stuck in connecting state$/, async function () {
  await this.client.execute(() => daedalus.api.ada.setSubscriptionStatus({}));
});

Then(/^I should see the "Manual Update" overlay$/, function () {
  return this.client.waitForVisible('.ManualUpdateOverlay_content');
});

Then(
  /^The overlay should accurately display the version info$/,
  async function () {
    const [renderedText] = await getVisibleTextsForSelector(
      this.client,
      SELECTORS.MANUAL_UPDATE_VERSION_INFO
    );

    let expectedText = await i18n.formatMessage(this.client, {
      id: 'manualUpdateOverlay.description2',
      values: {
        currentAppVersion,
        availableAppVersion: nextAppVersion,
      },
    });
    // Expected text contains HTML tags so we need to strip them before running comparison
    expectedText = expectedText.replace(/<[^>]*>?/gm, '');

    expect(renderedText).to.equal(expectedText);
  }
);

Then(
  /^I set the node subscription status to (subscribing|subscribed)$/,
  async function (state) {
    const subscriptionState =
      state === 'subscribed' ? { status: 'subscribed' } : {};
    await this.client.executeAsync((subscriptionStatus, done) => {
      daedalus.api.ada
        .setSubscriptionStatus(subscriptionStatus)
        .then(done)
        .catch(error => done(error));
    }, subscriptionState);
  }
);
