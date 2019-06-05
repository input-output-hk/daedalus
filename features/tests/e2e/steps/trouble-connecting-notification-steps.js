import { Then, When } from 'cucumber';
import { waitUntilTextInSelector } from '../helpers/shared-helpers';

const SELECTORS = {
  LOADING_COMPONENT: '.Loading_component',
  REPORT_ISSUE_TEXT_H1: '.Loading_reportIssueText',
  REPORT_ISSUE_BUTTON: '.Loading_reportIssueButton',
};

Then(/^I should not see the loading screen$/, async function() {
  await this.client.waitForVisible(SELECTORS.LOADING_COMPONENT, null, true);
});

Then(
  /^I should see the report issue notification displaying "([^"]*)"$/,
  async function(text) {
    await waitUntilTextInSelector(this.client, {
      selector: SELECTORS.REPORT_ISSUE_TEXT_H1,
      text,
    });
  }
);

Then(/^I should not see the report issue notification$/, async function() {
  await this.client.waitForVisible(SELECTORS.REPORT_ISSUE_TEXT_H1, null, true);
});

Then(/^The report issue button should be (hidden|visible)$/, async function(
  state
) {
  const waitForHidden = state === 'hidden';
  await this.client.waitForVisible(
    SELECTORS.REPORT_ISSUE_BUTTON,
    null,
    waitForHidden
  );
});

When(
  /^I set both local and network block heights to a static, equal number$/,
  async function() {
    await this.client.executeAsync(done => {
      daedalus.api.ada
        .setNetworkBlockHeight(150)
        .then(() => daedalus.api.ada.setLocalBlockHeight(150))
        .then(done)
        .catch(error => done(error));
    });
  }
);

When(
  /^I set the node subscription status to (subscribing|subscribed)$/,
  async function(state) {
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

When(
  /^I reconnect local and network block heights to the node$/,
  async function() {
    await this.client.executeAsync(done => {
      daedalus.api.ada
        .setNetworkBlockHeight(null)
        .then(() => daedalus.api.ada.setLocalBlockHeight(null))
        .then(() => daedalus.stores.networkStatus._updateDaedalusDiagnostics())
        .then(done)
        .catch(error => done(error));
    });
  }
);
