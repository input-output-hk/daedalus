// @flow
import { Then, When } from 'cucumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

const SELECTORS = {
  REPORT_ISSUE_BTN: '.ReportIssue_actionButton.reportIssueButton',
  REPORT_ISSUE_HEADER: '.ReportIssue_reportIssueText',
  SYNCING_CONNECTING_COMPONENT: '.SyncingConnecting_component',
};

Then(/^I should not see the loading screen$/, async function() {
  await this.client.waitForVisible(
    SELECTORS.SYNCING_CONNECTING_COMPONENT,
    null,
    true
  );
});

Then(
  /^I should see the report issue notification displaying "([^"]*)"$/,
  async function(text) {
    await waitUntilTextInSelector(this.client, {
      selector: SELECTORS.REPORT_ISSUE_HEADER,
      text,
    });
  }
);

Then(/^I should not see the report issue notification$/, async function() {
  await this.client.waitForVisible(SELECTORS.REPORT_ISSUE_HEADER, null, true);
});

Then(/^The report issue button should be (hidden|visible)$/, async function(
  state
) {
  const waitForHidden = state === 'hidden';
  await this.client.waitForVisible(
    SELECTORS.REPORT_ISSUE_BTN,
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
        .then(() => daedalus.stores.networkStatus._updateNetworkStatus())
        .then(done)
        .catch(error => done(error));
    });
  }
);
