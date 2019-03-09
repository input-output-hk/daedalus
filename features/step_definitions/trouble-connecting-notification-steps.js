import { Then, When } from 'cucumber';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';

const REPORT_ISSUE_TEXT_H1 = '.Loading_reportIssueText';

When(/^I set the node subscription status to subscribing$/, async function () {
  await this.client.executeAsync((subscriptionStatus, done) => {
    daedalus.api.ada.setSubscriptionStatus(subscriptionStatus)
      .then(done)
      .catch((error) => done(error));
  }, {});
});

When(/^I set the node subscription status to subscribed$/, async function () {
  await this.client.executeAsync((subscriptionStatus, done) => {
    daedalus.api.ada.setSubscriptionStatus(subscriptionStatus)
      .then(done)
      .catch((error) => done(error));
  }, { status: 'subscribed' });
});

Then(/^I should not see loading screen anymore$/, async function () {
  await this.client.waitForVisible('Loading_component', null, true);
});

When(/^I purposely set the network and local block heights to a static and equal number$/, async function () {
  await this.client.executeAsync((done) => {
    daedalus.api.ada.setNetworkBlockHeight(150)
      .then(() => daedalus.api.ada.setLocalBlockHeight(150))
      .then(done)
      .catch((error) => done(error));
  });
});

Then(/^I should see the report issue notification displaying "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: REPORT_ISSUE_TEXT_H1,
    text
  });
});
