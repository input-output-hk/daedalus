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

Then(/^I should see the report issue notification displaying "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: REPORT_ISSUE_TEXT_H1,
    text
  });
});
