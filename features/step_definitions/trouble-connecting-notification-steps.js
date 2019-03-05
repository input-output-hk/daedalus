import { Then, When } from 'cucumber';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';

When(/^I set the node subscription status to subscribing$/, async function () {
  await this.client.executeAsync((subscriptionStatus, done) => {
    daedalus.api.ada.setSubscriptionStatus(subscriptionStatus)
      .then(done)
      .catch((error) => done(error));
  }, {});
});

Then(/^I should see the loading screen with report connecting issue text "([^"]*)"$/, async function (message) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Loading_reportIssueText',
    text: message
  });
});

// When(/^I set isConnected to false$/, () => {
//   daedalus.stores.networkStatus.isConnected = false;
// });
