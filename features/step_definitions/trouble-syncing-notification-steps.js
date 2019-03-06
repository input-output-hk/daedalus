import { When, Then } from 'cucumber';
import { waitUntilTextInSelector } from '../support/helpers/shared-helpers';

When(/^I purposely unsync the node from the network$/, async function () {
  await this.client.executeAsync((done) => {
    daedalus.api.ada.setLocalBlockHeight(150)
      .then(() => daedalus.api.ada.setNetworkBlockHeight(300))
      .then(done)
      .catch((error) => done(error));
  });
});

Then(/^I should see the syncing status with "([^"]*)"$/, async function (text) {
  await waitUntilTextInSelector(this.client, {
    selector: '.Loading_syncing h1',
    text
  });
});
