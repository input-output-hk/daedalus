// @flow
import { When, Then } from 'cucumber';
import { waitUntilTextInSelector } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

When(
  /^I arbitrarily set the local block height to half the network block height$/,
  async function() {
    await this.client.executeAsync(done => {
      daedalus.api.ada
        .setLocalBlockHeight(150)
        .then(() => daedalus.api.ada.setNetworkBlockHeight(300))
        .then(done)
        .catch(error => done(error));
    });
  }
);

Then(/^I should see the syncing status with "([^"]*)"$/, async function(text) {
  await waitUntilTextInSelector(this.client, {
    selector: '.SyncingConnectingTitle_syncing h1',
    text,
  });
});
