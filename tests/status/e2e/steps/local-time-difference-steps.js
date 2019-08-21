import { Given, Then } from 'cucumber';
import { expectTextInSelector } from '../helpers/shared-helpers';

const selector = '.time-off';

Given('I set the local time difference to be {int} seconds', async function(
  seconds
) {
  const differenceTime = seconds * 1000000; // unit: microseconds
  await this.client.executeAsync((timeDifference, done) => {
    daedalus.api.ada
      .setLocalTimeDifference(timeDifference)
      .then(() => daedalus.stores.networkStatus._updateNetworkStatus())
      .then(done)
      .catch(error => done(error));
  }, differenceTime);
});

Then(/^the system time error overlay should be (hidden|visible)$/, function(
  state
) {
  const isVisible = state === 'visible';
  return this.client.waitForVisible(
    '.SystemTimeError_component',
    null,
    !isVisible
  );
});

Then('the system time difference should be {string}', async function(text) {
  await expectTextInSelector(this.client, { selector, text });
});
