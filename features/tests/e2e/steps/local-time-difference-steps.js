import { Given, Then } from 'cucumber';

Given(/^I set wrong local time difference$/, async function() {
  await this.client.executeAsync((timeDifference, done) => {
    daedalus.api.ada
      .setLocalTimeDifference(timeDifference)
      .then(() => daedalus.stores.networkStatus._updateDaedalusDiagnostics())
      .then(done)
      .catch(error => done(error));
  }, 1511823600000);
});

Then(/^I should see system time error overlay$/, function() {
  return this.client.waitForVisible('.SystemTimeErrorOverlay_component');
});
