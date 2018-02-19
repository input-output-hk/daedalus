import { Given, Then } from 'cucumber';

Given(/^I set wrong local time difference$/, async function () {
  await this.client.executeAsync(timeDifference => {
    return daedalus.api.ada.setLocalTimeDifference(timeDifference)
      .then(() => daedalus.stores.networkStatus._updateLocalTimeDifference());
  }, 1511823600000);
});

Then(/^I should see system time error overlay$/, function () {
  return this.client.waitForVisible('.SystemTimeErrorOverlay_component');
});
