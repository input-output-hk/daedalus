  import { Given, Then } from 'cucumber';

  Given(/^I set wrong local time difference$/, async function () {
    await this.client.execute(timeDifference => {
      daedalus.api.ada.setLocalTimeDifference(timeDifference);
    }, 1511823600000);
  });

  Then(/^I should see system time error overlay$/, function () {
    return this.client.waitForVisible('.SystemTimeErrorOverlay_component');
  });
