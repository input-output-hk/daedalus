export default function () {
  this.Given(/^I set wrong local time difference$/, async function () {
    await this.client.execute(timeDifference => {
      daedalus.api.setLocalTimeDifference(timeDifference);
    }, 1511823600000);
  });

  this.Then(/^I should see system time error overlay$/, function () {
    return this.client.waitForVisible('.SystemTimeErrorOverlay_component');
  });
}
