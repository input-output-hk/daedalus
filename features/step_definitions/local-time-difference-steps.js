export default function () {
  // TODO - create fake API response
  // this.When(/^I set wrong local time$/, async function () {
  //   await this.client.execute(localTimeDifference => {
  //     daedalus.actions.networkStatus.setLocalTimeDifference.trigger({ localTimeDifference });
  //   }, 9999999);
  // });

  this.Then(/^I should see system time error overlay$/, function () {
    return this.client.waitForVisible('.SystemTimeErrorOverlay_component');
  });
}
