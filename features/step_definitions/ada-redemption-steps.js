export default function () {
  this.Given(/^I am on the ada redemption screen$/, async function () {
    await this.navigateTo(`/ada-redemption`);
    return this.client.waitForVisible(`.AdaRedemptionForm_component`);
  });

  this.Then(/^I should(?: still)? be on the ada redemption screen$/, function () {
    return this.client.waitForVisible(`.AdaRedemptionForm_component`);
  });
}
