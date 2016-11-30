import { expect } from 'chai';

export default function () {
  this.Given(/^I am on the settings screen$/, async function () {
    await this.navigateTo('/settings');
    await this.client.waitForVisible('.Settings_component');
  });
  this.Then(/^I should(?: still)? be on the (.*) settings screen$/, async function (settingsScreen) {
    await this.client.waitForVisible(`.Settings_component .${settingsScreen}`);
  });
}
