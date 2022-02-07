import { When, Then } from "cucumber";
import { expect } from "chai";
import { camelCase } from "lodash";

When(/^I click on secondary menu (.*) item$/, async function (buttonName) {
  const buttonSelector = `.SettingsMenuItem_component.${camelCase(buttonName)}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});
When(/^I select second theme$/, async function () {
  await this.client.click('.DisplaySettings_component button:nth-child(2)');
});
When(/^I open General Settings language selection dropdown$/, async function () {
  await this.client.click('.ProfileSettingsForm_component input:nth-child(1)');
});
Then(/^I should see Japanese language as selected$/, async function () {
  const [selectedLanguage] = await this.client.waitUntil(async () => {
    return await this.client.getValue('.ProfileSettingsForm_component .SimpleInput_input');
  });
  const locale = await this.intl('global.language.japanese');
  expect(selectedLanguage).to.equal(locale);
});
Then(/^I should see second theme as selected$/, async function () {
  await this.client.waitForVisible('.DisplaySettings_component button:nth-child(2).DisplaySettings_active');
});
Then(/^I should see the page with Frequency asked questions title$/, async function () {
  return this.client.waitForVisible(await this.intl('settings.support.faq.title'), null, true);
});