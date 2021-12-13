import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { initialSettingsHelpers } from "./helpers";

const {
  ensureLanguageIsSelected
} = initialSettingsHelpers;
const INITIAL_SETTINGS_FORM = '.InitialSettings_component';
Given(/^I have selected English language$/, async function () {
  await ensureLanguageIsSelected(this.client, {
    language: 'en-US'
  });
});
Given(/^I dont have a language set$/, async function () {
  await this.client.execute(() => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.reset();
  });
});
When(/^I am on the language selection screen$/, function () {
  return this.client.waitForVisible('.InitialSettings_component');
});
When(/^I open language selection dropdown$/, function () {
  return this.waitAndClick('.InitialSettings_component .SimpleInput_input');
});
When(/^I select Japanese language$/, function () {
  return this.waitAndClick('//*[@class="SimpleOptions_option OptionsOverrides_option"]//*[contains(text(), "Japanese")]');
});
When(/^I submit the language selection form$/, function () {
  return this.waitAndClick('.ProfileSettingsForm_submitButton');
});
Then(/^I should not see the language selection screen anymore$/, function () {
  return this.client.waitForVisible(INITIAL_SETTINGS_FORM, null, true);
});
Then(/^I should have Japanese language set$/, async function () {
  const result = await this.client.executeAsync(done => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.profile.getProfileLocaleRequest.execute().then(done).catch(error => done(error));
  });
  expect(result.value).to.equal('ja-JP');
});