import { Given, When, Then } from 'cucumber';
import _ from 'lodash';
import {
  navigateTo,
  waitUntilUrlEquals,
} from '../support/helpers/route-helpers';

Given(/^I am on the General Settings "([^"]*)" screen$/, async function (screen) {
  await navigateTo.call(this, `/settings/${screen}`);
});

When(/^I click on secondary menu (.*) item$/, async function (buttonName) {
  const buttonSelector = `.SettingsMenuItem_component.${_.camelCase(buttonName)}`;
  await this.client.waitForVisible(buttonSelector);
  await this.client.click(buttonSelector);
});

When(/^I select second theme$/, async function () {
  await this.client.click('.DisplaySettings_themesWrapper > button:nth-child(2)');
});

When(/^I toggle switch to disable send-logs$/, async function () {
  await this.client.waitForVisible('.SupportSettings_component .SimpleSwitch_switch');
  await this.client.click('.SupportSettings_component .SimpleSwitch_checked');
});

When(/^I open General Settings language selection dropdown$/, async function () {
  await this.client.click('.GeneralSettings_component .SimpleInput_input');
});

Then(/^I should see General Settings "([^"]*)" screen$/, async function (screenName) {
  return waitUntilUrlEquals.call(this, `/settings/${screenName}`);
});

Then(/^I should see Japanese language as selected$/, async function () {
  return this.client.waitUntil(async () => {
    const selectedLanguage = await this.client.getValue('.GeneralSettings_component .SimpleInput_input');
    const expectedLanguage = await this.intl('global.language.japanese');
    return selectedLanguage === expectedLanguage;
  });
});

Then(/^I should see second theme as selected$/, async function () {
  await this.client.waitForVisible('.DisplaySettings_themesWrapper button:nth-child(2).DisplaySettings_active');
});

Then(/^I should not see send-logs switch checked anymore$/, async function () {
  return this.client.waitForVisible('.SupportSettings_component .SimpleSwitch_checked', null, true);
});
