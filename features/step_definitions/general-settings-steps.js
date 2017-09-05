import { expect } from 'chai';
import _ from 'lodash';
import {
  navigateTo,
  waitUntilUrlEquals,
} from '../support/helpers/route-helpers'

export default function () {

  this.Given(/^I am on the General Settings "([^"]*)" screen$/, async function (screen) {
    await navigateTo.call(this, `/settings/${screen}`);
  });

  this.When(/^I click on secondary menu (.*) item$/, async function (buttonName) {
    const buttonSelector = `.SettingsMenuItem_component.${_.camelCase(buttonName)}`;
    await this.client.waitForVisible(buttonSelector);
    await this.client.click(buttonSelector);
  });

  this.When(/^I select "Second" theme$/, async function () {
    const test = await this.client.element(".DisplaySettings_themesWrapper > button:nth-child(2)").click();
  });

  this.When(/^I toggle switch to disable send-logs$/, async function () {
    await this.client.waitForVisible('.SupportSettings_component .SimpleSwitch_switch');
    await this.client.click('.SupportSettings_component .SimpleSwitch_checked');
  });

  this.When(/^I open General Settings language selection dropdown$/, function () {
    return this.waitAndClick('.GeneralSettings_component .SimpleInput_input');
  });

  this.Then(/^I should see General Settings "([^"]*)" screen$/, async function (screenName) {
    return waitUntilUrlEquals.call(this, `/settings/${screenName}`);
  });

  this.Then(/^I should see Japanese language as selected$/, async function () {
    await this.client.waitForVisible('.GeneralSettings_component .SimpleInput_input');
    let selectedLanguageValue = await this.client.getValue('.GeneralSettings_component .SimpleInput_input');
    const expectedLanguage = await this.intl('global.language.japanese');
    expect(selectedLanguageValue).to.equal(expectedLanguage);
  });

  this.Then(/^I should see "Second" theme as selected$/, async function () {
    await this.client.waitForVisible(".DisplaySettings_themesWrapper button:nth-child(2).DisplaySettings_active");
  });

  this.Then(/^I should not see send-logs switch checked anymore$/, async function () {
    return this.client.waitForVisible('.SupportSettings_component .SimpleSwitch_checked', null, true);
  });

}
