import { expect } from 'chai';

export default function () {
  this.Given(/^I am on the(.*) settings screen$/, async function (screen) {
    const cleanedScreenName = screen ? screen.trim() : '';
    const screenClass = cleanedScreenName != '' ? `.${cleanedScreenName}` : '';
    await this.navigateTo(`/settings/${cleanedScreenName}`);
    return this.client.waitForVisible(`.SettingsLayout_component ${screenClass}`);
  });
  this.Given(/^My current language is "([^"]*)"$/, function (locale) {
    return this.client.execute(function (loc) {
      require('mobx').runInAction(() => {
        daedalus.stores.user.active.profile.languageLocale = loc;
        daedalus.stores.app.currentLocale = loc;
      });
    }, locale);
  });
  this.Given(/^The value of name input field should be "([^"]*)"$/, async function (value) {
    const newValue = await this.client.getValue(`.ProfileSettings_nameAndEmail .InlineEditingInput_component .input_inputElement`);
    expect(newValue).to.equal(value);
  });
  this.When(/^I select "([^"]*)" from the language dropdown on the settings page$/, async function (language) {
    await this.client.click('.dropdown_dropdown.language .input_inputElement');
    return this.client.click(`//*[text()="${language}"]/ancestor::*[@class="dropdown_values"]`);
  });
  this.When(/^I click on the (.*) settings menu item$/, function (menuItem) {
    return this.client.click(`.SettingsMenuItem_component.${menuItem}`);
  });
  this.When(/^I click on the name input field on profile settings page$/, function () {
    return this.client.click(`.ProfileSettings_nameAndEmail .InlineEditingInput_component .input_inputElement`);
  });
  this.When(/^I enter "([^"]*)" into the name input field$/, function (newName) {
    const inputField = '.ProfileSettings_nameAndEmail .InlineEditingInput_component .input_inputElement';
    return this.client.setValue(inputField, newName);
  });
  this.Then(/^My current language should be "([^"]*)"$/, async function (locale) {
    const result = await this.client.executeAsync(function (loc, done) {
      setTimeout(function() { // Allow mobx to flush changes
        setTimeout(function () {
          const user = daedalus.stores.user.active;
          done((user.profile.languageLocale === loc) && (daedalus.stores.app.currentLocale === loc));
        });
      });
    }, locale);
    expect(result.value).to.be.true;
  });
  this.Then(/^I should(?: still)? be on the (.*) settings screen$/, async function (settingsScreen) {
    await this.client.waitForVisible(`.SettingsLayout_component .${settingsScreen}`);
  });
  this.Then(/^Name input field should become (.*)/, async function (enabledOrDisabled) {
    const reverse = enabledOrDisabled === 'enabled' ? false : true;
    await this.client.waitForEnabled(`.ProfileSettings_nameAndEmail .InlineEditingInput_component .input_inputElement`, null, reverse);
  });
}
