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
      daedalus.stores.user.active.profile.languageLocale = loc;
      daedalus.stores.app.currentLocale = loc;
    }, locale);
  });
  this.When(/^I select "([^"]*)" from the language dropdown on the settings page$/, async function (language) {
    await this.client.click('.dropdown_dropdown.language .input_inputElement');
    return this.client.click(`//*[text()="${language}"]/ancestor::*[@class="dropdown_values"]`);
  });
  this.When(/^I click on the (.*) settings menu item$/, function (menuItem) {
    return this.client.click(`.SettingsMenuItem_component.${menuItem}`);
  });
  this.Then(/^My current language should be "([^"]*)"$/, async function (locale) {
    const result = await this.client.executeAsync(function (loc, done) {
      setTimeout(function() { // Allow mobx to flush changes
        const user = daedalus.stores.user.active;
        done((user.profile.languageLocale === loc) && (daedalus.stores.app.currentLocale === loc));
      }, 0);
    }, locale);
    expect(result.value).to.be.true;
  });
  this.Then(/^I should(?: still)? be on the (.*) settings screen$/, async function (settingsScreen) {
    await this.client.waitForVisible(`.SettingsLayout_component .${settingsScreen}`);
  });
}
