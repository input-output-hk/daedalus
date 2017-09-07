import { expect } from 'chai';

const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';

export default function () {
  this.Given(/^I have selected English language$/, async () => {
    await this.client.waitForVisible(LANGUAGE_SELECTION_FORM);
    await this.client.execute(locale => {
      daedalus.actions.profile.updateLocale.trigger({ locale });
    }, 'en-US');
    await this.client.waitForVisible(LANGUAGE_SELECTION_FORM, null, true);
  });

  this.Given(/^I dont have a language set$/, async () => {
    await this.client.execute(() => {
      daedalus.reset();
    });
  });

  this.When(/^I am on the language selection screen$/, () => (
    this.client.waitForVisible('.LanguageSelectionForm_component')
  ));

  this.When(/^I open language selection dropdown$/, () => (
    this.waitAndClick('.LanguageSelectionForm_component .SimpleInput_input')
  ));

  this.When(/^I select Japanese language$/, () => (
    this.waitAndClick('//li[contains(text(), "Japanese")]')
  ));

  this.When(/^I submit the language selection form$/, () => (
    this.waitAndClick('.LanguageSelectionForm_submitButton')
  ));

  this.Then(/^I should not see the language selection screen anymore$/, () => (
    this.client.waitForVisible(LANGUAGE_SELECTION_FORM, null, true)
  ));

  this.Then(/^I should have Japanese language set$/, async () => {
    const result = await this.client.executeAsync((done) => {
      daedalus.stores.app.getProfileLocaleRequest.execute()
        .then(done)
        .catch((error) => done(error));
    });
    expect(result.value).to.equal('ja-JP');
  });
}
